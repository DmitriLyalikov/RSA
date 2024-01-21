package sha3

import Chisel._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.random
import Chisel.ImplicitConversions._
import scala.collection.mutable.HashMap
import freechips.rocketchip.tile.HasCoreParameters
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import org.chipsalliance.cde.config._

class CtrlModule(val W: Int, val S: Int)(implicit val p: Parameters) extends CtrlModule
    with HasCoreParameters
    with MemoryOpConstants {
    val r = 2*256
    val c = 25*W - r 
    val round_size_words = c/W
    val rounds = 24 
    val hash_size_words = 256/W
    val bytes_per_word = W/8

    val io = new Bundle {
        val rocc_req_val      = Bool(INPUT)
        val rocc_req_rdy      = Bool(OUTPUT)
        val rocc_funct        = Bits(INPUT, 2)
        val rocc_rs1          = Bits(INPUT, 64)
        val rocc_rs2          = Bits(INPUT, 64)
        val rocc_rd           = Bits(INPUT, 5)

        val busy              = Bool(OUTPUT)

        val dmem_req_val      = Bool(OUTPUT)
        val dmem_req_rdy      = Bool(INPUT)
        val dmem_req_tag      = Bits(OUTPUT, coreParams.dcacheReqTagBits)
        val dmem_req_addr     = Bits(OUTPUT, coreMaxAddrBits)
        val dmem_req_cmd      = Bits(OUTPUT, M_SZ)
        val dmem_req_size     = Bits(OUTPUT, log2Ceil(coreDataBytes + 1))

        val dmem_resp_val     = Bool(INPUT)
        val dmem_resp_tag     = Bits(INPUT, 7)
        val dmem_resp_data    = Bits(INPUT, W)

        val sfence            = Bool(OUTPUT)

        //Sha3 Specific signals
        val round       = UInt(OUTPUT,width=5)
        val stage       = UInt(OUTPUT,width=log2Up(S))
        val absorb      = Bool(OUTPUT)
        val aindex      = UInt(OUTPUT,width=log2Up(round_size_words))
        val init        = Bool(OUTPUT)
        val write       = Bool(OUTPUT)
        val windex      = UInt(OUTPUT,width=log2Up(hash_size_words+1))

        val buffer_out  = Bits(OUTPUT,width=W)
    }


    // ROCC Handler
    // rocc pipe state
    val r_idle :: r_eat_addr :: r_eat_len :: Nil = Enum(UInt(), 3) // ??

    val msg_addr  = Reg(init = UInt(0, 64)) // 64 bit msg register with reset value 0
    val hash_addr = Reg(init = UInt(0, 64))
    val msg_len   = Reg(init = UInt(0, 64))

    val busy = Reg(init=Bool(false))

    val rocc_s = Reg(init=r_idle)  // ??

    val rocc_req_val_reg = Reg(next=io.rocc_req_val)
    val rocc_funct_reg   = Reg(init = Bits(0, 2))
    rocc_funct_reg := io.rocc_funct
    val rocc_rs1_reg = Reg(next=io.rocc_rs1)
    val rocc_rs2_reg = Reg(next=io.rocc_rs2)
    val rocc_rd_reg  = Reg(next=io.rocc_rd)

    val dmem_resp_val_reg = Reg(next=io.dmem_resp_val)
    val dmem_resp_tag_reg = Reg(next=io.dmem_resp_tag)
    // memory pipe state
    val fast_mem = p(Sha3FastMem)
    val m_idle :: m_read :: m_wait :: m_pad :: m_absorb :: Nil = Enum(UInt(), 5)
    val mem_s = Reg(init=m_idle)

    val buffer_sram = p(Sha3BUfferSram)
    // SRAM Buffer
    val buffer_mem = Mem(round_size_words, UInt(width = W))
    // Flip-Flop buffer
    val buffer = Reg(init=Vec.fill(round_size_words) {0.U(W.W) })    

    val buffer_raddr = Reg(UInt(width = log2Up(round_size_words)))
    val buffer_wen = Wire(Bool())
    buffer_wen := Bool(false) // Default value
    val buffer_waddr = Wire(UInt(width = W)); buffer_waddr := UInt(0)
    val buffer_wdata = Wire(UInt(width = W)); buffer_wdata := UInt(0)
    val buffer_rdata = Bits(width = W);
    if (buffer_sram) {
        when(buffer_wen) { buffer_mem.write(buffer_waddr, buffer_wdata) }
        buffer_rdata := buffer_mem(buffer_raddr)
    }

    // This is used to prevent the pad index from advancing if waiting for the sram to read
    // SRAM reads take 1 cycle
    val wait_for_sram = Reg(init = Bool(true))

    val buffer_valid = Reg(init = Bool(false))
    val buffer_count = Reg(init = UInt(0, 5))
    val read      = Reg(init = UInt(0, 32))
    val hashed    = Reg(init = UInt(0, 32))
    val areg      = Reg(init = Bool(false))
    val mindex    = Reg(init = UInt(0, 5))
    val windex    = Reg(init = UInt(0, log2Up(hash_size_words+1)))
    val aindex    = Reg(init = UInt(0, log2Up(round_size_words)))
    val pindex    = Reg(init = UInt(0, log2Up(round_size_words)))
    val writes_done  = Reg( init=Vec.fill(hash_size_words) { Bool(false) })
    val next_buff_val = Reg(init=Bool(false))
    if(fast_mem){
        next_buff_val := (buffer_count >= mindex) && 
                          (pindex >= UInt(round_size_words -1))
    } else {
        next_buff_val := ((mindex >= UInt(round_size_words)) || 
                        (read >= msg_len)) &&
                        (pindex >= UInt(round_size_words -1))
    }

    // Note that the output is delayed by 1 cycle
    io.aindex    := Reg(next = aindex)
    io.absorb    := areg 
    areg         := Bool(false)
    if(buffer_sram){
        buffer_raddr := aindex 

        when(mem_s === m_pad) {
            buffer_raddr := pindex
        }
        io.buffer_out := buffer_rdata
    } else {
        io.buffer_out := buffer(io.aindex)
    }
    io.windex    := windex

    // misc padding signals
    val first_pad  = Bits(if (p(Sha3Keccak)) "b0000_0001" else "b0000_0110")
    val last_pad   = Bits("b1000_0000")
    val both_pad   = first_pad | last_pad

    val words_filled = 
        Mux(mindex > UInt(0), mindex - UInt(1), mindex)

    val byte_offset = (msg_len)%UInt(bytes_per_word)

    val s_idle :: s_absorb :: s_finish_abs :: s_hash :: s_write :: Nil = Enum(UInt(), 5)

    val state = Reg(init = s_idle)

    val rindex = Reg(init = UInt(roundes+1, 5))
    val sindex = Reg(init = UInt(0, log2Up(S)))

    // default 
    io.rocc_req_rdy := Bool(false)
    io.init  := Bool(false)
    io.busy  := busy 
    io.round := rindex 
    io.stage := sindex 
    io.write := Bool(true)
    io.dmem_req_val := Bool(false)
    io.dmem_req_tag := rindex 
    io.dmem_reg_addr := Bits(0, 32)
    io.dmem_req_cmd := M_XRD 
    io.dmem_req_size := log2Ceil(8).U
    io.sfence := Bool(false)

    val rindex_reg = Reg(next=rindex)

    switch(rocc_s) {
    is(r_idle) {
        io.rocc_req_rdy := !busy
        when(io.rocc_req_val && !busy){
            when(io.rocc_funct == UInt(0)) {
                io.rocc_req_rdy := Bool(true)
                msg_addr := io.rocc_rs1 
                hash_addr := io.rocc_rs2 
                println("Msg Addr: "+msg_addr+", Hash Addr: "+hash_addr)
                io.busy := Bool(true)
            }.elsewhen(io.rocc_funct === UInt(1)) {
                busy := Bool(true)
                io.rocc_req_rdy := Bool(true)
                io.busy := Bool(true)
                msg_len := io.rocc_rs1
            }
            if (p(Sha3TLB).isDefined) {
                when (io.rocc_funct === UInt(2)) {
                    io.rocc_req_rdy := Bool(true)
                    io.sfence := Bool(true)
                }
            }
        }
      }
    }

    // End RoCC Handler
    // Start Mem Handler

    switch(mem_s){
        is(mem_idle) {
            // we can start filling the buffer if we are not writing and if we got a new message
            // or the hashing started
            // and there is more to read
            // and the buffer has been absorbed
            val canRead = busy && ( ( read < msg_len || (read === msg_len && msg_len === UInt(0))) &&
                    (!buffer_valid && buffer_count === UInt(0)))
            when(canRead){
                buffer_count := UInt(0)
                mindex := UInt(0)
                mem_s := m_read
            }.otherwise{
                mem_s := m_idle
            }
        }
        is(m_read) {
            // dmem signals
            // only read if we are not writing
            when(state =/= s_write) {
                io.dmem_req_val := read < msg_len && mindex < UInt(round_size_words)
                io.dmem_req_addr := msg_addr 
                io.dmem_req_tag := mindex 
                io.dmem_req_cmd := M_XRD 
                io.dmem_req_size := log2Ceil(8).U

                when(io.dmem_req_rdy && io.dmem_req_val) {
                    mindex := mindex + UInt(8)
                    msg_addr := msg_addr + UInt(8)
                    read := read + UInt(8) // read 8 bytes 
                    if(!fast_mem) {
                        mem_s := m_wait
                    }
                }.otherwise{
                    if(!fast_mem){
                        mem_s := m_read
                    }
                }

                when(msg_len === UInt(0)){
                    read := UInt(1)
                    if(!fast_mem){
                        mem_s := m_pad 
                            pindex := words_filled
                    }
                }
            }
            if(fast_mem){
                // Next state
                when(mindex < UInt(round_size_words - 1)){
                    //TODO: in pad check buffer_count ( or move on to next thread?)
                    when(msg_len > read){
                    //not sure if this case will be used but this means we haven't
                    //sent all the requests yet (maybe back pressure causes this)
                    when((msg_len+UInt(8)) < read){
                        buffer_valid := Bool(false)
                        mem_s := m_pad
                        pindex := words_filled
                    }
                    mem_s := m_read
                    }.otherwise{
                    //its ok we didn't send them all because the message wasn't big enough
                    buffer_valid := Bool(false)
                    mem_s := m_pad
                        pindex := words_filled
                    }
                }.otherwise{
                    when(mindex < UInt(round_size_words) &&
                        !(io.dmem_req_rdy && io.dmem_req_val)){
                    //we are still waiting to send the last request
                    mem_s := m_read
                    }.otherwise{
                    //we have reached the end of this chunk
                    mindex := mindex + UInt(1)
                    msg_addr := msg_addr + UInt(8)
                    read := read + UInt(8)//read 8 bytes
                    //we sent all the requests
                    when((msg_len < (read+UInt(8) ))){
                        //but the buffer still isn't full
                        buffer_valid := Bool(false)
                        mem_s := m_pad
                        pindex := words_filled
                    }.otherwise{
                        //we have more to read eventually
                        mem_s := m_idle
                    }
                    }
                }
            }
        }
        is(m_wait){
            
        }
    }

}