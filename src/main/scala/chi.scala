// see LICENSE for license
package sha3

import Chisel._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class ChiModule(val W: Int = 64) extends Module {
    val io = new Bundle {
        val state_i = Vec(5*5, Bits(INPUT, W))
        val state_o = Vec(5*5, Bits(OUTPUT, W))
    }

    for(i <- 0 until 5){
        for(j <- 0 until 5){
            io.state_o(i*5+j) := io.state_i(i*5+j) ^ 
            ( (~io.state_i(((i+1)%5)*5+((j)%5))) & io.state_i(((i+2)%5)*5+((j)%5)))
        }
    }
}

