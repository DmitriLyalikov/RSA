package rsa 

import Chisel._
import scala.collection.mutable.HashMap


// Encode a message using the RSA algorithm
// C = M^e mod n
class EncodeModule(val W: Int = 64) extends Module {
    val io = new Bundle {
        val m = Bits(INPUT, W)
        val e = Bits(INPUT, W)
        val n = Bits(INPUT, W)
        val c = Bits(OUTPUT, W)
    }

    io.c := io.m ^ io.e ^ io.n
}