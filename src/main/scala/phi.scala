package rsa

import Chisel._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


// Calculate the Phi and N values for the RSA algorithm
// Phi = (p-1)*(q-1)
// N = p*q
class PhiModule(val W: Int = 64) extends Module {
    val io = new Bundle {
        val p = Bits(INPUT, W)
        val q = Bits(INPUT, W)
        val phi = Bits(OUTPUT, W)
        val n = Bits(OUTPUT, W)
    }

    io.phi := (io.p - UInt(1)) * (io.q - UInt(1))
    io.n := io.p * io.q
}