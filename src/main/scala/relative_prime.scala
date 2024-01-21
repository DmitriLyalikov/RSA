package rsa

import Chisel._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

// Generate public key given phi 
// Public key is a number e such that 1 < e < phi and gcd(e, phi) = 1
class PublicKeyModule(val W: Int = 64) extends Module {
    val io = new Bundle {
        val phi = Bits(INPUT, W)
        val e = Bits(OUTPUT, W)
    }
    // Uses the Euclidean algorithm to find the greatest common divisor
    def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)
    // Find a random number e such that 1 < e < phi and gcd(e, phi) = 1
        var e = BigInt(0) 
        do {
            e = BigInt(W, Random)
        } while (e <= 1 || e >= io.phi || gcd(e, io.phi) != 1)
    io.e := e
}
  