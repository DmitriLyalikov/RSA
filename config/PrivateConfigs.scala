// See LICENSE for license
package rocketchip

import Chisel._
import uncore._
import rocket._
import cde.{Parameters, Field, Config, Knob, Dump, World, Ex, ViewSym}
import cde.Implicits._
import sha3._

class Sha3Config extends Config {
    override val topDefinitions:World.TopDefs = {
        (pname, site, here) => pname match {
            case WidthP => 64
            case Stages => Knob("stages")
            case FastMem => Knob("fast_mem")
            case BufferSram => Dump(Knob("buffer_sram"))
            case RoccMaxTaggedMemXacts => 32
            case BuildRocc => Seq(
                                RoccParameters(
                                    opcodes = OpcodeSet.custom0,
                                    generator = (p: Parameters) => (Module(new Sha3Accel()(p.alterPartial({case CoreName => "Rocket"}) ))) ))
    }
}

override val topConstraints:List[ViewSym=>Ex[Boolean]] = List(
    ex => ex(WidthP) === 64,
    ex => ex(Stages) >= 1 && ex(Stages) <= 4 && (ex(Stages)%2 === 0 || ex(Stages) === 1),
    ex => ex(FastMem) === ex(FastMem),
    ex => ex(BufferSram) === ex(BufferSram)
)

  override val knobValues:Any=>Any = {
    case "stages" => 1
    case "fast_mem" => true
    case "buffer_sram" => false
    case "multi_vt" => true
  }
}

class Sha3VLSIConfig extends Config(new Sha3Config ++ new DefaultVLSIConfig)
class Sha3FPGAConfig extends Config(new Sha3Config ++ new DefaultFPGAConfig) 
class Sha3CPPConfig extends Config(new Sha3Config ++ new DefaultCPPConfig) 


