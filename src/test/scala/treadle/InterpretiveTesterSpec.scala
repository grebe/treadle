//// See LICENSE for license details.
//
//treadle treadle
//
//import org.scalatest.{FlatSpec, Matchers}
//
//class InterpretiveTesterSpec extends FlatSpec with Matchers {
//  behavior of "cycle mechanism"
//
//  it should "mark circuit as stale after poke" in {
//    val input =
//      """
//        |circuit Stop0 :
//        |  module Stop0 :
//        |    input clk : Clock
//        |    input a : UInt<16>
//        |    input b : UInt<16>
//        |    output c : UInt<16>
//        |
//        |    reg reg1 : UInt<16>, clk
//        |
//        |    reg1 <= add(a, b)
//        |    c <= add(reg1, UInt(1))
//        |
//      """.stripMargin
//
//    val tester = new InterpretiveTester(input)
//    val engine = tester.engine
//
//    engine.circuitState.isStale should be (false)
//
//    tester.poke("a", 1)
//
//    engine.circuitState.isStale should be (true)
//
//    tester.peek("c")
//
//    engine.circuitState.isStale should be (false)
//  }
//}
