package com.rockthejvm.part5testing

import zio.*
import zio.test.*
import com.rockthejvm.utils.*

object PropertyBasedTesting extends ZIOSpecDefault:
  // general
  val effectGenerator = Gen.fromZIO(ZIO.succeed(42L))

  def spec =
    suite("property-based")(
      test("basics") {
        check(Gen.int, Gen.int, Gen.int) { (x, y, z) =>
          assertTrue((x + y) + z == x + (y + z))
        }
      }
    )

object Playground extends ZIOAppDefault:
  def run =
    val generalGenerator  = Gen.unfoldGen(0)(i => Gen.const(i + 1).zip(Gen.stringN(i)(Gen.alphaNumericChar)))
    val generatedListsZIO = generalGenerator.runCollectN(5)
    generatedListsZIO.debugThread


