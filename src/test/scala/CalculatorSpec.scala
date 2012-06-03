import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class CalculatorSpec extends FunSpec {
  describe("Calculator") {
    describe("apply") {
      it("Should identify numbers") {
        assert(Calculator("0") === 0)
      }
      
      it("Should do addition") {
        assert(Calculator("1 + 1") === 2)
      }
      
      it("Should do subtraction") {
        assert(Calculator("2 - 1") === 1)
      }
      
      it("Should do iterated addition") {
        assert(Calculator("1 + 2 + 3 + 5 + 7 + 2") === 1 + 2 + 3 + 5 + 7 + 2)
      }
      
      it("Should do iterated subtraction") {
        assert(Calculator("1 - 2 - 3 - 5 - 7 - 2") === 1 - 2 - 3 - 5 - 7 - 2)
      }
      
      it("Should do interleaved addition and subtraction") {
        assert(Calculator("1 + 2 - 3 + 5 - 7 - 2") === 1 + 2 - 3 + 5 - 7 - 2)
      }

      it("Should use parenthesization properly") {
        assert(Calculator("1 + 2 - 3 + 5 - (7 - 2)") === 1 + 2 - 3 + 5 - (7 - 2))
      }

      it("Should handle multiplication") {
        assert(Calculator("2 * 7") === 2 * 7)
      }
      
      it("Should handle iterated multiplication") {
        assert(Calculator("2 * 7 * 3 * 8 * 9 * 6") === 2 * 7 * 3 * 8 * 9 * 6)
      }

      it("Should handle mixed precedence levels") {
        assert(Calculator("2 * 4 + 2 * 8 * 9 * 6") === 2 * 4 + 2 * 8 * 9 * 6)
      }

    }
  }
}