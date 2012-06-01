import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class CalculatorSpec extends FunSpec {
  describe("Calculator") {
    describe("apply") {
      it("Should identify numbers") {
        assert(Calculator("0") == 0)
      }
      
      it("Should do addition") {
        assert(Calculator("1 + 1") == 2)
      }
      
      it("Should do subtraction") {
        assert(Calculator("2 - 1") == 1)
      }
    }
  }
}