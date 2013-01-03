package e3

import org.scalatest._

class MyIteratee3Test  extends FunSuite
{

  test("iteratee Collect reallu collect strings") {
    val s0 = ListEnumerator(List(1,2,3)).eof()(Collect[Int](""))
    s0.fold( k => assert(false, "iteration must be done after EOF"),
             s => assert(s === "123")
           )
  }

}

