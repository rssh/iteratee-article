package imp

import org.scalatest._

class MyIterateeImpTest  extends FunSuite
{

  test("[imp] iteratee Collect really collect strings") {
    val s0 = ListEnumerator(List(1,2,3)).eof()(Collect[Int](""))
    //s0.fold( k => assert(false, "iteration must be done after EOF"),
    //         (in,s) => assert(s === "123")
    //       )
    assert(s0.isDone)
    assert( (s0.whenDone.get._2 === "123") )
  }

  test("[imp] compositions of iteratees") {
    case class Accept(s:String, m: Boolean = true) extends MyIteratee[Char, Boolean]
    {
      def next(in: Input[Char]): MyIteratee[Char,Boolean] =
        if (s.length == 0)  Done(in,true)
        else 
         in match {
           case Input.El(ch) => if (ch==s.head) 
                                   s.tail match {
                                     case "" => Done(Input.Empty,true)
                                     case rest => Accept(rest, true)
                                   }
                                else Done(in, false)
           case Input.Empty => this
           case Input.EOF => Done(Input.EOF,m)
         }
    }
    val it = Accept("hello") andThen Accept(" ") andThen Accept("world")
    val e0 = ListEnumerator("hello world".toList)
    e0(it).fold( step => assert(false,"e0 must be finished, we have:"+e0(it)),
                 (inp, res) => assert(res,"hello world must match")
               )
    val e1 = ListEnumerator("qqq".toList)
    e1(it).fold( step => assert(false, "e1 processing must be finished"),
                 (inp, res) => assert(!res,"qqq must not match")
               )
  }

    case class Sum(acc: Int=0) extends MyIteratee[Int, Int]
    {
      def next(in: Input[Int]):MyIteratee[Int,Int] = 
        in match {
          case Input.El(x) => Sum(acc+x)
          case Input.Empty => Sum(acc)
          case Input.EOF => Done(Input.EOF,acc)
        }
    }

  test("[imp] mapping iteratee with enumeratee") {
    val mapEnumeratee = MyEnumeratee.map[String,Int](Integer.parseInt(_))
    val stringSum = mapEnumeratee.transform(Sum())
    val e1 = ListEnumerator(List("1","2","3")).eof()
    val r = e1(stringSum)
    //System.err.println("r="+r)
    r.fold( step => assert(false, "e1 processing must be finished, we have:"+r),
            (inp, res) => assert(res===(1+2+3))
          )
  }

  test("[imp] mapping enumerator with enumeratee") {
    val e1 = ListEnumerator(List("1","2","3")).eof()
    val e2 = e1 map (Integer.parseInt(_))
    val r = e2(Sum())
    r.fold( step => assert(false, "e1 processing must be finished"),
            (inp, res) => assert(res===(1+2+3))
          )
  }

}

