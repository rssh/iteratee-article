package e3

sealed trait Input[+A]

object Input
{
  case class El[A](a:A) extends Input[A]
  case object EOF extends Input[Nothing]
}

trait MyIteratee[A,S]
{

  def fold[B](onNext: (Input[A] => MyIteratee[A,S]) => B,
              onDone: S => B 
          ): B

}



case class Done[A,S](s:S) extends MyIteratee[A,S]
{

  def fold[B](onNext: (Input[A] => MyIteratee[A,S]) => B,
              onDone: (S => B))  = 
                              onDone( s )


}


case class Collect[A](s:String) extends MyIteratee[A,String]
{

  def fold[B](onNext: (Input[A] => MyIteratee[A,String])=>B,
             onDone: String => B) =
        onNext { case Input.El(a) => Collect(s + a.toString)
                 case Input.EOF => Done(s)
               }

}

trait MyEnumerator[A]
{

  self =>

  def apply[S](it: MyIteratee[A,S]): MyIteratee[A,S]

  def andThen(x: MyEnumerator[A]) = new MyEnumerator[A] {
       def apply[S](it: MyIteratee[A,S]) = 
             x(self(it))
  }

  def eof() = andThen(new PutEOF[A])

}

case class PutOne[A](a:A) extends MyEnumerator[A]
{

   def apply[S](it: MyIteratee[A,S]):MyIteratee[A,S] =
          it.fold( k => k(Input.El(a)), Function.const(it) )
                 
}


class PutEOF[A] extends MyEnumerator[A]
{

   def apply[S](it: MyIteratee[A,S]):MyIteratee[A,S] =
          it.fold( k => k(Input.EOF), Function.const(it) )

}


case class ListEnumerator[A](l:List[A]) extends MyEnumerator[A]
{
 
   def apply[S](it: MyIteratee[A,S]):MyIteratee[A,S] =
     l.foldLeft(it)((it,e) => 
          it.fold( step => step(Input.El(e)), Function.const(it) ))


}



object E
{

  def main(args:Array[String]):Unit =
  {
    val s0 = ListEnumerator(List(1,2,3)).apply(Collect[Int](""))
    System.err.println("s0="+s0)
    System.err.println("s0 class = "+s0.getClass)
  }

}

