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



