package e4

sealed trait Input[+A]

object Input
{
  case class El[A](a:A) extends Input[A]
  case object Empty extends Input[Nothing]
  case object EOF extends Input[Nothing]
}

trait MyIteratee[A,S]
{

  def fold[B](onNext: (Input[A] => MyIteratee[A,S]) => B,
              onDone: (Input[A], S) => B 
          ): B

}



case class Done[A,S](in:Input[A],s:S) extends MyIteratee[A,S]
{

  def fold[B](onNext: (Input[A] => MyIteratee[A,S]) => B,
              onDone: (Input[A], S) => B)  = 
                              onDone(in, s)


}


case class Collect[A](s:String) extends MyIteratee[A,String]
{

  def fold[B](onNext: (Input[A] => MyIteratee[A,String])=>B,
             onDone:  (Input[A], String) => B) =
        onNext { case Input.El(a) => Collect(s + a.toString)
                 case Input.Empty => Collect(s)
                 case Input.EOF => Done(Input.EOF, s)
               }

}



