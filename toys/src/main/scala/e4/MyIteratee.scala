package e4

sealed trait Input[+A]
{
  def map[B](f: A=>B): Input[B] =
    this match {
      case Input.El(a) => Input.El(f(a))
      case Input.Empty => Input.Empty
      case Input.EOF => Input.EOF
    }
}

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

  def andThen(x: MyIteratee[A,S]) = Compose(this,x)
 
  def map[B](f: B=>A) = MyEnumeratee.map(f).transform(this)

}



case class Done[A,S](in:Input[A],s:S) extends MyIteratee[A,S]
{

  def fold[B](onNext: (Input[A] => MyIteratee[A,S]) => B,
              onDone: (Input[A], S) => B)  = 
                              onDone(in, s)


}

case class Cont[A,S](k: Input[A] => MyIteratee[A,S]) extends MyIteratee[A,S]
{

  def fold[B](onNext: (Input[A] => MyIteratee[A,S]) => B,
              onDone: (Input[A], S) => B)  = onNext(k)

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

case class Compose[A,S](frs: MyIteratee[A,S], 
                        snd: MyIteratee[A,S]
                       ) extends  MyIteratee[A,S] 
{

  def fold[B](onNext: (Input[A] => MyIteratee[A,S])=>B,
             onDone:  (Input[A], S) => B) =
        frs.fold( step => onNext(
                           x => Compose(step(x),snd)
                          ),
                  (in, s) => in match {
                               case Input.EOF => onDone(in,s)
                               case Input.Empty => snd.fold(onNext,onDone)
                               case Input.El(a) => 
                                       PutOne(a)(snd).fold(onNext,onDone)
                             }
                )
}

