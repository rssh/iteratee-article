package imp

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
  case object EOF   extends Input[Nothing]
}


// analog of iteratee
trait MyIteratee[A,S]
{

  iterateeThis =>

  def next(inp: Input[A]): MyIteratee[A,S]

  def isDone: Boolean = whenDone.isDefined

  def whenDone: Option[(Input[A],S)] =
       this match {
         case Done(rest,s) => Some((rest,s))
         case _ => None
       }

  def whenNext: Option[Input[A]=>MyIteratee[A,S]]  =
        if (isDone) None else Some(next(_))

  def fold[B](onNext: (Input[A]=>MyIteratee[A,S])=>B,
              onDone: (Input[A],S) => B): B =
     whenNext.map(onNext(_)) getOrElse(
       whenDone.map(x => onDone(x._1,x._2)) getOrElse sys.error("one of whenNext or whenDone must be defined") 
     )

}

case class Done[A,S](rest: Input[A], s: S) extends MyIteratee[A,S]
{

  def next(inp: Input[A]) = this

  override def isDone: Boolean = true

  override def whenDone: Option[(Input[A],S)] = Some((rest,s))

}

case class Cont[A,S](f: Input[A] => MyIteratee[A,S]) extends MyIteratee[A,S]
{

  def next(inp: Input[A]) = f(inp)

}

case class Collect[A](s:String="") extends MyIteratee[A,String]
{
  override def next(in: Input[A]) = 
   { in match {
         case Input.El(a) => Collect(s+a.toString)
         case _ => Done(in,s)
   }          }
}

