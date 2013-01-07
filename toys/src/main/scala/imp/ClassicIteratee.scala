package imp

/**
 * mapping of classic functional to object interface.
 */
trait ClassicIteratee[A,S]
{

  def fold[B](onNext: (Input[A]=>ClassicIteratee[A,S])=>B,
              onDone: (Input[A],S) => B): B 

  def next(inp: Input[A]): ClassicIteratee[A,S] =
       fold(
          next => next(inp),
          (rest,s) => this
       )

  def isDone: Boolean =
       fold(next => true,
            (rest,s) => false)

  def whenDone: Option[(Input[A],S)] =
       fold(next => None,
            (rest,s) => Some((rest,s))
       )

  def whenNext: Option[Input[A]=>ClassicIteratee[A,S]]  =
       fold(next => Some(next), (rest,s) => None)

}


