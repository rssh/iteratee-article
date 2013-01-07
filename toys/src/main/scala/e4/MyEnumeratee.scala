package e4

trait MyEnumeratee[From,To]
{
  enumeratee =>

  def applyOn[S](it: MyIteratee[To,S]): MyIteratee[From, MyIteratee[To,S]]

  def transform[S](it: MyIteratee[To,S]): MyIteratee[From,S] =
    new JoinIteratee(applyOn(it))


  case class JoinIteratee[S](it:  MyIteratee[From, MyIteratee[To,S]]) 
                                                   extends MyIteratee[From, S] 
  {

   def fold[B](onNext: (Input[From] => MyIteratee[From,S]) => B,
               onDone: (Input[From], S) => B
          ): B =
      {
        def runAll[X](it: MyIteratee[X,S]): S =
          it.fold(
            step => step(Input.EOF),
            (in,s) => Done(in,s)
          ).fold(
             step => sys.error("iteratee is not done after after eof"),
             (in,S) => S
          )
        it.fold(
          // step: Input[From] => MyIteratee[From,MyIteratee[To,S]] 
          step => onNext({ x => new JoinIteratee(step(x))  }),
          (in, inner) => onDone(in, runAll(inner))
        )
      }

  }


}

case class MapEnumeratee[From,To](f:From => To) extends MyEnumeratee[From,To]
{

  def applyOn[S](it: MyIteratee[To,S]): MyIteratee[From, MyIteratee[To,S]]=
    new MapIteratee[S](it)

  case class MapIteratee[S](it: MyIteratee[To,S]) extends MyIteratee[From, MyIteratee[To,S]] {
      def fold[B](onNext:(Input[From] => MyIteratee[From,MyIteratee[To,S]]) => B,
                  onDone:(Input[From], MyIteratee[To,S]) => B
                 ): B =
           it.fold(
             step => onNext( x => applyOn(step(x map f)) ),
             (in, s) => //onNext( x => Done(x,Done(in,s)) ) 
                        onDone(Input.Empty,Done(in,s))
           )
    }


}

object MyEnumeratee
{

  def map[From,To](f: From => To)= new MapEnumeratee[From, To](f)

}


