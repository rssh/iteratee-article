package imp

trait MyEnumeratee[From,To]
{
  enumeratee =>

  def applyOn[S](it: MyIteratee[To,S]): MyIteratee[From, MyIteratee[To,S]]

  def transform[S](it: MyIteratee[To,S]): MyIteratee[From,S] =
    new JoinIteratee(applyOn(it))


  case class JoinIteratee[S](it:  MyIteratee[From, MyIteratee[To,S]]) 
                                                   extends MyIteratee[From, S] 
  {

     def next(in: Input[From]) =
      {
        def runAll[X](it: MyIteratee[X,S]) = 
          it match {
            case Done(in,y) => y
            case _ => it.next(Input.EOF) match {
                        case Done(in,s) => s
                        case _ => sys.error("iteratee is not done ager eof")
                      }
         }
        it match {
          case Done(inr,inner) => Done(in,runAll(inner))
          case _ => new JoinIteratee(it.next(in))
        } 
      }

    }

}

case class MapEnumeratee[From,To](f:From => To) extends MyEnumeratee[From,To]
{

  def applyOn[S](it: MyIteratee[To,S]): MyIteratee[From, MyIteratee[To,S]]=
    new MapIteratee[S](it)

  case class MapIteratee[S](it: MyIteratee[To,S]) extends MyIteratee[From, MyIteratee[To,S]] 
   {

      def next(in: Input[From]) = it.next(in map f) match {
                                  case Done(in1,s) => Done(in,Done(in1,s))
                                  case x => MapIteratee(x)
                               }

   }


}

object MyEnumeratee
{

  def map[From,To](f: From => To)= new MapEnumeratee[From, To](f)

}


