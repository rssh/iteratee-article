package imp

trait MyEnumeratee[From,To]
{
  enumeratee =>

  def applyOn[S](it: MyIteratee[To,S]): MyIteratee[From, MyIteratee[To,S]]

  def transform[S](it: MyIteratee[To,S]): MyIteratee[From,S] =
    new JoinIteratee(applyOn(it))

  def join[S](it: MyIteratee[From, MyIteratee[To,S]]): MyIteratee[From,S] =
     it match {
       case Done(from,inner) => Done(from, runAll(inner))
       case _ => JoinIteratee(it)
     }

   case class JoinIteratee[S](it:  MyIteratee[From, MyIteratee[To,S]]) 
                                                   extends MyIteratee[From, S] 
   {

     def next(in: Input[From]) = join(it.next(in))

   }


   private def runAll[X,S](it: MyIteratee[X,S]) = 
         it match {
           case Done(in,y) => y
           case _ => it.next(Input.EOF) match {
                       case Done(in,s) => s
                       case _ => sys.error("iteratee is not done ager eof")
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


