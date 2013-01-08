package imp


trait MyEnumerator[A] 
{

  enumeratorThis =>

  def apply[S](it: MyIteratee[A,S]): MyIteratee[A,S]

  def andThen(x: MyEnumerator[A]) = new MyEnumerator[A] {
       def apply[S](it: MyIteratee[A,S]) = x(enumeratorThis(it))
  }

  def eof() = andThen(MyEnumerator.eof())

  def map[B](f: A => B) = new MyEnumerator[B] {
       def apply[S](it: MyIteratee[B,S]) =
       {
          val tr: MyIteratee[A,MyIteratee[B,S]] = MyEnumeratee.map(f).applyOn(it)
          val applyed = enumeratorThis.apply(tr)
          applyed.next(Input.EOF) match {
              case Done(in,s) => s
              case _ => sys.error("stream must be closed after input")
          }
       }
  }


}

object MyEnumerator
{

   def eof[A](): MyEnumerator[A] = new MyEnumerator[A]{ 
          def apply[S](it:MyIteratee[A,S])=it.next(Input.EOF)
   }

}


case class PutOne[A](a:A) extends MyEnumerator[A]
{

   def apply[S](it: MyIteratee[A,S]) = it.next(Input.El(a))
     
}



case class ListEnumerator[A](l:List[A]) extends MyEnumerator[A]
{
 
   def apply[S](it: MyIteratee[A,S]) =
     l.foldLeft(it)((it,e)=>it.next(Input.El(e)))


}



