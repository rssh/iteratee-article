package imp


trait MyEnumerator[A] 
{

  enumeratorThis =>

  def apply[S](it: MyIteratee[A,S]): MyIteratee[A,S]

  def andThen(x: MyEnumerator[A]) = new MyEnumerator[A] {
       def apply[S](it: MyIteratee[A,S]) = x(enumeratorThis(it))
  }

  def eof() = andThen(MyEnumerator.eof())


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



