package e4

trait MyEnumerator[A]
{

  enumeratorThis =>

  def apply[S](it: MyIteratee[A,S]): MyIteratee[A,S]

  def andThen(x: MyEnumerator[A]) = new MyEnumerator[A] {
       def apply[S](it: MyIteratee[A,S]) = 
             x(enumeratorThis(it))
  }

  def eof() = andThen(new PutEOF[A])

  def map[B](f: A => B) = new MyEnumerator[B] {
       def apply[S](it: MyIteratee[B,S]) = 
       {
          val tr: MyIteratee[A,MyIteratee[B,S]] = MyEnumeratee.map(f).applyOn(it)
          val applyed = enumeratorThis.apply(tr)
          applyed.fold(
             step => step(Input.EOF).fold(
                       step1 => sys.error("strem must be closed after input"),
                       (in,s) => s
                     ),
             (in,s) => s 
          )
       }
  }

}

case class PutOne[A](a:A) extends MyEnumerator[A]
{

   def apply[S](it: MyIteratee[A,S]) =
          it.fold( k => k(Input.El(a)), (in, s) => it )
                 
}


class PutEOF[A] extends MyEnumerator[A]
{

   def apply[S](it: MyIteratee[A,S]) /*:MyIteratee[A,S]*/ =
          it.fold( k => k(Input.EOF), (in,s)=>it )

}


case class ListEnumerator[A](l:List[A]) extends MyEnumerator[A]
{
 
   def apply[S](it: MyIteratee[A,S]) /*:MyIteratee[A,S]*/ =
     l.foldLeft(it)((it,e) => 
          it.fold( step => step(Input.El(e)), (in,s) => it ))


}



