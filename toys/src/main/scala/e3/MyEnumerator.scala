package e3

trait MyEnumerator[A]
{

  self =>

  def apply[S](it: MyIteratee[A,S]): MyIteratee[A,S]

  def andThen(x: MyEnumerator[A]) = new MyEnumerator[A] {
       def apply[S](it: MyIteratee[A,S]) = 
             x(self(it))
  }

  def eof() = andThen(new PutEOF[A])

}

case class PutOne[A](a:A) extends MyEnumerator[A]
{

   def apply[S](it: MyIteratee[A,S]):MyIteratee[A,S] =
          it.fold( k => k(Input.El(a)), Function.const(it) )
                 
}


class PutEOF[A] extends MyEnumerator[A]
{

   def apply[S](it: MyIteratee[A,S]):MyIteratee[A,S] =
          it.fold( k => k(Input.EOF), Function.const(it) )

}

object PutEOF
{

  def apply[A,S](it: MyIteratee[A,S]): MyIteratee[A,S] =
       it.fold( k => k(Input.EOF), Function.const(it) )

}


case class ListEnumerator[A](l:List[A]) extends MyEnumerator[A]
{
 
   def apply[S](it: MyIteratee[A,S]):MyIteratee[A,S] =
     l.foldLeft(it)((it,e) => 
          it.fold( step => step(Input.El(e)), Function.const(it) ))


}



