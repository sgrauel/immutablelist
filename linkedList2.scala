
//  SORTING ALGORITHMS TO ADD: bubble sort, insertion sort, selection sort, and quicksort

object ListImplementation2 {

  // LinkedList[+T] type constructor and data constructor pattern with subtype polymorphism
  sealed trait LinkedList[+T] {
    def ::[U >: T](hd: U): LinkedList[U]
    def :::[U >: T](u: LinkedList[U]): LinkedList[U]
    def head: T
    def tail: LinkedList[T]
  }

  // data constructor for 'Empty' instances
  case object Empty extends LinkedList[Nothing] {
    def ::[U >: Nothing](hd: U): LinkedList[U] = NonEmpty(hd,this)
    def :::[U >: Nothing](u: LinkedList[U]): LinkedList[U] = u match {
      case xs: Empty.type => this
      case NonEmpty(x: U, xs: LinkedList[U]) => x :: (xs ::: this)
    }
    def head: Nothing = throw new Exception("Error. Cannot get head of empty list!")
    def tail: LinkedList[Nothing] = throw new Exception("Error. Cannot get tail of empty list!")
  }

  // data constructor for 'NonEmpty' instances
  // NonEmpty extends LinkedList[+T] NOTE: '+' is implicit here
  case class NonEmpty[+T](val hd: T, val tl: LinkedList[T]) extends LinkedList[T] {
    def ::[U >: T](hd: U): LinkedList[U] = NonEmpty(hd,this)
    def :::[U >: T](u: LinkedList[U]): LinkedList[U] = u match {
      case xs: Empty.type => this
      case NonEmpty(x: U, xs: LinkedList[U]) => 
        x :: (xs ::: this)
    }
    def head: T = hd
    def tail: LinkedList[T] = tl
  }

  // LinkedList companion object to the LinkedList[+T] type constructor pattern
  // purpose: hold one or more components, but there is no 'signature' 
  // i.e. an abstract interface that defines the contents of this object (like the ML module pattern)
  object LinkedList {

    def isEmpty[A](li1: LinkedList[A]): Boolean = (li1) match {
      case (x: Empty.type) => true
      case (x: NonEmpty[A]) => false
    }

    def map[A,B](li1: LinkedList[A])(f: A => B): LinkedList[B] = (f,li1) match {
      case (_, xs: Empty.type) => Empty
      case (f, NonEmpty(x: A, xs: LinkedList[A])) => 
        f(x) :: map(xs)(f)
    }

    def filter[A](li1: LinkedList[A])(p: A => Boolean): LinkedList[A] = (p,li1) match {
      case (_, xs: Empty.type) => Empty
      case (p, NonEmpty(x: A, xs: LinkedList[A])) => 
        if (p(x)) x :: filter(xs)(p) else filter(xs)(p)
    }

    def take[A](n: Int)(li1: LinkedList[A]): LinkedList[A] = {
      if (n < 0) throw new Exception("Error. You cannot take a negative number of elements")
      else if (n >= length(li1)) li1
      else (li1) match {
        case (xs: Empty.type) => li1
        case (NonEmpty(x: A, xs: LinkedList[A])) => 
          if (n != 0) x :: take(n - 1)(xs) else Empty
      }
    }

    def takeWhile[A](li1: LinkedList[A])(p: A => Boolean): LinkedList[A] = (p,li1) match {
      case (_, xs: Empty.type) => Empty
      case (p, NonEmpty(x: A, xs: LinkedList[A])) => 
        if (p(x)) x :: takeWhile(xs)(p) else Empty
    }

    def length[A](li1: LinkedList[A]): Int = (li1) match {
      case (xs: Empty.type) => 0
      case (NonEmpty(x: A, xs: LinkedList[A])) => 1 + length(xs)
    }

    def drop[A](n: Int)(li1: LinkedList[A]): LinkedList[A] = {
      if (n < 0) throw new Exception("Error. You cannot drop a negative number of elements")
      else if (n >= length(li1)) Empty
      else (li1) match {
        case (xs: Empty.type) => li1
        case (NonEmpty(x: A, xs: LinkedList[A])) => 
          if (n != 0) drop(n - 1)(xs) 
          else x :: drop(n)(xs)
      }
    }

    def dropWhile[A](li1: LinkedList[A])(p: A => Boolean): LinkedList[A] = (p,li1) match {
      case (_, xs: Empty.type) => Empty
      case (p, NonEmpty(x: A, xs: LinkedList[A])) => 
        if (p(x)) dropWhile(xs)(p) else x :: xs
    }

    def zipWith[A,B,C](li1: LinkedList[A])(li2: LinkedList[B])(f: A => B => C): LinkedList[C] = (f,li1,li2) match {
      case (_, _, xs: Empty.type) => Empty
      case (_, xs: Empty.type, _) => Empty
      case (f, NonEmpty(x: A, xs: LinkedList[A]), NonEmpty(y: B, ys: LinkedList[B])) => 
        f(x)(y) :: zipWith(xs)(ys)(f)
    }

    def zip[A,B](li1: LinkedList[A])(li2: LinkedList[B]): LinkedList[(A,B)] = (li1,li2) match {
      case (NonEmpty(x: A, xs: LinkedList[A]), NonEmpty(y: B, ys: LinkedList[B])) => (x,y) :: zip(xs)(ys)
      case (_,_) => Empty
    }

    def reverse[A](li1: LinkedList[A]): LinkedList[A] = li1 match {
      case x: Empty.type => Empty
      case NonEmpty(x: A, xs: LinkedList[A]) => reverse(xs) ::: NonEmpty(x,Empty)
    }

    def flatten(li1: LinkedList[_]): LinkedList[Any] = li1 match {
      case xs: Empty.type => Empty
      case NonEmpty(x: LinkedList[_], xs: LinkedList[_]) => flatten(x) ::: flatten(xs)
      case NonEmpty(x: Any, xs: LinkedList[_]) => x :: flatten(xs)
    }

    /*
     INPUT: list and an element
     OUTPUT: returns a new list with the first element replaced by the element argument
    */
    def setHead[A](li1: LinkedList[A])(elem: A): LinkedList[A] = (li1,elem) match {
      case (xs: Empty.type, y: A) => y :: xs
      case (NonEmpty(x: A, xs: LinkedList[A]), y: A) => y :: xs
    }

    // Combiner or 'monoid' typeclass that defines and describes the behavior of combining two objects into one
    trait Combiner[A] {
      def combiner(a: A, b: A): A
      def acc: A
    }

    // type instances that support and implement 
    // the behavior of the monoid typeclass for adding Int, Float and Double types
    val intCombinerAdd = new Combiner[Int] {
      def combiner(a: Int, b: Int): Int = a + b
      def acc: Int = 0
    }

    val floatCombinerAdd = new Combiner[Float] {
      def combiner(a: Float, b: Float): Float = a + b
      def acc: Float = 0
    }

    val dblCombinerAdd = new Combiner[Double] {
      def combiner(a: Double, b: Double): Double = a + b
      def acc: Double = 0
    }

    // type instances that support and implement the behavior 
    // of the monoid typeclass for multiplying Int, Float and Double types
    val intCombinerMult = new Combiner[Int] {
      def combiner(a: Int, b: Int): Int = a * b
      def acc: Int = 1
    }

    val floatCombinerMult = new Combiner[Float] {
      def combiner(a: Float, b: Float): Float = a * b
      def acc: Float = 1
    }

    val dblCombinerMult = new Combiner[Double] {
      def combiner(a: Double, b: Double): Double = a * b
      def acc: Double = 1
    }

    /*
     foldRight generalizes the process of recursing into a list, combining the last element with 
     the base case via a combiner function i.e. monoid, obtaining a result to be combined with 
     the next to last element and so on until we obtain a result to be combined with
     the first element, which is our final result
    */
    def foldRight[A](li1: LinkedList[A])(baseCase: A)(f: (A,A) => A): A = (li1) match {
      case (xs: Empty.type) => baseCase
      case (NonEmpty(x: A, xs: LinkedList[A])) => f(x,foldRight(xs)(baseCase)(f))
    }

    def sum[A](li1: LinkedList[A])(comb: Combiner[A]): A = foldRight(li1)(comb.acc)(comb.combiner)

    def product[A](li1: LinkedList[A])(comb: Combiner[A]): A = foldRight(li1)(comb.acc)(comb.combiner)

    /*
    def sum[A](li1: LinkedList[A])(comb: Combiner[A]): A = (li1,comb) match {
      case (xs: Empty.type, c) => c.acc
      case (NonEmpty(x: A, xs: LinkedList[A]), c) => c.combiner(x,sum(xs)(comb))
    }

    def product[A](li1: LinkedList[A])(comb: Combiner[A]): A = (li1,comb) match {
        case (xs: Empty.type, c) => c.acc
        case (NonEmpty(x,xs), c) => c.combiner(x,product(xs)(comb))
    }
    */

    /* 
     variadic function : a function that takes 0 or more arguments of type A
     syntax: 'A*'
     'as' is bound to a Seq[A] data structure with 0 or more arguments passed into a 'list literal' bearing the object's identifier
     PURPOSE: to construct instances of our ADT 'LinkedList'
     - as has two operations : head and tail
     - pass a Seq[A] to be interpreted as 0 or more arguments to apply recursively with
     type annotation ': _*'
     */
    def apply[A](as: A*): LinkedList[A] = 
      if (as.isEmpty) Empty else NonEmpty(as.head,apply(as.tail : _*))
  }


  def main(args: Array[String]): Unit = {
    val xs: LinkedList[Double] = LinkedList(1.1,4.5,6.7)
    println(LinkedList.sum(xs)(LinkedList.dblCombinerAdd))
    println(LinkedList.product(xs)(LinkedList.dblCombinerMult))
  }

}
