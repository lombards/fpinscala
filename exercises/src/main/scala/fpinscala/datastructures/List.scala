package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }


  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }


  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => throw new IllegalStateException()
      case Cons(_, xs) => xs
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => throw new IllegalStateException()
      case Cons(_, xs) => Cons(h, xs)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    }
  }

  def init[A](l: List[A]): List[A] = {

    l match {
      case Nil => throw new IllegalArgumentException
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  }

  //book answers
  def init1[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }

    go(l)
  }


  def length[A](l: List[A]): Int = foldRight(l, 0)((_, z) => 1 + z)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    def loop(o: List[A], acc: B): B = {
      o match {
        case Nil => acc
        case Cons(h, t) => loop(t, f(h, acc))
      }
    }

    loop(l, z)
  }

  def sum5(l: List[Int]): Int = foldLeft(l, 0)(_ + _)


  def prod5(l: List[Double]): Double = foldLeft(l, 0.0)(_ * _)

  def length5[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)


  //Exercise 3.15
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((x, y) => Cons(x, y))

  //Model answer
  def appendViaFoldRightMA[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))


  // Exercise 3.16
  // Write a function that transforms a list of integers by adding 1 to each element.
  // (Reminder: this should be a pure function that returns a new List!)
  def add1(l: List[Int]): List[Int] = {
    foldRight(l, List[Int]())((a, b) => Cons(a + 1, b))
  }

  // Exercise 3.17
  //Write a function that turns each value in a List[Double] into a String.
  // You can use the expression d.toString to convert some d: Double to a String.
  def doublesToString(l: List[Double]): String = foldRight(l, "")((d, s) => s.concat(d.toString))

  //Actually the book wanted a List of strings back, here is the model answer
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))


  // Exercise 3.18
  // Write a function map that generalizes modifying each element in a list while
  // maintaining the structure of the list. Here is its signature:[12]

  // 12 In the standard library, map and flatMap are methods of List.

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    as match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }
  }

  //from the book
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))


  // Exercise 3.19
  //Write a function filter that removes elements from a list unless they satisfy a given predicate.
  // Use it to remove all odd numbers from a List[Int].


  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)


  // Exercise 3.20
  // Write a function flatMap that works like map except that the function given will return a list instead of a single result,
  // and that list should be inserted into the final resulting list. Here is its signature:
  //  For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3).

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(map(as)(f), Nil: List[B])((h, t) => append(h, t))

  /*
  Book answer
  */
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = ???
    //concat(map(l)(f))

  // Exercise 3.21
  //Use flatMap to implement filter.
  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) Cons(a, Nil:List[A]) else Nil)

  //book answer
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)


  //Exercise 3.22
  //Write a function that accepts two lists and constructs a new list by adding corresponding elements.
  // For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
  def matchAndAdd(a: List[Int], b: List[Int]) : List[Int] = {
    a match {
      case Nil => Nil
      case Cons(h, t) => b match {
        case Nil => Nil
        case Cons(x, y) => Cons(h + x, matchAndAdd(t, y));
      }
    }
  }

  //Book answer
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }



  //Exercise 3.23
  //Generalize the function you just wrote so that it’s not specific to integers or addition.
  // Name your generalized function zipWith.

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = {
    (a,b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case(Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
    }
  }


  //Book same answer, yay!



}


