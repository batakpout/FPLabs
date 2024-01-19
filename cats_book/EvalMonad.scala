package cats_book

object EvalMonad_ModelOfEvaluation extends App {

  println("---call by value (Eager and Memoized)")
  val x: Double = {
    println("Computing X")
    math.random
  }
  println(x)
  println(x)

  println("---call by name (Lazy and not memoized)")
  def y = {
    println("Computing y")
    math.random
  }
  println(y)
  println(y)

  println("---call by need (Lazy and memoized)")
  lazy val z = {
    println("Computing z")
    math.random
  }

  println(z)
  println(z)

}

object EvalMonad1 extends App {
  import cats.Eval

  val now: Eval[Double] = Eval.now(math.random + 1000)
  val always: Eval[Double] = Eval.always(math.random + 1000)
  val later: Eval[Double] = Eval.later(math.random + 1000)

  println(now)
  println(always)
  println(later)

  println(now.value)
  println(always.value)
  println(always.value)

  println("---Eval.now semantics similar to val----")

  val x: Eval[Double] = Eval.now {
    println("Computing X")
    math.random
  }
  println(x)
  println(x.value)
  println(x.value)

  println("---Eval.always semantics similar to def----")
  val y: Eval[Double] = Eval.always {
    println("Computing Y")
    math.random
  }
  println(y.value)
  println(y.value)

  println("---Eval.later semantics similar to lazy val----")
  val z: Eval[Double] = Eval.later {
    println("Computing Z")
    math.random
  }
  println(z.value)
  println(z.value)


}

object EvalMonad2 extends App {
    import cats.Eval
    val greeting: Eval[String] = Eval.always {println("Step 1"); "Hello"}
      .map{s => println("Step 2"); s"$s World"}

    val r1: String = greeting.value
    println(r1)

 // map, flatMap has def semantics
  println("--------------")

  val ans: Eval[Int] = for {
     a <- Eval.now{ println("Calculating A"); 40}
     b <- Eval.now { println("Calculating B"); 2}
  } yield {
    println("Adding A and B")
    a + b
  }
  val r2 = Eval.now{ println("Calculating A"); 40}.flatMap { a => Eval.now { println("Calculating B"); 2}.map{ b => a + b}}
  println(ans)
  println(ans)
  println(r2)
  println("====1====")
  println(ans.value)
  println("====2====")
  println(ans.value)
  
   println("------------Memoizing chain of computations------------")
   val saying: Eval[String] = Eval.always{ println("Step 1"); "The Cat"}.
     map{x => println("Step 2"); s"$x sat on"}
     .memoize
     .map{s => println("Step 3"); s"$s the mat."}
  
   println(saying.value)
   println("--memoize 1---")
   println(saying.value)
   println("--memoize 2---")
  println(saying.value)
}
