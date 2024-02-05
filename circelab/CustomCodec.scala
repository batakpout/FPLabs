package circe_lab

object OptionTest extends App {
    //Option, EIther, Try etc
    //Monads --> Category Theory

    //Option --> algebric data type.
    //Some, None


    val someValue : Option[Int]= Some(1) //value exists
    println(someValue)
    val noValue: Option[Int] = None // absence of value
    println(noValue)


    val transformedValue1: Option[Int] = someValue.map(insideValue => insideValue + 1)
    val transformedValue2 = someValue.map(_ + 1)

    println(transformedValue1)
    println(transformedValue2)

    val wrappedValue: Int = someValue.getOrElse(0)

    println(wrappedValue)


  }
