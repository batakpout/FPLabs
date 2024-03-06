/**
  - Cats contains a wide variety of FP tools & allows devs to pick & choose the ones they want to use.
  - The majority of these tools are delivered in the form of type classes that we can apply to existing Scala types.
  - TCs are a programming pattern originating in Haskell. They allow us to extend existing types with new
    functionalities without using traditional inheritance, & without altering the original library source code.

  - There are 3 components of a TC: 1. the type class itself, 2. instances for particular types 3. and methods
    that use type classes
 -  TCs in scala are implemented using  implicit values and parameters and optionally using implicit classes.
 -  Scala language constructs that correspond to the components of the TC are: 1.traits( type classes)
    2. implicit values/implicit def with implicit param/implicit object (type classes instances)
    3. implicit parameters(type class use) 4. implicit classes (optional utility that make TC easier to use)

 - A type class is an interface or API that represents some functionality we want to implement. In scala a TC
  is represented by a trait with at least one type parameter.
 */
