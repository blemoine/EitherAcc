EitherAcc
=========

An `Either` type on steroïds, able to accumulate error types. 


Why
---
`Either`, especially in Scala 2.12, can be used to perform error management. 
By convention `Left` will represent the error and `Right` will represent the expected value.

For example: 
```tut:silent
def parseStr(s: String): Either[String, Int] = try {
    Right(s.toInt)
  } catch {
    case _:NumberFormatException => Left(s"$s cannot be formatted")
  }  
```

This function will return a `String` when there is an error, and an `Int` when the parameter `String` can be parsed.

```tut
parseStr("test")

parseStr("34")
```

That being said, `String` as an error type is not really descriptive. 
From an external point of view, we know that the function can fail, but we don't know with what kind of failure occurred.
It would be better to use a _specific type_ to represent our error.

```tut:silent
case class NumberFormatError(nonFormattableString: String)
def parse(s: String): Either[NumberFormatError, Int] = try {
      Right(s.toInt)
    } catch {
      case _:NumberFormatException => Left(NumberFormatError(s))
    }  
```

```tut
parse("test")
```

It's clearly better, but we now have a problem when mixing different error types.

```tut
case class DivideByZero(nb1: Double) 
def divide(nb1: Double, nb2: Double): Either[DivideByZero, Double] = {
   if (nb2 == 0) Left(DivideByZero(nb1))
   else Right(nb1 / nb2)
}
```

The following code will generate an `Either` with the super type of our two error types. The problem is that the only super type they share in common is
`Serializable with Product`, thus we lose all type information pertaining to the error, which stands in the way of our goal to achieve a more precise type.

```tut
for {
  nb1 <- parse("2")
  nb2 <- parse("3")
  result <- divide(nb1, nb2)
} yield result
```

The way to circumvent this is to try to have a
`Either[Either[NumberFormatError, DivideByZero], Double]`, but to generate this is not easy a lot of _accidental complexity_:


```tut
for {
  nb1 <- parse("2").left.map(x => Left(x))
  nb2 <- parse("3").left.map(x => Left(x))
  result <- divide(nb1, nb2).left.map(x => Right(x))
} yield result
```

And it would be even more complex with more than two error types.

`EitherAcc` is a way to solve this problem.
 
What
---

`EitherAcc` is a :
   - disjunction - there is 2 possible value, `Err` and `Success`.
   - like `Either`, `Err` is absorbing: you don't accumulate errors
   - unlike `Either` it's accumulating error _types_
   
Looking at our previous example, we can use `EitherAcc` the following way:

```tut
import co.sachemmolo.eitheracc.EitherAcc

for {
  nb1 <- EitherAcc.fromEither(parse("2"))
  nb2 <- EitherAcc.fromEither(parse("3"))
  result <- EitherAcc.fromEither(divide(nb1, nb2))
} yield result
```

We can see that the resulting type `EitherAcc[NumberFormatError :+: DivideByZero :+: CNil, Double]` has accumulated the type.
That type can be read as "A value of type Double or an error, which type will be `NumberFormatError` OR `DivideByZero`".

Using the library
-------

### Construction
There is currently 3 ways to construct an instance of `EitherAcc`:
```tut
EitherAcc.pure[DivideByZero, String]("test") // construct a Success Value

EitherAcc.err[DivideByZero, String](DivideByZero(1d)) // construct an Error Value

EitherAcc.fromEither(Right[DivideByZero, String]("test")) // construct a value depending of the Either
```

### Getting the value

When you finally have your value wrapped in an `EitherAcc`, you will want to get back your result - the error or the success value.
This operation is in fact _folding_ the `EitherAcc` and we'll need [`Poly1` from Shapeless](https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#polymorphic-function-values) to do that.

```tut
import shapeless.{Poly1, CNil, :+: }

object getErrorAsString extends Poly1 {
   implicit def caseDivideByZero = at[DivideByZero](err => s"Fail to divide ${err.nb1} by 0")
   implicit def caseFormatError = at[NumberFormatError](err => s"Fail format ${err.nonFormattableString} to number")
}

def parseAndDivide(str1: String, str2: String): EitherAcc[NumberFormatError :+: DivideByZero :+: CNil, Double] = {
    for {
      nb1 <- EitherAcc.fromEither(parse("2"))
      nb2 <- EitherAcc.fromEither(parse("3"))
      result <- EitherAcc.fromEither(divide(nb1, nb2))
    } yield result
}


```

```tut
parseAndDivide("3","5").fold(getErrorAsString, _.toString)
parseAndDivide("3","da").fold(getErrorAsString, _.toString)
parseAndDivide("3","0").fold(getErrorAsString, _.toString)
```

Monadic concern
----------------

The _left_ type of `EitherAcc` changes with a `flatMap`, and as such it cannot be a monad.
It is however possible to fix the type of the error accumulator first, through the `widen` operation.
That way, the _left_ type becomes fixed, and our `EitherAcc` becomes monadic.
 
```tut

type Errors = NumberFormatError :+: DivideByZero :+: CNil

for {
  nb1 <- EitherAcc.fromEither(parse("2")).widen[Errors]
  nb2 <- EitherAcc.fromEither(parse("3")).widen[Errors]
  result <- EitherAcc.fromEither(divide(nb1, nb2)).widen[Errors]
} yield result
``` 

Don't worry, `widen` is not a cast, it can be called only if the error type of `EitherAcc` is included in the widened type.
   
