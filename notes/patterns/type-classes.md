
## Type Classes

Uses implicits to provide (usually `object`) implementations of generic type parameters, thus allowing us to create type classes. It's mentioned in most blog posts about implicits in Scala.

You need:

- To declare the trait you want your type class to satisfy

```scala
trait Jsonable[T]{
  def serialize(t: T): Json
}
```

- To declare implicit objects (or I guess classes with implicit `val` instances work too) that provide implementations of the desired valid type parameters:

```scala
object Jsonable{
  implicit object StringJsonable extends Jsonable[String]{
    def serialize(t: String) = Json.Str(t)
  }
  implicit object DoubleJsonable extends Jsonable[Double]{
    def serialize(t: Double) = Json.Num(t)
  }
  implicit object IntJsonable extends Jsonable[Int]{
    def serialize(t: Int) = Json.Num(t.toDouble)
  }
}
```

Now we can use the defined type class with an implicit parameter (of the usual generic type) that will take any of the objects above, depending on the usage of the type class and what type it needs:

```scala
def convertToJson[T](x: T)(implicit converter: Jsonable[T]): Json = {
	
  	converter // Here the generic type parameter is resolved to the correct `implicit object`, since we know the type of x.
  		.serialize(x)
}
```

We can also find it with the shorthand syntax:

```scala
def convertToJson[T: Jsonable](x: T): Json = {
  implicitly[Jsonable[T]].serialize(x)
}
```

### Method overloading

It uses the same mechanism as above, to provide static method overloading. Where we could overload a method with concrete types:

```scala
def convertToJson(t: String) = Json.Str(t)
def convertToJson(t: Double) = Json.Num(t)
def convertToJson(t: Int) = Json.Num(t.toDouble)
```

It becomes hard when we need to use that method inside others, resulting in a exponential explosion of methods.

Instead, we can define the overloaded method in terms of the type class defined earlier, and have all the methods that use it also take the type class:

```scala
def convertToJson[T: Jsonable](x: T): Json = {
  implicitly[Jsonable[T]].serialize(x)
}
def convertToJsonAndPrint[T: Jsonable](x: T) = println(convertToJson(x))
def convertMultipleItemsToJson[T: Jsonable](t: Array[T]) = t.map(convertToJson(_))
```

### Deriving implicits

We can use the type class within itself to provide recursive definitions:

```scala
object Jsonable {
	implicit def SeqJsonable[T: Jsonable]: Jsonable[Seq[T]] = new Jsonable[Seq[T]]{
    def serialize(t: Seq[T]) = {
      Json.List(t.map(implicitly[Jsonable[T]].serialize):_*)
    }
}
```

Here we have a `serialize` method that accepts a Seq of Jsonables, which can itself be a Seq[Jsonable].


### Extending type classes from the user side

Type classes can be extended in the user side, just providing some `implicit object` that implements the desired trait with the correct type parameter.

```scala
// (from user side)
implicit object FileJsonable extends Jsonable[java.io.File]{
    def serialize(t: java.io.File) = Json.Str(t.getAbsolutePath)
 }
```

THat leaves type classes open for extension and closed for modification, which is great.

### Examples

**Taken from `airbnb/aerosolve`**

#### In file `com/airbnb/aerosolve/training/utils/Sort.scala`

Note how an implicit evidence parameter is necessary, to prove that A is orderable. In this case, I guess the ordering function is defined implicit elsewhere.
I'm not sure whether this constitutes an example of the Type Class Pattern though. It's basically the same as the blogs' examples, except the evidence function is out there in the wild and not in an `Orderable` trait or something like that. 

In this case, the only use of Sort inside the library is in a test case with an Array (which I guess defines the evidence function). Then it's clear that this function is a case of a library that wants to be open for extension.

```scala
  @tailrec
  def quickSelect[A](
      seq: Seq[A], n: Int, rand: Random = new Random)(implicit evidence: A => Ordered[A]): A = {
    assert(n < seq.length, s"n $n cannot be larger than length of sequence ${seq.length}")
    val pivot = rand.nextInt(seq.length)
    val (left, right) = seq.partition(seq(pivot).>)
    if (left.length == n) {
      seq(pivot)
    } else if (left.isEmpty) {
      val (left, right) = seq.partition(seq(pivot).==)
      if (left.length > n) {
        seq(pivot)
      } else {
        quickSelect(right, n - left.length, rand)
      }
    } else if (left.length < n) {
      quickSelect(right, n - left.length, rand)
    } else {
      quickSelect(left, n, rand)
    }
  }
```

#### In file `com/airbnb/aerosolve/training/utils/JsonParser.scala`

An example of client-side typeclass implicits.

```scala
def parseJson[T](json: String)(implicit m : Manifest[T]): T = {
   mapper.readValue[T](json)
}
```

In this case, readValue in the mapper object (of class ScalaObjectMapper, from [Jackson Module Scala](https://mvnrepository.com/artifact/com.fasterxml.jackson.module/jackson-module-scala_2.10)) requires the Manifest implicit parameter, and is used as a typeclass in the library (shown below):

```scala
def readValue[T: Manifest](jp: JsonParser): T = {
	readValue(jp, constructType[T])
}
```
