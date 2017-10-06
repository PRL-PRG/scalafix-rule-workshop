# Compilation of patterns with implicits in Scala

## Implicit Context

The one Heather was talking about, when a library needs a "config" or "context" object and it would otherwise need to be passed to every function call.

Links:
	- [Liao Yi post](http://www.lihaoyi.com/post/ImplicitDesignPatternsinScala.html#implicit-contexts)
	
	From the blog, we get the quote:

	>  Implicit Contexts usually have some properties that make them distinct from many other uses of implicits:

    	- The implicit parameter usually is not generic type, and does not have any type parameters

    	- The same implicit is being passed to all sorts of different functions with different signatures

     	- Different values of the implicit will be passed into the same function when called at different times, e.g. every Play Framework HTTP request gets a new Request value that gets passed around implicitly

    	- The implicit value might even be mutable! This is certainly the case for Akka's ActorSystems, which encapsulate a large pool of Actors and the ability to spawn new ones or send them messages.



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

## Comparison: Type classes vs Implicit Contexts

These are some differences that may help when trying to figure out patterns.

- Implicit Contexts are used to pass DATA around. They allow several instances of the same (a single) type. Thus:

	- Usually mutable
	- Usually not many functions
	- Usually different values injected each time
	- Full of data
	- They do not have type parameters.

- Type classes are used to provide FUNCTIONALITY (via a static common interface, or trait). They are composed of a type parameter, a trait and single instances of different types that fulfill that type parameter. Thus:

	- Don't really have any data.
	- They usually have pure functions (and usually only one).
	- They are tied to type parameters in their usage.
	- They are tied to a trait.

## Deriving implicits

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

## Extending type classes from the user side

Type classes can be extended in the user side, just providing some `implicit object` that implements the desired trait with the correct type parameter.

```scala
// (from user side)
implicit object FileJsonable extends Jsonable[java.io.File]{
    def serialize(t: java.io.File) = Json.Str(t.getAbsolutePath)
 }
```

THat leaves type classes open for extension and closed for modification, which is great.

## Implicit conversions

If we want to provide something similar to implicit conversions (or other similar trivial but type-manipulating operations) manually, we can do it with implicits.

To implement this pattern you need:
- A function, which takes an implicit parameter with the functionality we want to implement (if we want a number adder, then we pass an `Adder`), and calls the functions inside that type.
- A class (or trait I guess) `Adder` with the appropriate type parameters and a declaration of the conversion function we want (say, `class Adder[A, B]{val add: A, B => A})
- Some implementation object(s), that extend the base class with the desired functionality.

Different from type classes because we want just the conversion function that will get called and implicitly filled with the required type parameters. The conversion function itself is not part of a trait type.

This is used to **control how types are inferred**, since implicits are solved statically and can influence the type checker.

### Examples

**Taken from `airbnb/aerosolve`**

In file `com/airbnb/aerosolve/training/AdditiveModelTrainer.scala`

Creating an implicit conversion from a Scala type to their own function.

```scala
implicit def valueToPlanetVal(x: Value): LossFunction = x.asInstanceOf[LossFunction]
```

## Aux pattern

[Link](http://gigiigig.github.io/posts/2015/09/13/aux-pattern.html)


Used to extract type information of path-dependent implicit parameters. Basically, when we want to use the path-dependent type of a parameter that came earlier in the list of parameters.

Creates an alternative relation between the dependent types (`type Aux[A0, B0] = Foo[A0] { type B = B0  }`), so that we can refer to it from further down the parameter list (`def foo[T, R](t: T)(implicit f: Foo.Aux[T, R], m: Monoid[R]): R = m.zero `)

Not sure what this has to do with implicits though, because I think that even without them the problem can appear (extract from the REPL):

```scala
trait Foo { 
	type B
	def value: B
}
object StrFoo extends Foo {
	type B = String
	val value = "Hello"
}

def foo(f: Foo): f.B = f.value
/* 	
Running `val r1 = foo(StrFoo)` 
now prints "Hello"
*/

trait DependentFoo[T]{def dependentValue: T}

// Here it complains that we cannot access f.B in the parameter list, as expected
def wrongFoo(f: Foo, dep: DependentFoo[f.B]): f.B = dep.dependentValue

// This is the Aux pattern in action
type FooAux[B0] = Foo { type B = B0 }

// It works! We only need a valid DependentFoo instance
def rightFoo[B0](f: FooAux[B0], dep: DependentFoo[B0]): B0 = dep.dependentValue

```

## Implicit classes to extend functionality

The simplest use of implicit classes, to extend the functionality of an existing class without modifying it:

```scala
object ImplicitClasses {
	implicit class Hello(s: String) { def hello = s"Hello, $s" }
    def test = {
    	// Here, it assumes that "World" is of class Hello.
    	println( "World".hello )
    }
}

scala> ImplicitClasses.test
Hello, World
```

### Examples

**Taken from `airbnb/aerosolve`**

In file `com/airbnb/aerosolve/training/pipeline/PipelineTestingUtil.scala`

Rather unexciting use of implicit classes to extend the functionality of the tuples.

```scala
  implicit class Tupple4Add(t: (Long, Long, Long, Long)) {
    def +(p: (Long, Long, Long, Long)) = (p._1 + t._1, p._2 + t._2, p._3 + t._3, p._4 + t._4)
  }

  implicit class Tupple2Add(t: (Long, Long)) {
    def +(p: (Long, Long)) = (p._1 + t._1, p._2 + t._2)
  }
```

## Refine return types on the fly

**Taken from `airbnb/aerosolve`**

Here, I believe they are just passing the ClassTag[T] around 

File `/airlearner-utils/src/main/scala/com/airbnb/common/ml/util/HiveUtil.scala`:

They have only one caller each, shown below. Note how the callers refine T in the return types.

```scala
  def loadDataFromHive[T](
      hiveContext: HiveContext,
      dataQuery: String,
      parseKeyFromHiveRow: (Row) => String,
      parseSampleFromHiveRow: (Row) => T
  )(implicit c: ClassTag[T]):
  RDD[(String, T)] = {
    loadDataFromDataFrame(hiveContext.sql(dataQuery), parseKeyFromHiveRow, parseSampleFromHiveRow)
  }

  def loadDataFromDataFrame[T](
      data: DataFrame,
      parseKeyFromHiveRow: (Row) => String,
      parseSampleFromHiveRow: (Row) => T
  )
    (implicit c: ClassTag[T]): RDD[(String, T)] = {
    data.map(row => {
      val key = parseKeyFromHiveRow(row)
      val t = parseSampleFromHiveRow(row)
      (key, t)
    })
	}
```

File `com/airbnb/common/ml/xgboost/data/ModelData.scala`:

Here it calls the above two functions, and I guess ClassTag is used to specify the type parameters for the return types.
```scala
 def getScoringLabeledPoints(sc: SparkContext,
                              query: String, scoringLabeledPoint: ScoringModelData): RDD[(String, ScoringLabeledPoint)] = {
    val df = ModelData.getDataFrame(sc, query)
    HiveUtil.loadDataFromDataFrame(
      df,
      // score_query_head of scoring.conf also defined S_node_10k_id same as TRAINING_KEY_INDEX
      ModelData.parseKeyFromHiveRow(ModelData.TRAINING_KEY_INDEX),
      scoringLabeledPoint.parseRowToXgboostLabeledPointAndData)
  }

  def getLabeledPointsAndString(sc: SparkContext,
                                query: String, scoringLabeledPoint: ScoringModelData): RDD[(String, Seq[ScoringLabeledPoint])] = {
    val df = ModelData.getDataFrame(sc, query)
    HiveUtil.loadDataFromDataFrameGroupByKey(
      df,
      // score_query_head of scoring.conf also defined S_node_10k_id same as TRAINING_KEY_INDEX
      ModelData.parseKeyFromHiveRow(ModelData.TRAINING_KEY_INDEX),
      scoringLabeledPoint.parseRowToXgboostLabeledPointAndData)
  }
```

**NOTE:** There is another example of just this in the file `com/airbnb/common/ml/strategy/trainer/BinaryRegressionTrainer.scala` of the same project
