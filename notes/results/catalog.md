# Implicits Grimoire

## Conversion

### Ubiquitous conversions 

#### ArrowAssoc

- 20% of all conversions, 6% of all call sites with implicits
- Used to provide alternative syntax for tuple construction (`A -> B` instead of `(A, B)`).

```
/** @group implicit-classes-any */
implicit final class ArrowAssoc[A](private val self: A) extends AnyVal {
    @inline def -> [B](y: B): Tuple2[A, B] = Tuple2(self, y)
    def →[B](y: B): Tuple2[A, B] = ->(y)
}
```

- example: `json4s`

This style (`->`) is favored by `json4` to make their json syntax nicer, when combined with type inference and their special operator `~`:

```scala
// From http://json4s.org/#example
val json =
    ("lotto" ->
        ("lotto-id" -> lotto.id) ~
        ("winning-numbers" -> lotto.winningNumbers) ~
        ("draw-date" -> lotto.drawDate.map(_.toString)) ~
        ("winners" ->
            lotto.winners.map { w =>
                (("winner-id" -> w.id) ~
                ("numbers" -> w.numbers))}))
```

#### org.scalatest.WordSpecLike.convertToWordSpecStringWrapper

- Occurs in 10 projects (about 20%).
- Used to augment strings to give them `scalatest` methods (`when`, `in`...), thus allowing BDD-style testing.

- Example: `elastic4s`
```scala
// The wrapper is inserted at (1), and augments String with `in`
"publish all data from the index" /* 1 */ in {
    // ...
}
// From https://github.com/sksamuel/elastic4s/blob/c3bb17504a2d0d902e02c2a57b1873181f824e22/elastic4s-http-streams/src/test/scala/com/sksamuel/elastic4s/streams/ScrollPublisherIntegrationTest.scala
```

- It can be seen as a DSL-enabler, it's part of the DSL of scalatest.

### DSLs

#### DSLs with macros

##### Quill

Quill defines a DSL to interact with (among others) SQL databases as if they were Scala collections.
It wraps types in `quote`s, with macros:

```scala
private[dsl] trait QuotationDsl {
    // ...
    implicit def quote[T](body: T): Quoted[T] = macro QuotationMacro.quote[T]

    // `io.getquill.dsl.QuotationDsl.unquote`
    @compileTimeOnly(NonQuotedException.message)
    implicit def unquote[T](quoted: Quoted[T]): T = NonQuotedException()
}
// From https://github.com/getquill/quill/blob/master/quill-core/src/main/scala/io/getquill/dsl/QuotationDsl.scala#L31
```

In particular, the implicit `unquote` above accounts for just over 49% of all call implcit conversions in `quill`.
We have not found other uses of `quote` or `unquote` outside of the project. But the use of `unquote` is primarily intended to be client code, so its appearances in `quill` are mostly due to tests:

```scala
// Every quote comes with an implicit unquote so that q has the right type
val q = quote {
    e.map(t => t).filter(t => t.i == 1)
}
```

##### Scala reflection

Scala reflection offers a similar mechanism, `Quasiquotes`, to extract the representation of a piece of code. Used in the implementation of macros, to allow macros to easily insert pieces of AST into their result:

```scala
opt match {
    case Some(_) => q"document.getAs($pname)($reader)"
    case _       => q"document.getAsTry($pname)($reader).get"
}
// From https://github.com/ReactiveMongo/ReactiveMongo/blob/51d9d4068f868e9d38e1c72a2cd4ea4eccb4e119/macros/src/main/scala-2.11/MacroImpl.scala#L191
```


#### Matchers for testing DSLs

Implicit conversions are vital to provide a useful interface for testing frameworks that want their tests to look like natural language. 
Examples include the aforementioned `convertToWordSpecStringWrapper` from `scalatest` and the matchers in `specs2`.
The most popular of `specs2`'s matchers is `theValue`, which is applied to the right hand side of a `must` expression in `specs2`.

```scala
// DECLARATION from etorreborre/specs2
private[specs2]
trait MustExpectations1 extends MustExpectationsCreation {
  implicit def theValue[T](t: => T): MustExpectable[T] = createMustExpectable(t)
}

// EXAMPLE USAGE from ReactiveMongo/ReactiveMongo
bsonStr.byteSize must_== byteSize and {
    written(bsonStr) must_== byteSize
}
```

As a reference, here is the table of the conversions that (1) are obviously related to testing and (2) appear more than 100 times in the 48-project sample.

| Call Sites | Function                                              |                                                                 
|----------|-------------------------------| 
| 4052 |   org.scalatest.WordSpecLike.convertToWordSpecStringWrapper     |       
| 2739 |  org.specs2.matcher.MustExpectations1.theValue                 |     
| 1985 | org.specs2.specification.dsl.mutable.ExampleDsl1.blockExample |                                                                   
| 714 |   utest.TestableSymbol                                                | 
| 252 |   org.specs2.matcher.MatchResultLogicalCombinators.combineMatchResult | 
| 249 |    org.specs2.specification.dsl.mutable.BlockDsl.describe              |
| 175 | org.specs2.matcher.DataTables.toDataRow                       |  
| 152 |    org.specs2.matcher.MustThrownExpectables.akaMust                    | 
| 152 |    org.specs2.matcher.ExpectationsDescription.describe                 | 
| 113 |    org.specs2.specification.dsl.FragmentsDsl.appendToFragments         | 
| 104 |  org.specs2.matcher.ValueCheck.typedValueCheck                       | 
| 10687 | TOTAL (18.6% of all conversions) |

### Tricking the type system

#### Casting Some[T] to Option[T]

**sksamual/exts** (used 557 times in `elastic4s`)

```scala
  /**
    * Better than Some(t) because that will return the inferred type as Some[T], but in a fold we probably want the
    * type inferred as Option[T]
    */
  implicit class RichOptionImplicits[T](t: T) {
    def some: Option[T] = Some(t)
  }
```

The most frequent use is the following:

```scala
case class FlushIndexDefinition(indexes: Seq[String],
                                waitIfOngoing: Option[Boolean] = None,
                                force: Option[Boolean] = None) {
  def force(force: Boolean): FlushIndexDefinition = copy(force = force.some) // HERE
  def waitIfOngoing(waitIfOngoing: Boolean): FlushIndexDefinition = copy(waitIfOngoing = waitIfOngoing.some) // AND HERE
}
```

Here, they want to declare the parameters as Option[T], so that they can default them to None, and thus they remain optional without having to establish a convention for every possible default argument T (e.g. "-1 always means empty", or "The empty string means empty"). Then, when somebody uses the methods to force one of the fields, (e.g. `force` above), they use the Option wrapper to cast it to Option[T] instad of Some[T]. This `copy(field = field.some)` pattern is all over the codebase in `elastic4s`.

#### Akka: ScalaActorRef <-> ActorRef

An instance of API extension, `actorRef2Scala` is applied when we want to use the `!` method defined in the Scala actor API:

```scala
/**
 * This trait represents the Scala Actor API
 * There are implicit conversions in package.scala
 * from ActorRef -&gt; ScalaActorRef and back
 */
trait ScalaActorRef { ref: ActorRef ⇒
  // ...
  def !(message: Any)(implicit sender: ActorRef = Actor.noSender): Unit
}
```

The conversions are defined in a `package.scala` file, so as to be invisible to the user:

```scala
package object actor {
  implicit def actorRef2Scala(ref: ActorRef): ScalaActorRef = ref.asInstanceOf[ScalaActorRef]
  implicit def scala2ActorRef(ref: ScalaActorRef): ActorRef = ref.asInstanceOf[ActorRef]
}
```






