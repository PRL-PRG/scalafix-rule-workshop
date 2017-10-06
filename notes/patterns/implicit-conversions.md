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