# Compilation of patterns with implicits in Scala

## List of patterns

- [Implicit Context](patterns/implicit-context.md)
- [Type Classes](patterns/type-classes.md)
- [Implicit Conversions](patterns/implicit-conversions.md)
- [Refine Return Types](patterns/refine-return-types.md)
- [Implicit Class Extensions](patterns/implicit-class-extensions.md)
- [Aux Pattern](patterns/aux-pattern.md)

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

