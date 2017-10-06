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