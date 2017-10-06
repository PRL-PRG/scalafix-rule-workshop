
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