
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


## Repos to look at:

[gitbucket/gitbucket](https://github.com/gitbucket/gitbucket)
[playframework/playframework](https://github.com/playframework/playframework)
[mesos/chronos](https://github.com/mesos/chronos)
[ornicar/lila](https://github.com/ornicar/lila)
[ensime/ensime-server](https://github.com/ensime/ensime-server/)
[ThoughtWorksInc/Binding.scala](https://github.com/ThoughtWorksInc/Binding.scala/)