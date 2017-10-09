/*
rule = "class:implicits.ImplicitContext"
*/
package implicits

object ImplicitContext_Test {
    case class Employee(name: String)
    case class Boss(name: String)

    // Define a single context
    implicit val richard = Employee("Richard")
    implicit val barbara = Boss("Barbara")

    // Usage
    def getEmployee(id: Int)(implicit e: Employee, b: Boss) : String = {s"${id}: ${e.name}, ${b.name}"}

    getEmployee(12)
}