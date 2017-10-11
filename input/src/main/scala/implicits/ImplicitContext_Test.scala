/*
rule = "class:implicits.ImplicitContext"
*/
package implicits

object ImplicitContext_Test {
    import aux.EmployeeDefinitions._
    case class Boss(name: String)

    // Define a single context
    implicit val barbara = Boss("Barbara")
    // Usage
    def getEmployee(id: Int)(implicit e: Employee, b: Boss) : String = {s"${id}: ${e.name}, ${b.name}"}

    getEmployee(12)
    getEmployee(13)
}