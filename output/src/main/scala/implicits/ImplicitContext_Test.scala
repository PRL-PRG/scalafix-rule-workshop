package implicits

object ImplicitContext_Test {
    case class Employee(name: String)

    // Define a single context
    implicit val context = Employee("Richard")

    // Usage
    def getEmployee(id: Int)(implicit e: Employee) : String = {s"${id}: ${e.name}"}

    getEmployee(12)
}