/*
rule = "class:implicits.ImplicitContextCSV"
*/
package aux

object EmployeeDefinitions {
  case class Employee(name: String)
  implicit val richard = Employee("Richard")
}
