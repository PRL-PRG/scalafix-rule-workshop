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
    class EmployeeGetter {
        def getGetter()(implicit e: Employee, b: Boss): EmployeeGetter = this
        def getGetterPlain(): EmployeeGetter = this
        def getEmployee(id: Int)(implicit e: Employee, b: Boss) : String = {s"${id}: ${e.name}, ${b.name}"}
        def getEmployeePlain(id: Int) : String = {s"${id}: Lara"}
    }
    def employeeGetter()(implicit e: Employee, b: Boss) : EmployeeGetter = new EmployeeGetter()
    def employeeGetterPlain() : EmployeeGetter = new EmployeeGetter()

    employeeGetter().getGetter().getEmployee(12)
    employeeGetterPlain().getGetterPlain().getEmployeePlain(13)
}