
* In Erlang, all variables must start with a capital letter, so you can write A = 3. but not a = 3.
* Erlang only lets you assign a variable once in a function. (single assignment).
* Pattern matching is used for assigning values to variables and for controlling the flow of a program.

* The special variable underscore (written '_') is the anonymous or don't care variable.
* It is used as a place holder where the syntax requires a variable, but the value of the variable is of no interest.

* A variable whose value has been assigned is said to be bound otherwise it is said to be unbound.
* Bounded variables are called bind once or single assignment.

* Pattern matching occurs:
  * when evaluating an expression of the form Lhs = Rhs
  * when calling a function
  * when matching a pattern in a case or receive primitive.