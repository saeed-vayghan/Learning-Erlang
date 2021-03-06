http://erlang.org

### Erlang:
  * Was designed for programming concurrent, real-time, distributed, and fault-tolerant systems. (built-in support)
  * General-purpose programming language and runtime environment.
  * Has built-in support for supervised processes.

### Features:
  * Fault tolerance is the property that enables a system to continue operating properly in the event of the failure of some of its components.
  * Erlang processes are very lightweight, much lighter than an operating system thread. 
  * Each Erlang process is garbage collected separately.
  * What sort of applications is Erlang particularly suitable for? Telecommunication, mail servers, Database, Banking, soft real-time concurrent systems
  * Erlang provides a simple and powerful model for error containment and fault tolerance (supervised processes).
  * Concurrency and message passing are a fundamental to the language.
  * Applications written in Erlang are often composed of hundreds or thousands of lightweight processes.
  * Context switching between Erlang processes is typically one or two orders of magnitude cheaper than switching between threads in a C program.
  * Erlang's distribution mechanisms are transparent: programs need not be aware that they are distributed.
  * The OTP libraries provide support for many common problems.
  * The Erlang runtime environment means that code compiled on one architecture and runs anywhere.
  * The runtime system also allows code in a running system to be updated without interrupting the program. 
  * Erlang is less suitable for: drivers, Image/signal processing
  * Erlang has two main paradigms: functional and concurrent.
  * Let it crash concept is in the concurrent part of the language


#### Erlang is about programming for the right cases:
  * You only program for what you know will happen and what you expect.
  * Everything else should cause errors as soon as possible.

#### OTP (Open Telecom Platform) is a large collection of libraries for Erlang to do everything

#### Erlang shell:
  * The Erlang shell works by reading an Erlang expression, evaluating it, printing the result and looping for another expression, i.e. a REPL shell. 

#### The use of a pattern matching syntax, and the `single assignment' property of Erlang variables, leads to clear, short and reliable programs.

#### The module system allows the structuring of very large programs into conceptually manageable units.

#### Continuous operation, Code loading primitives allow code in a running system to be changed without stopping the system.
(Very important in embedded real-time systems, telephone exchanges or airtraffic control systems {such systems should not be stopped for software maintenance purposes})

#### Automatically memory allocation and real-time garbage collector. Typical programming errors associated with memory management cannot occur.

#### Integration, Erlangcan easily call or make use of programs written in other programming languages. 

#### An application written for a uniprocessor can easily be changed to run on a multiprocessoror network of uniprocessors.

<hr>

### Definitions:

  1. What does soft realtime mean? "basically nothing"!

  A hard realtime system is one which can guarantee that a certain action will always be carried out in less than a certain time.
  Many telecomms systems have less strict requirements, for instance they might require a statistical guarantee along the lines
  of "a database lookup takes less than 20ms in 97% of cases". Soft realtime systems, such as Erlang, let you make that sort of guarantee. 


  2. RTE - Runtime environment? Essentially, everything that happens in the context of running an erl app.

  As soon as a software program is executed, it is in a run-time state.
  In this state, the program can send instructions to the computer's processor and access the computer's memory (RAM) and other system resources.
  RTE supports the execution of a program/process. A program, for being able to execute, requires runtime environment.
  Runtime environment provides following services to the program/process:
    * Resident Memory
    * Resources such as File, Sockets, etc.
    * Environment variables
    * Proper initialization
    * Proper disposal

<hr>

### Type system

  * Erlang is dynamically typed: every error is caught at runtime and the compiler won't always yell at you when compiling modules where things may result in failure.
  * Dynamically typed rather means that the type of an object is determined at runtime, instead of at compile time.
  * For example, the compiler will not have any complaints about this code:
    ```
    foo() ->
      X = {a,b,c},
      [H|T] = X.
    ```

    but if you run it, it will cause a run-time error, since the head-tail pattern cannot be used on a tuple.
    In a statically typed language, this program wouldn't compile.

To read more: [imperative-vs-declarative-programming-procedural-functional-and-oop](https://zach-gollwitzer.medium.com/imperative-vs-declarative-programming-procedural-functional-and-oop-b03a53ba745c)
