#### Principles:

1. Concurrency refers to the idea of having many actors running independently, but not necessarily all at the same time.
1. Parallelism is having actors running exactly at the same time. 
1. Each Erlang process would have its own slice of time to run.


### Erlang requirements: Scalability / reliability(Fault-tolerance)
The idea is thus to find good ways to handle errors and problems rather than trying to prevent them all.


#### Scalability
* To make it efficient, it made sense for processes to be started very quickly, to be destroyed very quickly and to be able to switch them really fast.

* Having them lightweight was mandatory to achieve this.
It was also mandatory because you didn't want to have things like process pools (a fixed amount of processes you split the work between.) Instead,
* it would be much easier to design programs that could use as many processes as they need.

* They are created, scheduled, and handled in the VM, independent of underlying operating system.
As a result, process creation time is of the order of microseconds and independent of the number of concurrently existing processes. 

* Another important aspect of scalability is to be able to bypass your hardware's limitations by adding more hardware.

* Shared memory could leave things in an inconsistent state after some crashes (especially on data shared across different nodes) and had some complications.

* Instead, processes should communicate by sending messages where all the data is copied. This would risk being slower, but safer.


#### Reliability

* The idea is thus to find good ways to handle errors and problems rather than trying to prevent them all.

* Your ideal solution in Erlang is thus to kill processes as fast as possible to avoid data corruption and transient bugs. Lightweight processes are a key element in this.

* Further error handling mechanisms are also part of the language to allow processes to monitor other processes.

* Second approach is distributing the program in different hardwares and also to prevent of being single point failure due to hardware crash.

* Well it turns out the choice of asynchronous message passing was a good design pick there too.

* Under the processes-with-asynchronous-messages model, messages are sent from one process to a second one and stored in a mailbox inside the receiving process until they are taken out to be read.

* It's important to mention that messages are sent without even checking if the receiving process exists or not.

* If you need to have a confirmation of delivery, you have to send a second message as a reply to the original process. 

* When you design the architecture of your application, you determine which process will do which jobs, and what will depend on what.

* Some processes will supervise others, some couldn't live without a twin process, etc.

<hr>

#### The Erlang VM (BEAM) runs as one OS process.

* Erlang processes are implemented entirely by the Erlang VM and have no connection to either OS processes or OS threads.

* So even if you are running an Erlang system of over one million processes it is still only one OS process and one thread per core.

* Erlang is far less efficient than C++. Erlang's big strength is scalability, not efficiency.

* It will linearly scale across multiple CPUs and, due to its programming and communications model, will very easily scale across machine clusters.

* Erlang doesn't have 'threads' it has Erlang processes. Thinking of Erlang processes as 'threads' is a major category mistake.

* Fault-tolerance and scalability are the main advantages of using Processes vs. Threads.


* Another advantage of processes is that they can crash and you can feel relatively safe in the knowledge that you can just restart them (even across network hosts).

* However, if a thread crashes, it may crash the entire process, which may bring down your entire application.

* To illustrate: If an Erlang process crashes, you will only lose that phone call, or that webrequest, etc. Not the whole application.

<hr>

#### Threads/Processes

* Erlang is a concurrent language, It meana threads are part of the programming language,
* They do not belong to the operating system. That's really what's wrong with programming languages like Java and C++.
* Threads aren't in the programming language, threads are something in the operating system – and they inherit all the problems that they have in the operating system.


#### Tip:
* Each process provides the resources needed to execute a program. 

* A thread is an entity within a process that can be scheduled for execution.
* But, Erlang uses the term "process" because it does not expose a shared-memory multiprogramming model. Calling them "threads" would imply that they have shared memory.

<hr>

#### More

* Read more about run-queue and processes-migration ??
* Read more about the best cases of Parallelism ??
* http://erlang.org/euc/08/euc_smp.pdf
* [More on Erlang processes](https://stackoverflow.com/questions/2708033/technically-why-are-processes-in-erlang-more-efficient-than-os-threads)
* [More - erlang-software-for-a-concurrent-world](https://www.infoq.com/presentations/erlang-software-for-a-concurrent-world/)