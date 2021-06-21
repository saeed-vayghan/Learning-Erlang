### Source code related to your application. That means your Erlang .erl files and internal .hrl files.

#### tr_app.erl

Every active application needs one module that implements the application behaviour.
This module provides the startup logic for the system.
At a minimum, it provides the point from which the root supervisor is started,
That supervisor is the grandparent of all the processes that will be part of the application.


#### tr_sup.erl

You create supervisors by writing modules that implement the supervisor behaviour.
