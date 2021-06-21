

tr_app.erl

Every active application needs one module that implements the application behav- iour.
This module provides the startup logic for the system. At a minimum, it provides the point from which the root supervisor is started;
that supervisor is the grandparent of all the processes that will be part of the application.


tr_sup.erl

You create supervisors by writing modules that implement the supervisor behaviour.
