### Applications
  * Applications are the way you package related modules in Erlang.
  * Supervisors are one of the most important features of OTP. They monitor other processes and take action if anything goes wrong.

<hr>

#### The Structure Of An OTP Application Directories

* `/doc`
  Documentation. If you generate documentation from EDoc, you put your overview.edoc file here

* `/ebin`
  Compiled code (the .beam files). It’s also the location of the .app file, which contains the application metadata.
  * `/ebin/<application-name>.app` application metadata

* `/include`
  Public header files.

* `/priv`
  Odd bits that need to be distributed along with your application.

* `/src`
  Source code related to your application. That means your Erlang .erl files and internal .hrl files
  * `src/<app-name>_app.erl`
    Every active application needs one module that implements the application behaviour.
    This module provides the startup logic for the system. At a minimum, it provides the point from which the root supervisor is started.

  * `src/<app-name>_sup.erl`
    root supervisor

<hr>

#### The supervisor restart strategy reflects the relationship between processes under a supervisor:
 
* `one_for_one` and `simple_one_for_one` are used for processes that are not dependentupon each other directly, although their failures will collectively be counted towardstotal application shutdown.

* `rest_for_onewill` be used to represent processes that depend on each other in alinear manner.

* `one_for_allis` used for processes that entirely depend on each other
