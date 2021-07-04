Compiled code (the .beam files). It’s also the location of the `.app` file, which contains the application metadata.


#### Explaining <application-name>.app

* .app file name: <application-name>.app

* ‍‍‍`description`: A description of your application.

* `versioning` format (`vsn`): <major>.<minor>.<patch>

* ‍‍‍`modules`: A list of all the modules in your application.

* `registerd`: it allows the OTP system to know which applications register what names

* `applications`: All the applications that need to be started before this application can start. Applications usually have dependencies. 

* `mod`: Tells the OTP system how to start your application. The value is a tuple constaining the module name along with some optional startup arguments.
If `mod` is not used, then this very app would be considered as a `library application`, if not it would be called ` regular application`.

```
{application, tcp_rpc,
 [{description, "RPC server for Erlang and OTP in action"},
  {vsn, "0.1.0"},
  {modules, [tr_app,
             tr_sup,
             tr_server]},
  {registered, [tr_sup, tr_server]},
  {applications, [kernel, stdlib]},
  {mod, {tr_app, []}}
 ]}.
```
