#### Intro:
This application uses the prefix `sc_ `(for Simple Cache) for all modules, except the main user API module, which is named `simple_cache`.
This is a common pattern, to have a single module acting as a `front` end that uses the same name as the `application`.


#### Application Desing: (Based on Erlang and OTP in Action Book)


* `simple_cache` The user API; the application’s face to the outside.
* `sc_app` The application behaviour implementation.
* `sc_sup` The root supervisor implementation.
* `sc_store` A module to encapsulate your key-to-pid mapping.
* `sc_element` Code for the individual processes that store the cached data.


#### Flow:

1. Compile all the module:
  ```
  erlc -o ./ebin ./src/*.erl
  % Or
  erl –pa ebin
  ```

1. Start the application:
  * `application:start(simple_cache).` => `application:start(simple_cache, temporary).`
  This means even if it terminates unexpectedly, the rest of the runtime system isn’t affected; only a crash report is generated.

  * `application:start(simple_cache, permanent).`
  Application is considered required for the target system to function. the entire `runtime system shuts down` so that everything can be restarted from scratch.

1. `{mod, {sc_app, []}}` inside the `simple_cache.app` tells OTP how to start the application.
  * `sc_app:start/2` and `sc_app:stop/1`
  * `sc_store` process will be initialized.

1. `sc_sup:start_child/2` starts a new child through `{sc_element, start_link, []}`
  * Which indicates the `module name`, `function name`, and `arguments` of the start function for the child process.
  * Gets the list `[Value, LeaseTime]` appended to the argument list `[]` before the call is made, resulting in a call to `sc_element:start_link(Value, LeaseTime)`.
  * This results in a dynamically generated supervi- sion tree.
  * It hides the details of how `starting a child process` is done.

1. `sc_store` process will take care of the local storage.

1. `simple_cache:insert/2`
  * Will create a new `sc_element` process through `sc_element:create`.

1. A new `sc_element` process will be generated through `sc_sup:start_child(Value, LeaseTime)`
