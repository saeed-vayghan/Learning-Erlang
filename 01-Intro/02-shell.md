#### Compile a file
  ```
  c(<file-name-without-extension>).
  ```

#### Run a method
  ```
  <module_name>:<func-name>().
  ```

#### Quit the Erlang shell:
  * To shut a system down cleanly, use init:stop(). 
  * Some quick ways are evaluating halt(). or Control+\. 
  * Control+C and Control+G give you access to menus. 

#### Compile directly from linux shell
  ```
  > erl -compile hello
  > erl -noshell -s hello hello_world -s init stop
  ```

#### Clear the shell
  ```
  io:format(os:cmd(clear)).
  io:format("\ec").
  ```

#### Information about individual processes can be obtained from 
```
  erlang:process_info/1 or erlang:process_info/2: 
  erlang:process_info(self(), memory).
  ```

<hr>

#### Other useful commands:########

```
b() is used to display all the binded variables.

f() − Removes all current variable bindings.

f(x) − Removes the binding for a particular variable.

h() − Prints the history list of all the commands executed in the shell.

history(N) − Sets the number of previous commands to keep in the history list to N. The previous number is returned. The default number is 20.

e(N) − Repeats the command N, if N is positive. If it is negative, the Nth previous command is repeated (i.e., e(-1) repeats the previous command).
```