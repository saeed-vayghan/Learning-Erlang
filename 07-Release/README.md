#### A release consists of a set of applications together with some metadata specifying how to start and manage those applications as a system.

* A release describes a running Erlang runtime system.
* A release has a version.
* A release specifies which versions of required applications are required.
* Installing a release on a host machine produces a target system.

### Using Rebar3
You'll usually have a file named `relx.config` or a `relx` tuple inside `rebar.config` file, which will state which top-level applications are part of the release and some options regarding their packaging.

### Common `release` strcture:
```
./lib/
  _build/
  
  apps/
    myapp1/
      src/
    myapp2/
      src/
  
  doc/
  LICENSE.txt
  README.md
  rebar.config
  rebar.lock
```

<hr>

#### Through Classic way (systools, reltool):

* release file with the extension `.rel` containing the metadata for each release.  
* `.script` file contains a full specification of what will be included in applications
* The `.boot` file is a binary representation of the .script file that will be read by the Erlang runtime system when it boots.


#### Steps:

* Create `.rel` file.

* On a terminal session:
  ```
  erl -pa path-to/lib/simple_cache/ebin

  systools:make_script("simple_cache", [local]).
  %% Running this results in the generation of two files in your current directory: .script and .boot files.

  %% Omit local for release process.
  systools:make_script("simple_cache", []).
  ```
* create `.config` file to be used with your release.

* Starting a target system:
  * At this point, you have all the parts required to start the system. To do this, you need to specify two things: which .boot file to use and which .config file to use
  ```
  erl –sname cache –boot ./simple_cache –config ./sys
  %% Or
  erl –sname cache –boot ./simple_cache –config ./sys –detached
  ```

* Creating a release package:
  ```
  systools:make_tar("simple_cache", [{erts, code:root_dir()}]).
  ```
