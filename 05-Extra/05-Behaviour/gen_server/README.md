Structure:
  * `Library function` => `Associated callback function`
    Description.


#### `gen_server` library functions for implementing the API

* `gen_server:start_link/4` => `Module:init/1`
  Starts a gen_server container process and simultaneously links to it.

* `gen_server:call/2` => `Module:handle_call/3`
  Sends a synchronous message to a gen_server process and waits for a reply.

* `gen_server:cast/2` => `Module:handle_cast/2`
  Sends an asynchronous message to a gen_server process.

* `gen_server:cast/2` => `Module:handle_info/2`
  Handles messages sent to a gen_server container that were not sent using one of the call or cast functions. This is for out-of-band messages.

<br>
<hr>
<br>


#### Tip: Messages are delivered to the mailbox of a process and stay there until the process extracts them. There is no size limit on the mailbox.

<br>

#### Timeout
When a gen_server has set a timeout, and that timeout triggers, an out-of-band mes- sage with the single atom timeout is generated,
and the handle_info/2 callback is invoked to handle it.
This mechanism is usually used to make servers wake up and take some action if they have received no requests within the timeout period.