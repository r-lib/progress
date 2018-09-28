
## Architecture

Since version 2.0.0, progress has a client side architecture. Clients
report progress or status, and the server shows it on the screen. Clients
communicate via conditions.

## Subprocesses

To allow progress bars for subprocesses, progress conditions can include
the process id (pid) of the process, as the `pid` member. (If no process
id is included, the current process can be assumed.) The server may
optionally use the pid information for a better display.

## Detecting termination

It is the client's responsibility to detect and signal the termination of
the progess bar. This can usually be achieved:

* using a finalizer on a progress bar object (e.g. from the old API),
* using `on.exit()` in a mapping function
* detecting termination of a subprocess in an event loop (if the progress
bar is from a subprocess). There is also a `complete_process` message,
which should trigger the completion of all progress bars that belong to a
process.

