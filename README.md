# BB debug

Bare bones debugger for Clojure, do not need any special IDE or editor - just only REPL.

![alt text](https://user-images.githubusercontent.com/10473034/55596814-41d6a980-5753-11e9-86cd-a07e659b8757.png "Debug in terminal")

![alt text](https://user-images.githubusercontent.com/10473034/55596818-47cc8a80-5753-11e9-93fe-64a458c90767.png "Debug in editor")

## Overview

Two macroses, `dbg` and `dbg-all`, allows to set up single/multiple named/unnamed perpetual/conditional breakpoints in your Clojure code.

Two another macroses, `inspect` and `watch`, allows to inspect any Clojure values, even lazy ones, statically and dynamically on debug session.

`dbg-all*` macro expands multiple breakpoint forms and pretty-prints it with highlighted breakpoints.

`last-context` function jumps you into nested REPL session with last debug context loaded and allows to inspect its local state. It especially useful after debug session breaking cause of exception.

See `example.clj` and try eval it forms in REPL.

## License

Copyright © 2019

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
