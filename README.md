# BB debug

Bare bones debugger for Clojure, do not need any special IDE or editor - just only REPL.

## Overview

Two macroses, `dbg` and `dbg-all`, allows to set up single/multiple named/unnamed perpetual/conditional breakpoints in your Clojure code.

Two another macroses, `inspect` and `watch`, allows to inspect any Clojure values, even lazy ones, statically and dynamically on debug session.

`dbg-all*` macro expands multiple breakpoint forms and pretty-prints it with highlighted breakpoints.

See `example.clj` and try eval it forms in REPL.

## License

Copyright © 2019

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
