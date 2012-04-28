Introduction

The deoxybyte-io system is a selection of utility code focused on
transfer of data between Lisp and its environment. It includes:

- IO and parser conditions
- File and directory utilities
- Command line interface definition utilities
- Stream classes and methods
- Tabular text parsing
- Binary encoding and decoding *

* Nathan Froyd has released nibbles
(https://github.com/froydnj/nibbles) which implements octet-vector
operations similar to those in this system, only optimized for
efficiency. I'll consider replacing my implementations with a shim to
nibbles, or calling nibbles directly.


Installation

deoxybyte-io uses ASDF for system definition. Install as described in
the ASDF documentation and then load:

 (asdf:load-system :deoxybyte-io)

Alternatively, use the equivalent deoxybyte-systems:load-system function:

 (dxs:load-system :deoxybyte-io)


Tests

To run the unit and regression tests you need to have LIFT
installed. Run the tests:

 (asdf:test-system :deoxybyte-io)

Alternatively, use the equivalent deoxybyte-systems:test-system function:

 (dxs:test-system :deoxybyte-io)


Documentation

See the Lisp docstrings, particularly the package docstrings for an
overview. HTML documentation may be generated with the command:

 (dxs:document-system :deoxybyte-io)

at the REPL, provided that CLDOC is installed.


Dependencies

deoxybyte-systems       git://github.com/keithj/deoxybyte-systems.git
deoxybyte-utilities     git://github.com/keithj/deoxybyte-utilities.git

cl-fad                  http://www.weitz.de/cl-fad/
getopt                  git://git.b9.com/getopt.git

Optional dependencies

LIFT                    http://common-lisp.net/project/lift/
CLDOC                   http://common-lisp.net/project/cldoc/
