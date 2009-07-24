Introduction

The deoxybyte-io system is a selection of utility code focused on
transfer of data between Lisp and its environment. It includes:

- IO and parser conditions
- File and directory utilities
- Command line interface definition utilities
- Stream classes and methods
- Tabular text parsing
- Binary encoding and decoding


Installation

deoxybyte-io uses ASDF for system definition. Copy or symlink
deoxybyte-io.asd (and optionally deoxybyte-io-test.asd) to your
asdf:*central-registry* and load deoxybyte-io with the asdf:operate
function:

 (asdf:operate 'asdf:load-op :deoxybyte-io)

or with the equivalent deoxybyte-systems:load-system function:

 (dxs:load-system :deoxybyte-io)


Tests

To run the unit and regression tests you need to have LIFT
installed. Run the tests with the asdf:operate function:

 (asdf:operate 'asdf:test-op :deoxybyte-io)

or with the equivalent deoxybyte-systems:test-system function:

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
ieee-floats             http://common-lisp.net/project/ieee-floats/
trivial-gray-streams    http://www.cliki.net/trivial-gray-streams


Optional dependencies

LIFT                    http://common-lisp.net/project/lift/
CLDOC                   http://common-lisp.net/project/cldoc/
