bash-config
===========

This package allows Bash configuration files to be parsed and interpreted
from Haskell, without executing any commands. Many shell scripts use
these as configuration files, which are intended to be sourced for
shell variable assignments.

This library fully parses Bash scripts, but can only interpret a limited
subset of Bash. No commands are actually executed. Since this in theory
could limit the ability to interpret a configuration file, the interpreter
only returns variables it is sure are correct; all other variables are
"unknown". In practice, however, configuration scripts are simple enough
and pure enough for the library to interpret completely.

Current status
--------------
Can fully lex and parse Bash scripts.
