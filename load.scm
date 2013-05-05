;;; This loader constructs a new user-initial-environment.

(set! user-initial-environment
      (extend-top-level-environment system-global-environment))
(ge user-initial-environment)

(load "utils.scm")
(load "io.scm")
(load "node.scm")
(load "structure.scm")
(load "structure-extensions.scm")
(load "parser.scm")
(load "tokenizer.scm")
(load "xml-tokenizer.scm")
(load "traverse.scm")

(load "test-traverse.scm")
(load "test-io.scm")
