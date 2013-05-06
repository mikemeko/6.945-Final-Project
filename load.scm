;;; This loader constructs a new user-initial-environment.

(set! user-initial-environment
      (extend-top-level-environment system-global-environment))
(ge user-initial-environment)

(load "utils.scm")
(load "io.scm")
(load "xml-node.scm")
(load "interact.scm")
(load "parser.scm")
(load "xml-parser.scm")
(load "tokenizer.scm")
(load "xml-tokenizer.scm")
(load "xml-traverse.scm")

(load "test-traverse.scm")
(load "test-io.scm")

