;;;; api-tests.lisp - Tests for the public API surface
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl/test)

(def-suite api-tests
  :description "Tests for the documented public API"
  :in rewrite-cl-tests)

(in-suite api-tests)

;;; The README's API list is a promise: every name it advertises should be
;;; usable from a package that does (:use #:cl #:rewrite-cl).  Twice now that
;;; promise has been broken by a function living only in an internal package --
;;; first the batch fixed in "Add missing exports", then zip-transform and
;;; zip-transform-if.  This test closes the loop so the next one is caught here
;;; rather than by a user.

(defun readme-api-names ()
  "Names in backticks on the README's API-list bullet lines.
Returns NIL if the README cannot be found.  Keywords such as `:list` are tag
names rather than bindings, so they are skipped."
  (let ((path (asdf:system-relative-pathname "rewrite-cl" "README.md")))
    (when (probe-file path)
      (with-open-file (stream path)
        (let ((names '()))
          (loop for line = (read-line stream nil)
                while line
                when (and (> (length line) 3)
                          (string= "- `" (subseq line 0 3)))
                  do (loop with start = 0
                           for open = (position #\` line :start start)
                           while open
                           for close = (position #\` line :start (1+ open))
                           while close
                           do (let ((name (subseq line (1+ open) close)))
                                (unless (or (zerop (length name))
                                            (char= #\: (char name 0)))
                                  (pushnew name names :test #'string=)))
                              (setf start (1+ close))))
          (nreverse names))))))

(test readme-api-names-are-exported
  "Every name the README advertises is external in #:rewrite-cl."
  (let ((names (readme-api-names)))
    (if (null names)
        (skip "README.md not found next to the system definition")
        (progn
          ;; Guard against the extractor silently matching nothing.
          (is (< 20 (length names))
              "Expected the README to advertise many names, found ~a"
              (length names))
          (dolist (name names)
            (multiple-value-bind (symbol status)
                (find-symbol (string-upcase name) :rewrite-cl)
              (declare (ignore symbol))
              (is (eq :external status)
                  "README advertises ~s but in #:rewrite-cl it is ~(~a~)"
                  name (or status "absent"))))))))

(test documented-walks-are-callable
  "The walk entry points are fbound, not merely interned."
  (dolist (name '("zip-prewalk" "zip-postwalk" "zip-prewalk-while"
                  "zip-transform" "zip-transform-if"))
    (let ((symbol (find-symbol (string-upcase name) :rewrite-cl)))
      (is (and symbol (fboundp symbol))
          "~s is not fbound in #:rewrite-cl" name))))
