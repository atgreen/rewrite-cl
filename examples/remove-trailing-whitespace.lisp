;;;; remove-trailing-whitespace.lisp
;;;; Example: Remove whitespace before closing parens in CL source

(require :asdf)
(push (truename ".") asdf:*central-registry*)
(asdf:load-system "rewrite-cl")

(defpackage #:remove-trailing-ws
  (:use #:cl #:rewrite-cl)
  (:import-from #:rewrite-cl.node
                #:whitespace-node-p
                #:newline-node-p
                #:seq-node-p
                #:comment-node-p))

(in-package #:remove-trailing-ws)

(defun trailing-whitespace-p (zipper)
  "Check if current node is whitespace/newline at end of a sequence."
  (and (or (whitespace-node-p (zip-node zipper))
           (newline-node-p (zip-node zipper)))
       ;; Parent must be a sequence (list or vector)
       (let ((parent (zip-up zipper)))
         (and parent (seq-node-p (zip-node parent))))
       (let ((prev (zip-left zipper))
	     (next (zip-right zipper)))
	 ;; Normally, we want to delete whitespace/newlines at the end.
	 ;; However, if there's a comment followed only by whitespace/newlines,
	 ;; we want to delete all _except_ the last one, so the indentation on
	 ;; the containing close paren doesn't change.
	 (if (and prev (newline-node-p (zip-node prev))
		  (zip-left prev)
		  (comment-node-p (zip-node (zip-left prev))))
	     ;; Comment case: we're on the first node after the newline after
	     ;; a comment.  If we're not at the end, scan forward to see whether
	     ;; we hit another non-whitespace/newline node.
	     (and next
		  (loop while (and next (or (whitespace-node-p (zip-node next))
					    (newline-node-p (zip-node next))))
		    do (setf next (zip-right next))
		    finally (return (null next))))
	   ;; Normal case: delete if it's the last one
	   (null next)))))

(defun remove-trailing-whitespace (source)
  "Remove whitespace before closing parens in SOURCE string."
  (let ((z (of-string source)))
    (when z
      ;; Walk the tree, removing trailing whitespace in sequences
      (loop with z = (of-string source)
            with changed = t
            while changed
            do (setf changed nil)
               (setf z (zip-prewalk
                        z
                        (lambda (zz)
                          (if (trailing-whitespace-p zz)
                              (progn
				(setf changed t)
                                (zip-remove zz))
                              zz))))
            finally (return (zip-root-string z))))))

;;; Demo
(defun demo ()
  (format t "~%=== Remove Trailing Whitespace Demo ===~%~%")

  (let ((examples
          '(;; Empty case 1
            "( )"

            ;; Empty case 2
            "(
)"

            ;; Empty case 3
            "(
  )"

            ;; Simple case
            "(defun foo (x)
  (+ x 1   ))"

            ;; Multiple levels
            "(let ((a 1  )
      (b 2  ))
  (+ a b ))"

            ;; Mixed - whitespace in strings should be preserved
            "(format t \"hello world   \"   )"

            ;; Comments should be preserved
            "(defun bar ()
  ;; comment with trailing spaces
  (+ 1 2  ))"

            ;; A comment before the close paren should retain its terminal
	    ;; newline, along with the indentation of the close paren
            "(defun foo (x)
  (+ x 1   )
  ;; comment

  )"

            ;; The same case, but with whitespace on the blank line
            "(defun foo (x)
  (+ x 1   )
  ;; comment (whitespace on next line is intentional)
  
  )"

            ;; Vector
            "#(1 2 3   )"

            ;; Already clean
            "(+ 1 2)")))

    (dolist (source examples)
      (let ((result (remove-trailing-whitespace source)))
        (format t "BEFORE:~%~A~%~%" source)
        (format t "AFTER:~%~A~%~%" result)
        (format t "~A~%" (make-string 50 :initial-element #\-))))))

;; Run demo
(demo)
