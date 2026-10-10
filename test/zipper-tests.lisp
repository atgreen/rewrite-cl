;;;; zipper-tests.lisp - Tests for zipper operations
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl/test)

(def-suite zipper-tests
  :description "Tests for zipper operations"
  :in rewrite-cl-tests)

(in-suite zipper-tests)

;;; Basic creation

(test zipper-of-string
  "Test creating zipper from string."
  (let ((z (of-string "(+ 1 2)")))
    (is (not (null z)))
    (is (eql :list (zip-tag z)))
    (is (equal '(+ 1 2) (zip-sexpr z)))))

(test zipper-of-node
  "Test creating zipper from node."
  (let* ((node (make-list-node (list (make-token-node 'a))))
         (z (of-node node)))
    (is (eq node (zip-node z)))))

;;; Navigation

(test zip-down
  "Test moving down."
  (let* ((z (of-string "(a b c)"))
         (z2 (zip-down z)))
    (is (not (null z2)))
    (is (eq 'a (zip-sexpr z2)))))

(test zip-down-star
  "Test moving down with whitespace skip."
  (let* ((z (of-string "( a b c)"))  ; leading space
         (z2 (zip-down* z)))
    (is (eq 'a (zip-sexpr z2)))))

(test zip-right
  "Test moving right."
  (let* ((z (of-string "(a b c)"))
         (z2 (zip-down z))
         (z3 (zip-right z2)))
    (is (not (null z3)))))

(test zip-right-star
  "Test moving right with whitespace skip."
  (let* ((z (of-string "(a b c)"))
         (z2 (zip-down* z))
         (z3 (zip-right* z2)))
    (is (eq 'b (zip-sexpr z3)))))

(test zip-left
  "Test moving left."
  (let* ((z (of-string "(a b c)"))
         (z2 (zip-down* z))
         (z3 (zip-right* z2))
         (z4 (zip-left* z3)))
    (is (eq 'a (zip-sexpr z4)))))

(test zip-up
  "Test moving up."
  (let* ((z (of-string "(a b c)"))
         (z2 (zip-down* z))
         (z3 (zip-up z2)))
    (is (eql :list (zip-tag z3)))))

(test zip-next
  "Test depth-first next."
  (let* ((z (of-string "(a (b c))"))
         (z2 (zip-down* z))   ; at 'a'
         (z3 (zip-next* z2))) ; at '(b c)'
    (is (eql :list (zip-tag z3)))))

(test zip-root
  "Test navigating to root."
  (let* ((z (of-string "(a (b c))"))
         (z2 (zip-down* z))
         (z3 (zip-right* z2))
         (z4 (zip-down* z3))
         (root (zip-root z4)))
    (is (equal '(a (b c)) (zip-sexpr root)))))

;;; Modification

(test zip-replace
  "Test replacing node."
  (let* ((z (of-string "(+ 1 2)"))
         (z2 (zip-down* z))
         (z3 (zip-right* z2))  ; at 1
         (z4 (zip-replace z3 (make-token-node 10))))
    (is (string= "(+ 10 2)" (zip-root-string z4)))))

(test zip-edit
  "Test editing node via sexpr."
  (let* ((z (of-string "(+ 1 2)"))
         (z2 (zip-down* z))
         (z3 (zip-right* z2))  ; at 1
         (z4 (zip-edit z3 #'1+)))
    (is (string= "(+ 2 2)" (zip-root-string z4)))))

(test zip-insert-left
  "Test inserting left sibling."
  (let* ((z (of-string "(a b)"))
         (z2 (zip-down* z))
         (z3 (zip-right* z2))  ; at 'b'
         (z4 (zip-insert-left z3 (make-token-node 'x))))
    (is (equal '(a x b) (zip-sexpr (zip-root z4))))))

(test zip-insert-right
  "Test inserting right sibling."
  (let* ((z (of-string "(a b)"))
         (z2 (zip-down* z))  ; at 'a'
         (z3 (zip-insert-right z2 (make-token-node 'x))))
    (is (equal '(a x b) (zip-sexpr (zip-root z3))))))

(test zip-remove
  "Test removing node."
  (let* ((z (of-string "(a b c)"))
         (z2 (zip-down* z))
         (z3 (zip-right* z2))  ; at 'b'
         (z4 (zip-remove z3)))
    (is (equal '(a c) (zip-sexpr (zip-root z4))))))

(test zip-insert-child
  "Test inserting first child."
  (let* ((z (of-string "(a b)"))
         (z2 (zip-insert-child z (make-token-node 'x))))
    (is (equal '(x a b) (zip-sexpr z2)))))

(test zip-append-child
  "Test appending child."
  (let* ((z (of-string "(a b)"))
         (z2 (zip-append-child z (make-token-node 'x))))
    (is (equal '(a b x) (zip-sexpr z2)))))

;;; Find operations

(test zip-find-value
  "Test finding by value."
  (let* ((z (of-string "(defun foo (x) (+ x 1))"))
         (z2 (zip-find-value z '+)))
    (is (not (null z2)))
    (is (eq '+ (zip-sexpr z2)))))

(test zip-find-tag
  "Test finding by tag."
  (let* ((z (of-string "(a \"hello\" b)"))
         (z2 (zip-find-tag z :string)))
    (is (not (null z2)))
    (is (string= "hello" (zip-sexpr z2)))))

;;; Walk operations

(test zip-prewalk
  "Test prewalk transformation."
  (let* ((z (of-string "(+ 1 2)"))
         (z2 (zip-prewalk z
               (lambda (zz)
                 (if (and (member (zip-tag zz) '(:integer :number))
                          (numberp (zip-sexpr zz)))
                     (zip-edit zz (lambda (n) (* 2 n)))
                     zz)))))
    (is (string= "(+ 2 4)" (zip-root-string z2)))))

(test zip-postwalk
  "Test postwalk transformation."
  (let* ((z (of-string "(+ 1 (* 2 3))"))
         (z2 (zip-postwalk z
               (lambda (zz)
                 (if (and (member (zip-tag zz) '(:integer :number))
                          (numberp (zip-sexpr zz)))
                     (zip-edit zz #'1+)
                     zz)))))
    ;; All numbers should be incremented
    (let ((result (zip-sexpr (zip-root z2))))
      (is (= 2 (second result)))
      (is (= 3 (second (third result))))
      (is (= 4 (third (third result)))))))

;;; Walk regression tests
;;;
;;; The sibling-chain helpers in zip-prewalk and zip-prewalk-while used to end
;;; with (zip-up z2), which is NIL once the walk has returned to the root.
;;; Both walks then fell through to their (if result ... zipper) guard and
;;; handed the caller back its *input* zipper, silently dropping every edit.
;;; A caller iterating to a fixpoint -- as examples/remove-trailing-whitespace
;;; does -- would spin forever on such input.

(defun drop-whitespace (zipper)
  "Walk function that removes whitespace and newline nodes, keeping the rest."
  (if (or (whitespace-node-p (zip-node zipper))
          (newline-node-p (zip-node zipper)))
      (zip-remove zipper)
      zipper))

(test zip-prewalk-keeps-edits-made-at-root
  "Prewalk must not discard edits when the sibling chain ends at the root."
  ;; "( )" is the minimal trigger: the only editable node is the sole child of
  ;; the root, so walk-siblings reaches the end of the chain with the zipper
  ;; already back at the root and (zip-up z2) is NIL.
  (is (string= "()" (zip-root-string (zip-prewalk (of-string "( )")
                                                  #'drop-whitespace))))
  (is (string= "()" (zip-root-string (zip-prewalk (of-string (format nil "(~%)"))
                                                  #'drop-whitespace))))
  ;; Same shape with a token present, so the edit is not the last sibling.
  (is (string= "(a)" (zip-root-string (zip-prewalk (of-string "(a   )")
                                                   #'drop-whitespace)))))

(test zip-prewalk-while-keeps-edits-made-at-root
  "zip-prewalk-while shares the sibling-chain contract with zip-prewalk."
  (is (string= "()" (zip-root-string (zip-prewalk-while (of-string "( )")
                                                        (constantly t)
                                                        #'drop-whitespace))))
  (is (string= "(a)" (zip-root-string (zip-prewalk-while (of-string "(a   )")
                                                         (constantly t)
                                                         #'drop-whitespace)))))

(test zip-prewalk-reaches-fixpoint-on-nested-whitespace
  "Repeated prewalks must converge, not cycle on a dropped edit."
  ;; zip-remove returns the removed node's right sibling, which walk-siblings
  ;; then steps past, so a run of whitespace can need more than one pass.  What
  ;; must hold is that each pass makes progress and the iteration terminates --
  ;; that stays true if zip-remove's return position is ever changed.
  (let ((current (format nil "(~%  )")))
    (loop repeat 10
          for next = (zip-root-string (zip-prewalk (of-string current)
                                                   #'drop-whitespace))
          until (string= next current)
          do (setf current next))
    (is (string= "()" current))))

(test zip-postwalk-tolerates-nil-from-fn
  "Postwalk must not crash when FN returns NIL, matching prewalk's contract."
  ;; Previously walk-siblings called (zip-right z2) on a NIL z2, signalling a
  ;; TYPE-ERROR rather than leaving the node alone.
  (is (string= "(a)" (zip-root-string (zip-postwalk (of-string "(a)")
                                                    (constantly nil)))))
  (is (string= "(a b)"
               (zip-root-string
                (zip-postwalk (of-string "(a b)")
                              (lambda (z)
                                (if (equal "b" (zip-string z)) nil z)))))))

;;; Sequence operations

(test zip-length
  "Test counting children."
  (let ((z (of-string "(a b c d)")))
    (is (= 4 (zip-length z)))))

(test zip-nth-child
  "Test accessing nth child."
  (let ((z (of-string "(a b c d)")))
    (is (eq 'a (zip-sexpr (zip-nth-child z 0))))
    (is (eq 'b (zip-sexpr (zip-nth-child z 1))))
    (is (eq 'd (zip-sexpr (zip-nth-child z 3))))))

(test zip-child-sexprs
  "Test getting child sexprs."
  (let ((z (of-string "(a b c)")))
    (is (equal '(a b c) (zip-child-sexprs z)))))

;;; Subedit

(test zip-subedit
  "Test isolated subtree editing."
  (let* ((z (of-string "(a (b c) d)"))
         (z2 (zip-down* z))
         (z3 (zip-right* z2))  ; at (b c)
         (z4 (zip-subedit z3
               (lambda (subz)
                 (zip-append-child subz (make-token-node 'x))))))
    (is (equal '(a (b c x) d) (zip-sexpr (zip-root z4))))))

;;; zip-remove position contract
;;;
;;; zip-remove lands on the node *before* the one removed, never the one after.
;;; Returning the right sibling made a left-to-right walk step over it, because
;;; walk-siblings advances with zip-right -- so a removing prewalk needed a
;;; second pass to finish.  Note this is the same-level predecessor, not
;;; clojure.zip's depth-first one: descending into the left sibling would leave
;;; the sibling chain with nowhere to go.  zip-prev remains the DFS predecessor.

(test zip-remove-lands-on-preceding-sibling
  "zip-remove returns the left sibling, with the removed node gone."
  (let* ((z (of-string "(a b c)"))
         (b (zip-find-value z 'b))
         (after (zip-remove b)))
    ;; The node immediately to the left is the separating whitespace, not 'a --
    ;; whitespace is a node in this tree, so that is the true left sibling.
    (is (zip-whitespace-p after))
    (is (eq 'a (zip-sexpr (zip-left* after))))
    (is (equal '(a c) (zip-sexpr (zip-root after))))
    ;; Both separators survive; only 'b is gone.
    (is (string= "(a  c)" (zip-root-string after)))))

(test zip-remove-lands-on-parent-when-first-child
  "With no left sibling, zip-remove moves up and keeps the right siblings."
  (let* ((z (of-string "(a b)"))
         (a (zip-find-value z 'a))
         (after (zip-remove a)))
    (is (eql :list (zip-tag after)))
    (is (equal '(b) (zip-sexpr (zip-root after))))))

(test zip-remove-only-child-empties-parent
  "Removing an only child leaves the parent childless."
  (let* ((z (of-string "(a)"))
         (a (zip-find-value z 'a))
         (after (zip-remove a)))
    (is (eql :list (zip-tag after)))
    (is (string= "()" (zip-root-string after)))))

(test zip-remove-at-root-returns-nil
  "There is nothing to point at after removing the root."
  (is (null (zip-remove (of-string "(a)")))))

(test zip-remove-does-not-skip-during-prewalk
  "A removing prewalk clears a run of whitespace in a single pass."
  ;; This is the assertion that pins the contract down.  Returning the right
  ;; sibling made walk-siblings step over the node after the removed one, so a
  ;; run of adjacent whitespace needed a second pass: one prewalk over "(\n  )"
  ;; dropped the newline and left the spaces behind, giving "(  )".
  (is (string= "()" (zip-root-string
                     (zip-prewalk (of-string (format nil "(~%  )"))
                                  #'drop-whitespace))))
  (is (string= "()" (zip-root-string
                     (zip-prewalk (of-string (format nil "(~% ~%  ~%)"))
                                  #'drop-whitespace))))
  ;; Whitespace following a nested list: this is where clojure.zip's
  ;; depth-first predecessor would strand the walk inside that list.
  (dolist (case '(("(a (b c)   d   )" . "(a(bc)d)")
                  ("((x y)   (z w)   )" . "((xy)(zw))")
                  ("#((p q)   r   )" . "#((pq)r)")))
    (is (string= (cdr case)
                 (zip-root-string (zip-prewalk (of-string (car case))
                                               #'drop-whitespace))))))

;;; Whitespace operations

(test zip-remove-surrounding-whitespace-both-sides
  "Whitespace is stripped on both sides of the current node."
  ;; Previously only the left side was reached, and the all-whitespace case
  ;; signalled a TYPE-ERROR.
  (dolist (case '(("(a   b   c)" . "(abc)")
                  ("(  b   c)" . "(bc)")
                  ("(a   b  )" . "(ab)")
                  ("(  b  )" . "(b)")))
    (let* ((z (of-string (car case)))
           (b (zip-find-value z 'b)))
      (is (string= (cdr case)
                   (zip-root-string
                    (rewrite-cl.zip::zip-remove-surrounding-whitespace b)))))))

(test zip-whitespace-p
  "Test whitespace predicate."
  (let* ((z (of-string "(a b)"))
         (z2 (zip-down z))   ; 'a'
         (z3 (zip-right z2))) ; whitespace
    (is (not (zip-whitespace-p z2)))
    (is (zip-whitespace-p z3))))
