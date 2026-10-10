;;;; whitespace.lisp - Whitespace-aware zipper operations
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.zip)

;;; Predicates

(defun zip-whitespace-p (zipper)
  "Return T if current node is whitespace."
  (eql :whitespace (zip-tag zipper)))

(defun zip-newline-p (zipper)
  "Return T if current node is a newline."
  (eql :newline (zip-tag zipper)))

(defun zip-comment-p (zipper)
  "Return T if current node is a comment."
  (member (zip-tag zipper) '(:comment :block-comment)))

(defun zip-whitespace-or-comment-p (zipper)
  "Return T if current node is whitespace, newline, or comment."
  (whitespace-or-comment-p (zip-node zipper)))

;;; Whitespace-skipping navigation

(defun zip-skip-whitespace (zipper &key (direction :right))
  "Skip whitespace/comment nodes in DIRECTION (:left or :right)."
  (let ((move-fn (ecase direction
                   (:right #'zip-right)
                   (:left #'zip-left))))
    (loop for z = zipper then (funcall move-fn z)
          while (and z (zip-whitespace-or-comment-p z))
          finally (return z))))

(defun zip-right* (zipper)
  "Move right, skipping whitespace and comments."
  (when zipper
    (loop for z = (zip-right zipper) then (zip-right z)
          while z
          unless (zip-whitespace-or-comment-p z)
            return z)))

(defun zip-left* (zipper)
  "Move left, skipping whitespace and comments."
  (when zipper
    (loop for z = (zip-left zipper) then (zip-left z)
          while z
          unless (zip-whitespace-or-comment-p z)
            return z)))

(defun zip-down* (zipper)
  "Move down to first non-whitespace child."
  (let ((z (zip-down zipper)))
    (when z
      (if (zip-whitespace-or-comment-p z)
          (zip-right* z)
          z))))

(defun zip-up* (zipper)
  "Move up (same as zip-up, included for symmetry)."
  (zip-up zipper))

(defun zip-next* (zipper)
  "Move to next node in depth-first order, skipping whitespace."
  (loop for z = (zip-next zipper) then (zip-next z)
        while z
        unless (zip-whitespace-or-comment-p z)
          return z))

(defun zip-prev* (zipper)
  "Move to previous node in depth-first order, skipping whitespace."
  (loop for z = (zip-prev zipper) then (zip-prev z)
        while z
        unless (zip-whitespace-or-comment-p z)
          return z))

;;; Whitespace manipulation

(defun zip-ensure-space-left (zipper)
  "Ensure there is whitespace to the left of current node."
  (let ((left (zip-left zipper)))
    (if (or (null left)
            (zip-whitespace-p left)
            (zip-newline-p left))
        zipper
        (zip-insert-left zipper (spaces 1)))))

(defun zip-ensure-space-right (zipper)
  "Ensure there is whitespace to the right of current node."
  (let ((right (zip-right zipper)))
    (if (or (null right)
            (zip-whitespace-p right)
            (zip-newline-p right))
        zipper
        (zip-insert-right zipper (spaces 1)))))

(defun zip-remove-surrounding-whitespace (zipper)
  "Remove whitespace nodes immediately around current node."
  (let ((z zipper))
    ;; Remove whitespace on the left.  ZIP-REMOVE lands on the node *before*
    ;; the one it removed, so step forward again to get back to our own node:
    ;; to the right sibling, or down from the parent when the whitespace we
    ;; removed was the first child and ZIP-REMOVE therefore moved up.
    (loop for ws = (zip-left z)
          while (and ws (zip-whitespace-p ws))
          do (setf z (if (zip-left ws)
                         (zip-right (zip-remove ws))
                         (zip-down (zip-remove ws)))))
    ;; Remove whitespace on the right.  Here ZIP-REMOVE lands on our own node
    ;; already, since we are the removed node's left sibling.
    (loop for ws = (zip-right z)
          while (and ws (zip-whitespace-p ws))
          do (setf z (zip-remove ws)))
    z))
