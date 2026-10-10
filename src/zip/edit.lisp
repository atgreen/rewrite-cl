;;;; edit.lisp - Zipper modification operations
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.zip)

;;; Basic modifications

(defun zip-replace (zipper node)
  "Replace current node with NODE."
  (%make-zipper
   :node node
   :parent (%zip-parent zipper)
   :left (%zip-left zipper)
   :right (%zip-right zipper)
   :changed-p t
   :track-position-p (%zip-track-position-p zipper)))

(defun zip-edit (zipper fn &rest args)
  "Apply FN to current node's sexpr, coerce result to node, replace.
FN receives the sexpr value and any additional ARGS."
  (let* ((current-value (node-sexpr (zip-node zipper)))
         (new-value (apply fn current-value args))
         (new-node (coerce-to-node new-value)))
    (zip-replace zipper new-node)))

(defun zip-edit-node (zipper fn &rest args)
  "Apply FN to current node directly (not sexpr), replace with result.
FN should return a node."
  (let ((new-node (apply fn (zip-node zipper) args)))
    (zip-replace zipper new-node)))

;;; Insertion

(defun zip-insert-left (zipper node)
  "Insert NODE as left sibling of current node."
  (%make-zipper
   :node (zip-node zipper)
   :parent (%zip-parent zipper)
   :left (cons node (%zip-left zipper))
   :right (%zip-right zipper)
   :changed-p t
   :track-position-p (%zip-track-position-p zipper)))

(defun zip-insert-right (zipper node)
  "Insert NODE as right sibling of current node."
  (%make-zipper
   :node (zip-node zipper)
   :parent (%zip-parent zipper)
   :left (%zip-left zipper)
   :right (cons node (%zip-right zipper))
   :changed-p t
   :track-position-p (%zip-track-position-p zipper)))

(defun zip-insert-child (zipper node)
  "Insert NODE as first child of current node.
Current node must be an inner node."
  (let ((children (node-children (zip-node zipper))))
    (zip-replace zipper
                 (node-replace-children
                  (zip-node zipper)
                  (cons node children)))))

(defun zip-append-child (zipper node)
  "Append NODE as last child of current node.
Current node must be an inner node."
  (let ((children (node-children (zip-node zipper))))
    (zip-replace zipper
                 (node-replace-children
                  (zip-node zipper)
                  (append children (list node))))))

;;; Removal

(defun zip-remove (zipper)
  "Remove current node, returning zipper at the preceding sibling.
Moves to the left sibling, or to the parent when there is none; returns NIL
at the root.  Returns NIL only when there is nothing left to point at.

The position is the node *before* the one removed, never the one after, so a
left-to-right walk resumes on the removed node's successor instead of
stepping over it.  ZIP-PREWALK and friends advance with ZIP-RIGHT, so this
stays at the removed node's own level rather than descending into the left
sibling the way CLOJURE.ZIP's REMOVE does -- descending is right only for a
ZIP-NEXT-based traversal, and would leave the sibling chain unable to
continue.  See ZIP-PREV for the depth-first predecessor."
  (cond
    ;; Move to left sibling, dropping this node from its right siblings
    ((%zip-left zipper)
     (%make-zipper
      :node (first (%zip-left zipper))
      :parent (%zip-parent zipper)
      :left (rest (%zip-left zipper))
      :right (%zip-right zipper)
      :changed-p t
      :track-position-p (%zip-track-position-p zipper)))

    ;; No left sibling: move to parent, keeping any right siblings as its
    ;; remaining children
    ((%zip-parent zipper)
     (let ((parent (%zip-parent zipper)))
       (%make-zipper
        :node (node-replace-children (zip-node parent) (%zip-right zipper))
        :parent (%zip-parent parent)
        :left (%zip-left parent)
        :right (%zip-right parent)
        :changed-p t
        :track-position-p (%zip-track-position-p zipper))))

    ;; At root with no siblings - return nil
    (t nil)))

;;; Splicing

(defun zip-splice (zipper nodes)
  "Replace current node with multiple NODES (splice into siblings).
Zipper ends up at first of the spliced nodes."
  (if (null nodes)
      (zip-remove zipper)
      (%make-zipper
       :node (first nodes)
       :parent (%zip-parent zipper)
       :left (%zip-left zipper)
       :right (append (rest nodes) (%zip-right zipper))
       :changed-p t
       :track-position-p (%zip-track-position-p zipper))))

(defun zip-splice-left (zipper nodes)
  "Insert NODES as left siblings of current node."
  (reduce (lambda (z node)
            (zip-insert-left z node))
          (reverse nodes)
          :initial-value zipper))

(defun zip-splice-right (zipper nodes)
  "Insert NODES as right siblings of current node."
  (reduce (lambda (z node)
            (zip-insert-right z node))
          (reverse nodes)
          :initial-value zipper))

;;; Subedit

(defun zip-subzip (zipper)
  "Create a new zipper rooted at current node."
  (make-zipper (zip-node zipper)
               :track-position (%zip-track-position-p zipper)))

(defun zip-subedit (zipper fn)
  "Apply FN to a subzipper at current node, replace with result.
FN receives a zipper rooted at current node and should return a zipper."
  (let* ((subzip (zip-subzip zipper))
         (result (funcall fn subzip)))
    (zip-replace zipper (zip-root-node result))))
