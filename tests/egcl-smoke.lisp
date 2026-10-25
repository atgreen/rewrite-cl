(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry
       (list :directory (merge-pathnames "../" (uiop:pathname-directory-pathname *load-truename*)))
       :inherit-configuration))
(asdf:load-system "rewrite-cl")
(let* ((source (format nil "; preserve me~%(foo  42)~%"))
       (node (rewrite-cl:parse-string-all source)))
  (assert (string= source (format nil "~{~A~}" (mapcar #'rewrite-cl.node:node-string node)))))
(format t "REWRITE-CL-PASS~%")
