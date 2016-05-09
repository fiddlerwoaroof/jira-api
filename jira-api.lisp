;;;; jira-api.lisp
(declaim (optimize (debug 3)))

(in-package #:jira-api)

(defun get-issues (auth)
  (api-get-call auth "search"))

(defun get-issue (auth key &rest params &key expand)
  (declare (ignore expand))
  (apply 'api-get-call auth (format nil "issue/~a" key) params))

(defun get-projects (auth)
  (api-get-call auth "issue/createmeta"))

(defun make-issue (project-key summary &key (description nil description-p) (issue-type "Bug"))
  (let* ((issue (make-hash-table :test 'equal))
         (project (alist-string-hash-table `(("key" . ,project-key))))
         (issue-type (alist-string-hash-table `(("name" . ,issue-type)))))
    (setf (gethash "fields" issue)
          (alist-string-hash-table
            `(("project" . ,project)
              ("summary" . ,summary)
              ,@(when description-p
                  (list
                    (cons "description" description)))
              ("issuetype" . ,issue-type))))
    issue))

(defun post-issue (auth issue)
  (let-each (:be *)
    issue
    (with-output-to-string (s)
      (yason:encode * s))
    (api-post-call auth "issue" *)))

(defun post-issue-from-fields (auth project-key summary &key
                        (description nil description-p)
                        (issue-type nil issue-type-p))
  (let ((optional-arguments '()))
    (when issue-type-p
      (push issue-type optional-arguments))
    (when description-p
      (push description optional-arguments))

    (let-each (:be *)
      (apply #'make-issue project-key summary optional-arguments)
      (post-issue auth *))))

(defun read-issue (&optional (iostream *query-io*))
  (let ((project-key (prompt :project-key iostream))
        (summary (prompt :summary iostream))
        (description (optional-prompt iostream :description "Add a description? ")))
    (apply #'make-issue project-key summary
           (list*
             (when description
               (list description))))))

(defun show-person (person title)
  (format t "~&~4t~a: ~a~%" title (show person)))

(defun show-labels (labels)
  (format t "~&~4tLabels: ~{~a~}~%" (coerce labels 'list)))

(defun show-description (description)
  (pprint-logical-block (*standard-output* (mapcar (compose 'tokens 'trim-whitespace)
                                                 (lines description)))
    (pprint-indent :block 4 *standard-output*)
    (pprint-newline :mandatory *standard-output*)
    (loop
      (pprint-exit-if-list-exhausted)
      (let ((line (pprint-pop)))
        (pprint-logical-block (*standard-output* line)
          (loop
            (princ (pprint-pop) *standard-output*)
            (pprint-exit-if-list-exhausted)
            (princ #\space *standard-output*)
            (pprint-indent :block 3)
            (pprint-newline :fill *standard-output*)))
        (pprint-newline :mandatory *standard-output*)))))

(defun show-summary (summary)
  (pprint-logical-block (*standard-output* (funcall (compose 'tokens 'trim-whitespace) summary))
    (pprint-indent :block 8 *standard-output*)
    (pprint-exit-if-list-exhausted)
    (format *standard-output* "~4tSummary: ")
    (loop
      (princ (pprint-pop))
      (pprint-exit-if-list-exhausted)
      (pprint-newline :fill *standard-output*)
      (princ #\space))))

(defun classify-issues (sheeple-issues &optional (classify-by '(status name)))
  (sheeple:with-properties (issues) sheeple-issues
    (loop with classes = (make-hash-table :test 'equalp)
          for issue across issues
          do (loop for classification in classify-by
                   for thing = (sheeple:property-value (fields issue) classification)
                       then (sheeple:property-value thing classification)
                   finally (push issue (gethash thing classes)))
          finally (return classes))))

(defun show-issue-short (issue)
  (sheeple:with-properties (key self) issue
    (let* ((fields (fields issue))
           (status (name (status fields)))
           (summary (summary fields)))
      (format t "~&~a ~a (~a)~%" self key status)
      (show-summary summary)
      (fresh-line))))

(defun show-issues (sheeple-issues)
  (sheeple:with-properties (issues) sheeple-issues
    (loop for issue across issues
          do (ensure-parent issue =issue=)
          do (show-issue-short issue))))

(defun print-on-own-line (str)
  (format t "~&~a~&" str))

(defun show-projects (projects)
  (sheeple:with-properties (projects) projects
    (loop for project across projects
          do (ensure-parent project =project=)
          do (print-on-own-line (show project)))))
