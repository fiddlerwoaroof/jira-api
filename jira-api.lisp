;;;; jira-api.lisp
(in-package #:jira-api)

(defun get-filter (auth filter &rest r &key (expand "subscriptions[:-5]"))
  (declare (ignore expand))
  (apply 'api-get-call auth (format nil "filter/~d" filter) r))

(defun get-issues (auth &key jql)
  (if jql
      (api-get-call auth "search" "jql" jql)
      (api-get-call auth "search")))

(defmacro jql (auth &body jql)
  `(api-get-call ,auth "search" "jql"
                 ,(apply #'serapeum:concat jql)))

(defun run-filter (auth filter)
  (let ((jql (gethash "jql" (yason:parse (get-filter auth filter)))))
    (get-issues auth :jql jql)))

(defun get-issue (auth key &rest params &key expand)
  (declare (ignore expand))
  (apply 'api-get-call auth (format nil "issue/~a" key) params))

(defun get-board-issues (auth key &rest params &key expand)
  (declare (ignore expand))
  (apply 'api-get-call auth (format nil "board/~a/issue" key) params))

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

(defun classify-issues (sheeple-issues &optional (classify-by '(status name)))
  (sheeple:with-properties (issues) sheeple-issues
    (loop with classes = (make-hash-table :test 'equalp)
          for issue across issues
          do (ensure-parent issue =issue=)
          do (loop for classification in classify-by
                   for thing = (sheeple:property-value (fields issue) classification)
                       then (sheeple:property-value thing classification)
                   finally (push issue (gethash thing classes)))
          finally (return classes))))

(defun status-to-num (status)
  (string-case (string-downcase status)
    ("open" 0)
    ("in progress" 1)
    ("needs qr" 2)
    ("needs demo" 3)
    ("closed" 4)
    (t 5)))

(defun sort-issues-by-status (sheeple-issues)
  (prog1 sheeple-issues
    (sheeple:with-properties (issues) sheeple-issues
      (setf issues
            (sort issues #'< :key (op (status-to-num (name (status (fields _))))))))))

(defun show-issue-short (issue &optional (stream t))
  (sheeple:with-properties (key self) issue
    (let* ((fields (fields issue))
           (status (name (status fields)))
           (summary (summary fields)))

      (format stream "~&~a (~a) <~a>~%"
              key
              status 
              (puri:merge-uris (format nil "/browse/~a" (key issue))
                               *hostname*))

      (show-summary summary stream)
      (fresh-line stream))))

(defun map-issues (fun sheeple-issues)
  (sheeple:with-properties (issues) sheeple-issues
    (loop for issue across issues
          do (ensure-parent issue =issue=)
          collect (funcall fun issue))))

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

(defun get-points-for-issues (count start)
  (loop with end = (+ count start)
     for batch-start from start to end by 50
     for batch-count = (min (- end batch-start)
                            50)
     append (map 'list #'points
                 (issues
                  (json2sheeple
                   (get-issues *auth* :jql
                               (format nil "issueKey in (~{CJPM-~a~^, ~})"
                                       (alexandria:iota batch-count
                                                        :start batch-start)))
                   =issues=)))))
