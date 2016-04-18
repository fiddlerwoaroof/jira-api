;;;; jira-api.lisp

(in-package #:jira-api)

;;; "jira-api" goes here. Hacks and glory await!

; curl -u user:password https://atomampd.atlassian.net/rest/api/2/issue/ATOMOS-212 | jq .
(defparameter *endpoint* "https://socraticum.atlassian.net/rest/api/2/")

(defun api-get-call (auth method &rest parameters)
  "Connect to a GET REST endpoint specified by method and return a stream from
   which the response can be read."
  (let ((drakma:*text-content-types* (acons "application" "json" drakma:*text-content-types*)))
    (drakma:http-request (puri:merge-uris method *endpoint*)
                         :parameters (alexandria:plist-alist parameters)
                         :basic-authorization auth
                         :want-stream t)))

(defun api-post-call (auth method post-data)
  "Connect to a GET REST endpoint specified by method and return a stream from
   which the response can be read."
  (let ((drakma:*text-content-types* (acons "application" "json" drakma:*text-content-types*)))
    (drakma:http-request (puri:merge-uris method *endpoint*)
                         :method :POST
                         :content-type "application/json"
                         :content post-data
                         :basic-authorization auth
                         :want-stream t)))

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

(deftype vector-of-objects () '(vector (or hash-table sheeple:object) *))

(defun json2sheeple (json)
  (labels
    ((handle-parsed (parsed-json)
       (typecase parsed-json
         (vector-of-objects (map 'vector #'handle-parsed parsed-json))
         (hash-table
           (let ((result (sheeple:object)))
             (loop for json-prop being the hash-keys of parsed-json using (hash-value json-value)
                   do (setf (sheeple:property-value result
                                                    (intern (string-upcase json-prop) :jira-api))
                            (typecase json-value
                              (hash-table (handle-parsed json-value))
                              (vector-of-objects (map 'vector #'handle-parsed json-value))
                              (t json-value)))
                   finally (return result))))
         (t parsed-json))))
    (let ((yason:*parse-json-arrays-as-vectors* t))
      (handle-parsed (yason:parse json)))))

(defun show-person (person title)
  (sheeple:with-properties (displayname emailaddress) person
    (format t "~&~4t~a: \"~a\" <~a>~%" title displayname emailaddress)))

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
           do (sheeple:with-properties (fields) issue
                (loop for classification in classify-by
                      for thing = (sheeple:property-value fields classification)
                          then (sheeple:property-value thing classification)
                      finally (push issue (gethash thing classes))))
           finally (return classes))))

(defun show-issues (sheeple-issues)
  (sheeple:with-properties (issues) sheeple-issues
    (loop for issue across issues
          do (show-issue-short issue))))

(defun show-issue-short (issue)
  (sheeple:with-properties (key fields self) issue
    (sheeple:with-properties (summary status) fields
      (sheeple:with-properties ((status-name name)) status
        (format t "~&~a ~a (~a)~%" self key status-name))
      (show-summary summary)
      (fresh-line))))

(defun show-issue (issue)
  (declare (optimize (debug 3)))
  (sheeple:with-properties (fields key id self) issue
    (sheeple:with-properties (summary description reporter creator assignee status labels) fields
      (sheeple:with-properties ((status-name name)) status
        (format t "~a (~a) <~a>~%" key status-name self))

      (show-summary summary)

      (show-person reporter "Reporter")
      (show-person creator "Creator")
      (when assignee
        (show-person assignee "Assignee"))

      (when (< 0 (length labels))
        (show-labels labels))

      (when description
        (show-description description)))))

(defun show-issuetype (issuetype)
  (sheeple:with-properties (description name) issuetype
    (pprint-logical-block (*standard-output* (tokens description))
      (pprint-indent :block 15 *standard-output*)
      (pprint-exit-if-list-exhausted)
      (format *standard-output* "~4t~10@a: " name)
      (loop
        (princ (pprint-pop))
        (pprint-exit-if-list-exhausted)
        (pprint-newline :fill *standard-output*)
        (princ #\space)))))

(defun show-project (project)
  (sheeple:with-properties (name key issuetypes) project
    (format t "~a: ~a~%" key name)
    (loop for issuetype across issuetypes
          do (show-issuetype issuetype)
          do (terpri))))

(defun show-projects (projects)
  (sheeple:with-properties (projects) projects
    (loop for project across projects
          do (show-project project))))
