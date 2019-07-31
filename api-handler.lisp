(in-package #:jira-api) 

; curl -u user:password https://atomampd.atlassian.net/rest/api/2/issue/ATOMOS-212 | jq .
(defparameter *hostname* "https://atomampd.atlassian.net")
(defparameter *endpoint* (princ-to-string (puri:merge-uris *hostname* "/rest/api/2/")))
(defparameter *agile-endpoint* (princ-to-string (puri:merge-uris *hostname* "/rest/agile/1.0/")))

(defun update-hostname ()
  (setf *hostname* "https://jira.cnvrmedia.net")
  (setf *endpoint* (princ-to-string (puri:merge-uris "/rest/api/2/" *hostname*)))
  (setf *agile-endpoint* (princ-to-string (puri:merge-uris *hostname* "/rest/agile/1.0/"))))

(define-condition jira-error ()
  ())

(define-condition auth-call-unauthorized (jira-error)
  ())

(define-condition server-error ()
  ())

(defun api-get-call (auth method &rest parameters)
  "Connect to a GET REST endpoint specified by method and return a stream from
   which the response can be read."
  (let ((drakma:*text-content-types* (acons "application" "json" drakma:*text-content-types*)))
    (format t "~&~a ~s~%" (puri:merge-uris method *endpoint*) (alexandria:plist-alist parameters))
    (multiple-value-bind (stream retcode)
        (drakma:http-request (puri:merge-uris method *endpoint*)
                             :parameters (alexandria:plist-alist parameters)
                             :basic-authorization auth
                             :want-stream t)
      (case retcode
        (401 (error 'auth-call-unauthorized))
        (500 (error 'server-error))
        (t stream)))))

(defun agile-get-call (auth method &rest parameters)
  "Connect to a GET REST endpoint specified by method and return a stream from
   which the response can be read."
  (let ((drakma:*text-content-types* (acons "application" "json" drakma:*text-content-types*)))
    (format t "~&~a ~s~%" (puri:merge-uris method *agile-endpoint*) (alexandria:plist-alist parameters))
    (multiple-value-bind (stream retcode)
        (drakma:http-request (puri:merge-uris method *agile-endpoint*)
                             :parameters (alexandria:plist-alist parameters)
                             :basic-authorization auth
                             :want-stream t)
      (case retcode
        (401 (error 'auth-call-unauthorized))
        (500 (error 'server-error))
        (t stream)))))

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


