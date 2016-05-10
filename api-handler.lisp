(in-package #:jira-api) 

; curl -u user:password https://atomampd.atlassian.net/rest/api/2/issue/ATOMOS-212 | jq .
(defparameter *hostname* "https://atomampd.atlassian.net")
(defparameter *endpoint* (princ-to-string (puri:merge-uris *hostname* "/rest/api/2/")))

(defun update-hostname (new-subdomain)
  (setf *hostname* (format nil "https://~a.atlassian.net" new-subdomain))
  (setf *endpoint* (princ-to-string (puri:merge-uris "/rest/api/2/" *hostname*))))

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


