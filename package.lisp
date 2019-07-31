;;;; package.lisp

(defpackage #:jira-api.cli
  (:use :cl #:anaphora #:alexandria #:serapeum #:fw.lu)
  (:export #:optional-prompt
           #:prompt
           #:prompt-for-line
           #:prompt-for-lines))

(defpackage #:jira-api
  (:shadow #:comment)
  (:use #:cl #:jira-api.cli #:alexandria #:fw.lu #:serapeum)
  (:export #:auth-call-unauthorized
           #:classify-issues
           #:get-issues
           #:jql
           #:jira-error
           #:server-error))

