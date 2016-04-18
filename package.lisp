;;;; package.lisp

(defpackage #:jira-api.cli
  (:use :cl :anaphora :alexandria :serapeum :fw.lu)
  (:export #:prompt-for-lines
           #:prompt-for-line
           #:optional-prompt
           #:prompt))

(defpackage #:jira-api
  (:use #:cl #:serapeum #:alexandria #:fw.lu #:jira-api.cli))

