#!/usr/bin/sbcl --script
(require :sb-posix)
(load #p"~/quicklisp/setup.lisp")
(eval-when (:load-toplevel :compile-toplevel :execute)
  (push (truename ".") asdf:*central-registry*)
  (ql:quickload :ubiquitous)
  (sb-posix:setenv "CC" "gcc" 1)
  (ql:quickload :net.didierverna.clon)
  (ql:quickload :jira-api))

(defpackage #:jira-api.client
  (:use #:cl #:serapeum #:alexandria #:fw.lu #:jira-api.cli #:jira-api #:net.didierverna.clon))

(in-package #:jira-api.client)
(defparameter *version* (format nil "0.1-init"))

(defparameter *endpoint-template* "https://~a.atlassian.net/rest/api/2/")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ubiquitous:restore :jira-api))

(defsynopsis (:postfix "ARGUMENTS...")
  (text :contents "A command line client for Jira issues")
  (group (:header "Main actions")
         (flag :short-name "lp" :long-name "list-projects"
               :description "List available JIRA projects")
         (flag :short-name "is"
               :long-name "get-issues"
               :description "list issues")
         (flag :short-name "i"
               :long-name "get-issue"
               :description "show an issue")
         ;(flag :short-name "pi" :long-name "post-issue"
         ;      :description "post and issue")
         )
  (group (:header "JIRA options")
         (stropt :long-name "jira-account"
                 :description "The jira account to use."
                 :argument-name "URL-SUBDOMAIN"
                 :default-value (ubiquitous:value :jira :account)))
  (group (:header "Filtering Issues")
         (flag :short-name "c" :long-name "with-comments"
               :description "Show the issue's comments") 
         (stropt :short-name "s" :long-name "status"
                 :description "Only show issues with a certain status"))
  (group (:header "Other options")
         (flag :short-name "dc" :long-name "dump-configuration"
               :description "Dump the current configuration")
         (flag :short-name "g" :long-name "configure"
               :description "Configure the default values")
         (flag :short-name "h" :long-name "help"
               :description "Show this help")
         (flag :short-name "v" :long-name "version"
               :description "Show the program version")))

(defvar *auth*)

(defun dump-configuration ()
  (format t "~&~s~&"
          (hash-table-alist
            (ubiquitous:value :jira))))

(defun main ()
  (ubiquitous:restore :jira-api)
  (setf *auth* (ubiquitous:value :jira :creds))
  (make-context)

  (jira-api::update-hostname (getopt :long-name "jira-account"))
  (cond
    ((getopt :long-name "help") (help))
    ((getopt :long-name "version") (format t "~&~a~%" *version*))
    ((getopt :long-name "dump-configuration") (dump-configuration))
    ((getopt :long-name "configure") (let ((creds (prompt :creds))
                                           (account (prompt :jira-account)))
                                       (setf (ubiquitous:value :jira :creds) creds)
                                       (setf (ubiquitous:value :jira :account) account)
                                       (dump-configuration)))
    ((getopt :long-name "get-issue") (let ((options (remainder)))
                                       (format t "~&~a~&"
                                               (jira-api::show
                                                 (jira-api::json2sheeple
                                                   (jira-api::get-issue *auth*
                                                                        (format nil "~a-~a"
                                                                                (car options)
                                                                                (cadr options)))
                                                   jira-api::=issue=)
                                                 (getopt :long-name "with-comments")))))
    ((getopt :long-name "get-issues")
     (let ((options (remainder)))
       (let ((issues (jira-api::json2sheeple (jira-api::get-issues *auth*))))
         (alexandria:if-let ((status (getopt :long-name "status")))
           (setf issues
                 (sheeple:defobject ()
                                    ((jira-api::issues
                                       (apply 'vector
                                              (gethash status
                                                       (jira-api::classify-issues issues))))))))
         (jira-api::show-issues issues))))
    ((getopt :long-name "list-projects") (jira-api::show-projects
                                           (jira-api::json2sheeple
                                             (jira-api::get-projects *auth*))))
    ((getopt :long-name "post-issue") (yason:encode (jira-api::read-issue)))
    (t  (help) (exit))))

(dump "jira-client" main)
