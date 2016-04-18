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
         (flag :short-name "pi" :long-name "post-issue"
               :description "post and issue"))
  (group (:header "JIRA options")
         (stropt :long-name "jira-account"
                 :description "The jira account to use."
                 :argument-name "URL-SUBDOMAIN"
                 :default-value (ubiquitous:value :jira :account)))
  (group (:header "Filtering Issues")
         (stropt :short-name "s" :long-name "status"
                 :description "Only show issues with a certain status"))
  (group (:header "Other options")
         (flag :short-name "h" :long-name "help"
               :description "Show this help") 
         (flag :short-name "v" :long-name "version"
               :description "Show the program version")))

(defvar *auth*)

(defun main ()
  (ubiquitous:restore :jira-api) 
  (setf *auth* (ubiquitous:value :jira :creds))
  (make-context)
  
  (let ((jira-api::*endpoint* (format nil *endpoint-template* (getopt :long-name "jira-account"))))
    (cond
      ((getopt :long-name "help") (help))
      ((getopt :long-name "version") (format t "~&~a~%" *version*))
      ((getopt :long-name "get-issue") (let ((options (remainder)))
                                         (jira-api::show-issue
                                           (jira-api::json2sheeple
                                             (jira-api::get-issue *auth*
                                                                  (format nil "~a-~a"
                                                                          (car options)
                                                                          (cadr options)))))))
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
      (t   (do-cmdline-options (option name value source)
                               (print (list option name value source)))
           (terpri)
           (exit)))))

(dump "jira-client" main)
