;;;; jira-api.asd

(asdf:defsystem #:jira-api
  :description "Describe jira-api here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:fwoar.lisputils
               #:alexandria
               #:serapeum
               #:drakma
               #:xhtmlambda
               #:yason
               #:sheeple)
  :serial t
  :components ((:file "package")
               (:file "prompt")
               (:file "api-handler")
               (:file "sheeple-protos")
               (:file "jira-api")))

