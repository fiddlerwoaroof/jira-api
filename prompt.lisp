(in-package #:jira-api.cli)

(defparameter *prompt-stream* (make-synonym-stream '*query-io*))

(defun prompt-for-line (stream prompt &rest args)
  (apply #'format stream prompt args)
  (force-output stream)
  (read-line stream nil :done))
(trace prompt-for-line)

(defun prompt-for-lines (stream initial-prompt continuation-prompt &rest args)
  "Not finished ... see :description below"
  (loop with lines = (make-array 10 :adjustable t :fill-pointer 0)
        for line = (prompt-for-line stream initial-prompt args)
        then (prompt-for-line stream continuation-prompt)
        for trim-line = (trim-whitespace line)
        until (equal trim-line ".")
        do (vector-push-extend trim-line lines)
        finally (return (string-join lines #\space))))

(defun prompt-for-boolean (stream prompt &rest args)
  (let ((input (string-downcase (prompt-for-line stream prompt args))))
    (ecase (elt input 0)
      (#\y t)
      (#\n nil))))

(defgeneric prompt (field-name &optional stream))

(defun optional-prompt (stream field-name test-prompt &rest args)
  (when (apply #'prompt-for-boolean stream test-prompt args)
    (prompt field-name stream)))


(defmethod prompt ((field-name (eql :project-key)) &optional (stream *prompt-stream*))
  (funcall #'string-upcase (prompt-for-line stream "Project Key (e.g. ATOMOS)? ")))

(defmethod prompt ((field-name (eql :summary)) &optional (stream *prompt-stream*))
  (prompt-for-line stream "Summary? "))

(defmethod prompt ((field-name (eql :description)) &optional (stream *prompt-stream*))
  (loop with lines = (make-array 10 :adjustable t :fill-pointer 0)
        for line = (prompt-for-line stream "Enter a description end with \".\" on its own line~%? ")
        then (prompt-for-line stream "? ")
        for trim-line = (when (stringp line)
                          (trim-whitespace line))
        until (or (eql trim-line :done) (equal trim-line "."))
        do (vector-push-extend trim-line lines)
        finally (return (string-join lines #\newline))))

