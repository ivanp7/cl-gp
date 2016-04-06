;;;; uses.lisp

(in-package #:cl-gp)

(defconstant +purpose/callee+ :callee)
(defconstant +purpose/caller+ :caller)
(defconstant +purpose/call+ :call)

(defun node/callee? (node)
  (eql (node/purpose node) +purpose/callee+))

(defun node/caller? (node)
  (eql (node/purpose node) +purpose/caller+))

(defun connection/call? (connection)
  (eql (connection/purpose connection) +purpose/call+))

(defun connection/callee-label (connection)
  (if (connection/call? connection)
      (connection/source-label connection)
      (error "CONNECTION/CALLEE-LABEL -- connection is not a call")))

(defun connection/caller-label (connection)
  (if (connection/call? connection)
      (connection/target-label connection)
      (error "CONNECTION/CALLER-LABEL -- connection is not a call")))

(defun make-call-connection (callee-label caller-label &rest args)
  (apply (alexandria:curry #'make-instance 'object/connection
                           :source callee-label
                           :target caller-label
                           :purpose +purpose/call+) args))
