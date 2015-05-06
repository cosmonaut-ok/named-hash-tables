;; Copyright (C) 2014 Alexander aka CosmonauT Vynnyk
;;
;;  This file is part of common lisp ghostcode library.
;;
;; ghostcode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; ghostcode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

(defpackage :named-hashtables
  (:nicknames :n-c)
  (:use :cl)
)

(in-package :named-hashtables)


(defclass named-hashtable ()
  ((name :initarg :name :accessor n-h-name)
   (hash :initarg :body :accessor n-h-hash :initform (make-hash-table))
   (documentation :initarg :documentation :accessor n-h-documentation)))

(defun make-named-hash-table (name &key (test #+clisp 'fasthash-eql #-clisp 'eql)
                                (size 1) (rehash-size 1.5)
                                (rehash-threshold 0.75) documentation)
  (let ((hashtable
         (make-hash-table :test test :size size
                          :rehash-size rehash-size
                          :rehash-threshold rehash-threshold)))
    (make-instance 'named-hashtable
                   :name name
                   :documentation documentation
                   :hash hashtable)))

(defmethod get-named-hash (key (hashtable named-hashtable))
  "makes gethash from named hashtable"
  (gethash key (n-h-hash hashtable)))

(defmethod map-named-hash (function (hashtable named-hashtable))
  "apply function to named hashtable"
  (maphash function (n-h-hash hashtable)))

(defmethod named-hash-table-size ((hashtable named-hashtable))
  "get named hashtable size"
  (hash-table-size (n-h-hash hashtable)))

(defun named-hash-table-p (hashtable)
  "returns T if object is named hashtable"
  (when (and
         (eq (class-of hashtable) (find-class 'named-hashtable))
         (hash-table-p (n-h-hash hashtable)))
    t))

(defmethod named-hash-table-count ((hashtable named-hashtable))
  "returns elements count in named hashtable"
  (hash-table-count (n-h-hash hashtable)))

(defmethod named-hash-table-rehash-size ((hashtable named-hashtable))
  "returns hash-table-rehash-size for named hashtable"
    (hash-table-rehash-size (n-h-hash hashtable)))

(defmethod named-hash-table-rehash-threshold ((hashtable named-hashtable))
  "returns hash-table-rehash-threshold for named hashtable"
  (hash-table-rehash-threshold (n-h-hash hashtable)))

(defmethod named-hash-table-test ((hashtable named-hashtable))
  "returns hash-table-test for named hashtable"
    (hash-table-test (n-h-hash hashtable)))

;; /DONE
;; TODO:
(defmethod ensure-named-gethash (name (hashtable named-hashtable))
  ""
  )
(defmethod copy-hamed-hash-table ((hashtable named-hashtable)
                                  &key key test size
                                    rehash-size rehash-threshold)
  "Returns a copy of hash table TABLE, with the same keys and values
as the TABLE. The copy has the same properties as the original, unless
overridden by the keyword arguments.

Before each of the original values is set into the new hash-table, KEY
is invoked on the value. As KEY defaults to CL:IDENTITY, a shallow
copy is returned by default."
  (let ((table (n-h-hash hashtable)))
  (setf key (or key 'identity))
  (setf test (or test (hash-table-test table)))
  (setf size (or size (hash-table-size table)))
  (setf rehash-size (or rehash-size (hash-table-rehash-size table)))
  (setf rehash-threshold (or rehash-threshold (hash-table-rehash-threshold table)))
  (let ((copy (make-hash-table :test test :size size
                               :rehash-size rehash-size
                               :rehash-threshold rehash-threshold))
        (obj-copy (make-named-hash-table ))
    (maphash (lambda (k v)
               (setf (gethash k copy) (funcall key v)))
             table)
    copy)))

(defmethod named-maphash-keys ((hashtable named-hashtable))
  ""
  )
(defmethod named-maphash-values ((hashtable named-hashtable))
  ""
  )
(defmethod named-hash-table-keys ((hashtable named-hashtable))
  ""
  )
(defmethod named-hash-table-values ((hashtable named-hashtable))
  ""
  )
(defmethod named-hash-table-alist ((hashtable named-hashtable))
  ""
  )
(defmethod named-hash-table-plist ((hashtable named-hashtable))
  ""
  )
(defmethod named-alist-hash-table ((hashtable named-hashtable))
  ""
  )
(defmethod named-plist-hash-table ((hashtable named-hashtable))
  ""
  )
(defmethod named-named-hashtable-table ((hashtable named-hashtable))
  ""
  )








;; (declaim (inline maphash-keys))
;; (defun maphash-keys (function table)
;;   "Like MAPHASH, but calls FUNCTION with each key in the hash table TABLE."
;;   (maphash (lambda (k v)
;;              (declare (ignore v))
;;              (funcall function k))
;;            table))

;; (declaim (inline maphash-values))
;; (defun maphash-values (function table)
;;   "Like MAPHASH, but calls FUNCTION with each value in the hash table TABLE."
;;   (maphash (lambda (k v)
;;              (declare (ignore k))
;;              (funcall function v))
;;            table))

;; (defun hash-table-keys (table)
;;   "Returns a list containing the keys of hash table TABLE."
;;   (let ((keys nil))
;;     (maphash-keys (lambda (k)
;;                     (push k keys))
;;                   table)
;;     keys))

;; (defun hash-table-values (table)
;;   "Returns a list containing the values of hash table TABLE."
;;   (let ((values nil))
;;     (maphash-values (lambda (v)
;;                       (push v values))
;;                     table)
;;     values))

;; (defun hash-table-alist (table)
;;   "Returns an association list containing the keys and values of hash table
;; TABLE."
;;   (let ((alist nil))
;;     (maphash (lambda (k v)
;;                (push (cons k v) alist))
;;              table)
;;     alist))

;; (defun hash-table-plist (table)
;;   "Returns a property list containing the keys and values of hash table
;; TABLE."
;;   (let ((plist nil))
;;     (maphash (lambda (k v)
;;                (setf plist (list* k v plist)))
;;              table)
;;     plist))

;; (defun alist-hash-table (alist &rest hash-table-initargs)
;;   "Returns a hash table containing the keys and values of the association list
;; ALIST. Hash table is initialized using the HASH-TABLE-INITARGS."
;;   (let ((table (apply #'make-hash-table hash-table-initargs)))
;;     (dolist (cons alist)
;;       (setf (gethash (car cons) table) (cdr cons)))
;;     table))

;; (defun plist-hash-table (plist &rest hash-table-initargs)
;;   "Returns a hash table containing the keys and values of the property list
;; PLIST. Hash table is initialized using the HASH-TABLE-INITARGS."
;;   (let ((table (apply #'make-hash-table hash-table-initargs)))
;;     (do ((tail plist (cddr tail)))
;;         ((not tail))
;;       (setf (gethash (car tail) table) (cadr tail)))
;;     table))

;; (defmacro ensure-gethash (key hash-table &optional default)
;;   "Like GETHASH, but if KEY is not found in the HASH-TABLE saves the DEFAULT
;; under key before returning it. Secondary return value is true if key was
;; already in the table."
;;   `(multiple-value-bind (value ok) (gethash ,key ,hash-table)
;;      (if ok
;;          (values value ok)
;;          (values (setf (gethash ,key ,hash-table) ,default) nil))))
