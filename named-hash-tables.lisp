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

(defpackage :named-hash-tables
  (:nicknames :nht)
  (:use :cl :alexandria)
  (:export :named-hash-table
           :make-named-hash-table
           :get-named-hash
	   :set-named-hash
	   :rem-named-hash
	   :get-named-name
           :map-named-hash
           :named-hash-table-size
           :named-hash-table-p
           :named-hash-table-count
           :named-hash-table-rehash-size
           :named-hash-table-rehash-threshold
           :named-hash-table-test
           :ensure-named-gethash
           :copy-hamed-hash-table
           :named-maphash-keys
           :named-maphash-values
           :named-hash-table-keys
           :named-hash-table-values
           :named-hash-table-alist
           :named-hash-table-plist
           :named-alist-hash-table
           :named-plist-hash-table
           :named-named-hashtable-table
))

(in-package :named-hash-tables)

(defclass named-hash-table ()
  ((name :initarg :name :accessor nh-name :initform (gensym "NH"))
   (table :initarg :table :accessor nh-table :initform (make-hash-table))))

(defun make-named-hash-table (name &key (test #+clisp 'fasthash-eql #-clisp 'eql)
                                   (size 1) (rehash-size 1.5)
                                   (rehash-threshold 0.75) documentation)
  (check-type name (or string symbol))
  (check-type size integer)
  (check-type rehash-size float)
  (let ((table
         (make-hash-table :test test :size size
                          :rehash-size rehash-size
                          :rehash-threshold rehash-threshold))
        (nh-instance (make-instance 'named-hash-table)))
    (setf (nh-name nh-instance) name
          (nh-table nh-instance) table)
    nh-instance))

(defmethod get-named-hash (key (table named-hash-table))
  "makes gethash from named hash-table"
  (gethash key (nh-table table)))

(defmethod get-named-name ((table named-hash-table))
  "get name from named hash-table"
  (nh-name table))

(defmethod get-named-table ((table named-hash-table))
  "get name from named hash-table"
  (nh-table table))

(defmethod rem-named-hash (key (table named-hash-table))
  "makes gethash from named hash-table"
  (remhash key (nh-table table)))

(defmethod set-named-hash (key value (table named-hash-table))
  "makes gethash from named hash-table"
  (setf (gethash key (nh-table table)) value))


(defmethod map-named-hash (function (table named-hash-table))
  "apply function to named hash-table"
  (maphash function (nh-table table)))

(defmethod named-hash-table-size ((table named-hash-table))
  "get named hash-table size"
  (hash-table-size (nh-table table)))

(defun named-hash-table-p (table)
  "returns T if object is named hash-table"
  (when (and
         (eq (class-of table) (find-class 'named-hash-table))
         (hash-table-p (nh-table table)))
    t))

(defmethod named-hash-table-count ((table named-hash-table))
  "returns elements count in named hash-table"
  (hash-table-count (nh-table table)))

(defmethod named-hash-table-rehash-size ((table named-hash-table))
  "returns hash-table-rehash-size for named hash-table"
    (hash-table-rehash-size (nh-table table)))

(defmethod named-hash-table-rehash-threshold ((table named-hash-table))
  "returns hash-table-rehash-threshold for named hash-table"
  (hash-table-rehash-threshold (nh-table table)))

(defmethod named-hash-table-test ((table named-hash-table))
  "returns hash-table-test for named hash-table"
    (hash-table-test (nh-table table)))

;; /DONE
;; TODO:
(defmethod ensure-named-gethash (key (table named-hash-table) &optional default)
  "Like GETHASH, but if KEY is not found in the HASH-TABLE saves the DEFAULT
under key before returning it. Secondary return value is true if key was
already in the table.
Main code used from alexandria"
  (ensure-gethash key (nh-table table) default))

(defmethod copy-named-hash-table ((table named-hash-table) new-name
                                  &key key test size
                                    rehash-size rehash-threshold)
  "Returns a copy of hash table TABLE, with the same keys and values
as the TABLE. The copy has the same properties as the original, unless
overridden by the keyword arguments.

Before each of the original values is set into the new hash-table, KEY
is invoked on the value. As KEY defaults to CL:IDENTITY, a shallow
copy is returned by default."
  (let ((hash-table-object (make-named-hash-table new-name))
        (hash-table-table (copy-hash-table (nh-table table)
                                           :key key
                                           :test test
                                           :size size
                                           :rehash-size rehash-size
                                           :rehash-threshold rehash-threshold)))
    (setf (nh-table hash-table-object)
          hash-table-table)
    hash-table-object))

(defmethod maphash-named-keys (function (table named-hash-table))
  "Like MAPHASH, but calls FUNCTION with each key in the hash table TABLE."
  (maphash-keys function (nh-table table)))

 (defmethod maphash-named-values (function (table named-hash-table))
   "Like MAPHASH, but calls FUNCTION with each value in the hash table TABLE."
   (maphash-values function (nh-table table)))

(defmethod named-hash-table-keys ((table named-hash-table))
  "Returns a list containing the keys of hash table TABLE."
  (hash-table-keys table))

(defmethod named-hash-table-values ((table named-hash-table))
  "Returns a list containing the values of hash table TABLE."
  (hash-table-values table))

(defmethod named-hash-table-alist ((table named-hash-table))
  "Returns an association list containing the keys and values of hash table
TABLE."
  (hash-table-alist (nh-table table)))

(defmethod named-hash-table-plist ((table named-hash-table))
  "Returns a property list containing the keys and values of hash table
TABLE."
  (hash-table-plist (nh-table table)))

(defmethod named-alist-hash-table ((table named-hash-table))
  "Returns a hash table containing the keys and values of the association list
ALIST. Hash table is initialized using the HASH-TABLE-INITARGS."
  (alist-hash-table (nh-table table)))

(defmethod named-plist-hash-table ((table named-hash-table))
    "Returns a hash table containing the keys and values of the property list
PLIST. Hash table is initialized using the HASH-TABLE-INITARGS."
  (plist-hash-table (nh-table table)))
