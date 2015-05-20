;;; -*- Mode: Lisp -*-

(defpackage :named-hash-tables-system
  (:use :cl :asdf))

(in-package :named-hash-tables-system)

(defsystem :named-hash-tables
  :name "NAMED-HASH-TABLES"
  :author "Alexander aka 'CosmonauT' Vynnyk <cosmonaut.ok@zoho.com>"
  :version "0.2"
  :maintainer "Alexander aka 'CosmonauT' Vynnyk <cosmonaut.ok@zoho.com>"
  :license "GNU General Public License v.3"
  :description "A tiny library for definition functions as objects for future expansion to ordinary functions"
  :serial t
  :components ((:file "named-hash-tables")))
