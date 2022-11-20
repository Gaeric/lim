;; lim-dylib.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(load-rs-module "./target/release/liblim.so")

(defun lim-count-words ()
  (interactive)
  (lim-do-count
   (buffer-substring-no-properties
    (point-min)
    (point-max))))
