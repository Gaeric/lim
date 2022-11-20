;; lim-dylib.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(load-rs-module "~/.emacs.d/site-lisp/lim/target/release/liblim.so")

(defun lim-count-words ()
  "Count Chinese words between START and END."
  (interactive)
  (let (start end)
    (if (region-active-p)
        (progn
          (setq start (region-beginning)
                end (region-end)))
      (setq start (point-min)
            end (point-max)))
    (lim-do-count
     (buffer-substring-no-properties start end))))

(provide 'lim-dylib)
