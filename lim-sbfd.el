;; lim-sbfd.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3
(require 'lim-core)
(require 'lim-advanced)

(defun lim-handle-sbfd ()
  "sbfd handle current string"
  (let ((re (lim-get lim-current-string))
        (candidates (cdr (car lim-optional-result)))
        current-item)
    (if (caar re)
        (progn
          (setq 
           lim-current-word (caar re)
           lim-current-pos 1
           lim-optional-result re)
          (if (functionp 'lim-show) (funcall 'lim-show)))
      (setq current-item
            (cond
             ((equal ?a last-command-event) (nth 1 candidates))
             ((equal ?e last-command-event) (nth 2 candidates))
             ((equal ?u last-command-event) (nth 3 candidates))
             ((equal ?i last-command-event) (nth 4 candidates))
             ((equal ?o last-command-event) (nth 5 candidates))
             (t nil)))
        (lim-terminate-translation)
        (unless current-item
          (setq unread-command-events (cons last-command-event unread-command-events))
          (setq current-item (car candidates)))
        (setq lim-current-word (car current-item)))))

(autoload 'lim-use-package "lim-xixi" "Lightly input mehtod xixi")
(autoload 'lim-orderless-regexp "lim-tools")
(autoload 'lim-evil-find-mode "lim-tools")
(autoload 'lim-count-words "lim-tools")
(autoload 'lim-insert-ascii "lim-tools")
(autoload 'lim-insert-org-verbatim "lim-tools")

(register-input-method
 "lim-sbfd" "euc-cn" 'lim-use-package
 "飞单" "声笔飞单" "lim-sbfd.txt")

(setq default-input-method "lim-sbfd")

(defun lim-load-sbfd ()
  (setq lim-punctuation-list (lim-read-punctuation lim-current-scheme))
  (setq lim-convert-function 'lim-punctuation-translate)
  (setq lim-handle-function 'lim-handle-sbfd)
  (with-eval-after-load 'evil (lim-evil-find-mode)))

(defvar lim-ascii-char (cons ?\' "‘’")
  "*Key used for `lim-insert-ascii'.")

(global-set-key "'" 'lim-insert-ascii)
(global-set-key "~" 'lim-insert-org-verbatim)

(add-hook 'lim-load-hook 'lim-load-sbfd)
