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

(defun lim-handle-string-sbfd ()
  (if (if (> (length lim-current-string) 2)
          (not (member (char-to-string last-command-event) lim-possible-char))
        (> (length lim-current-string) 4))
      (let ((candidates (cdr (car lim-optional-result)))
            current-item)
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
        (setq lim-current-word (car current-item)))
    (setq lim-optional-result (lim-get lim-current-string)
          lim-current-word (car (car lim-optional-result))
          lim-possible-char (cdr (assoc "completions" lim-optional-result))
          lim-current-pos 1)
    (if (functionp 'lim-show) (funcall 'lim-show))))

(autoload 'lim-use-package "lim-xixi" "Lightly input mehtod xixi")
(autoload 'lim-orderless-regexp "lim-tools")
(autoload 'lim-evil-find-mode "lim-tools")
(autoload 'lim-count-words "lim-tools")

(register-input-method
 "lim-sbfd" "euc-cn" 'lim-use-package
 "飞单" "声笔飞单顶功输入法" "lim-sbfd.txt")

(setq default-input-method "lim-sbfd")

(defun lim-load-sbfd ()
  (setq lim-punctuation-list (lim-read-punctuation lim-current-scheme))
  (setq lim-translate-function 'lim-punctuation-translate)
  (setq lim-handle-function 'lim-handle-string-sbfd)
  (with-eval-after-load 'evil (lim-evil-find-mode)))

(defvar lim-ascii-char (cons ?\' "‘’")
  "*Key used for `lim-insert-ascii'.")

(global-set-key "'" 'lim-insert-ascii)
(global-set-key "~" 'lim-insert-org-verbatim)

(add-hook 'lim-load-hook 'lim-load-sbfd)
