;;; packages.el --- lantian layer packages file for Spacemacs.
;;
;; Copyright (c) 2018 lantian
;;
;; Author:  <lantian@ZERO>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(add-to-list 'load-path "~/.spacemacs.d/layers/lim/")
(autoload 'lim-use-package "lim-xixi" "Lightly input mehtod xixi")

(register-input-method
 "lim-xixi" "euc-cn" 'lim-use-package
 "淅淅" "淅淅顶功输入法" "lim-xixi.txt")

(setq default-input-method "lim-xixi")

(defun lim-active-xixi ()
  (setq lim-punctuation-list (lim-read-punctuation lim-current-scheme))
  (setq lim-translate-function 'lim-punctuation-translate))
(setq lim-active-hook 'lim-active-xixi)

(global-set-key "'" 'lim-insert-ascii)
