;;; packages.el --- lantian layer packages file for Spacemacs.
;;
;; Copyright (c) 2018-2019 lantian
;;
;; Author:  <lantian@ZERO>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(autoload 'lim-use-package "lim-xixi" "Lightly input mehtod xixi")
(autoload 'lim-orderless-regexp "lim-tools")

(register-input-method
 "lim-xixi" "euc-cn" 'lim-use-package
 "淅淅" "淅淅顶功输入法" "lim-xixi.txt")
(setq default-input-method "lim-xixi")

(defun lim-active-xixi ()
  (setq lim-stop-function 'lim-overflow)
  (setq lim-punctuation-list (lim-read-punctuation lim-current-scheme))
  (setq lim-translate-function 'lim-punctuation-translate)
  (lim-evil-find-mode))

;; 载入输入法时即加载标点相关控制函数，保证不受影响
(setq lim-load-hook 'lim-active-xixi)
