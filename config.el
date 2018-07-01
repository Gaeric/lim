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
 "lim-xixi" "euc-cn" 'eim-use-package
 "淅淅" "淅淅顶功输入法" "lim-xixi.txt")
