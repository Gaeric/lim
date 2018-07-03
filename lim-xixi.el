;;; -*- coding: utf-8 -*-
;;; lim.el -- Ligthly Input Architecture for lantian-xixi input method

;; Compatibility: Emacs 26.1
;; Copyright 2018
;; Author: lantian
;; version 0.06
;; Description: Ligthly Input Architecture

;; Fork from Eim but refactor all code
;; All-version 0.06.000
;;; License: GPLv3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; ==============================================================================

(require 'lim-core)
(require 'lim-advanced)

(setq lim-stop-function 'lim-overflow)

(defun lim-overflow ()
  (interactive)
  (if (> (length lim-current-string) 2)
      (not (member (char-to-string last-command-event) lim-possible-char))
    (> (length lim-current-string) 5)))

(defvar lim-ascii-char (cons ?\' "‘’")
  "*Key used for `lim-insert-ascii'.")

(defun lim-insert-ascii ()
  (interactive)
  (if current-input-method
      (let (c)
        (message (format "自定义输入(空格%s, 回车%c): "
                         (cdr lim-ascii-char)
                         (car lim-ascii-char)))
        ;; (setq c (read-event)) change read-event to read-char
        (setq c (read-char))
        (cond ((= c ?\r) (progn
                           (insert (cdr lim-ascii-char))
                           (backward-char 1)))
              ((= c ? ) (insert-char (car lim-ascii-char) 1))
              (t
               (setq unread-command-events (list last-input-event))
               (insert (read-from-minibuffer "自定义输入: ")))))
    (call-interactively 'self-insert-command)))

;; ==============================================================================
(provide 'lim-xixi)
