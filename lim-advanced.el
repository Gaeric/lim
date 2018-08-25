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

(require 'lim-core)

;; ==============================================================================
(defun note ()
  "Record note as necessary."
  (interactive)
  (let ((object (read-from-minibuffer "object: "))
        (note (read-from-minibuffer "note: ")))
    (evil-open-below 1)
    (insert (format ";; %s :: %s" object note))
    (evil-force-normal-state)))

(defun message-info (&optional message-info message-log)
  (interactive)
  (message (format "%s return: %s" message-info message-log)))

;; ==============================================================================
;;; 输入法高级功能 - advanced function
;; ------------------------------------------------------------------------------
;; Variable declare
(defun lim-punctuation-translate (char)
  (lim-punc-translate lim-punctuation-list char))

(defvar lim-overlay nil "lim的overlay")
(defvar lim-punc-exception-list (number-sequence ?0 ?9) "不进行编码转译的特殊情况")
(defvar lim-punc-translate-status t "标点转译控制开关")
(defvar lim-guidance-status t "候选栏控制开关 ")

;; ------------------------------------------------------------------------------
;;; Overlay for lim
;; Setup overlay by `lim-setup-overlay'
;; Clear overlay by `lim-clear-overlay'
;; Delete overlay and the content by `lim-delete-overlay'
;; Show the overlay by `lim-show-overlay'
;; Format the possible phrase by `lim-format'
;; Show the guidance by `lim-guidance'


(defface lim-string-face '((t (:underline t)))
  "Face to show current string"
  :group 'lim)

(defun lim-setup-overlay ()
  "Bulid overlay for lim."
  (let ((pos (point)))
    (if (overlayp lim-overlay)
        (move-overlay lim-overlay pos pos)
      (setq lim-overlay (make-overlay pos pos))
      (if input-method-highlight-flag
          (overlay-put lim-overlay 'face 'lim-string-face)))))

(defun lim-clear-overlay ()
  "Clear the overlay, but reserved the text."
  (if (and (overlayp lim-overlay) (overlay-start lim-overlay))
      (delete-overlay lim-overlay)))

(defsubst lim-delete-overlay ()
  "Delete the text which in lim-overlay."
  (if (overlay-start lim-overlay)
      (delete-region (overlay-start lim-overlay)
                     (overlay-end   lim-overlay))))

(defun lim-show ()
  "Show the Input process presentation"
  (unless enable-multibyte-characters
    (setq lim-current-string ""
          lim-current-word "")
    (error "Can't input in unibyte buffer"))
  (lim-delete-overlay)
  (insert lim-current-word)
  ;; (lim-completion-prompt)
  (move-overlay lim-overlay (overlay-start lim-overlay) (point))
  ;;   (lim-guidance)
  (lim-format-guidance)
  ;; (let ((message-log-max nil))
  (message "%s" lim-guidance-item))

(defun lim-format (key pos total phrase)
  (let ((i 0))
    (format "%s[%d/%d]: %s"
            key pos total
            (mapconcat 'identity
                       (mapcar
                        (lambda (c)
                          (format "%d.%s " (setq i (1+ i))
                                  c))
                        phrase)
                       " "))))

(defun lim-guidance ()
  "展示候选栏"
  (if lim-guidance-status
      (let* ((phrase
              (if (= (length lim-current-string) 1)
                  (car lim-optional-result)
                  (mapcar
                   (lambda (a) (concat (car a) (cdr a)))
                   (cdr (car lim-optional-result)))))
             (total (length phrase))
             ;; (pos lim-current-pos)
             (pos 1))
        (setq lim-guidance-item
              (lim-format lim-current-string pos total phrase)))))

;; ------------------------------------------------------------------------------
;;; Pass the function to lim-translate-function and add punctuation translation control
;; 将函数传递给lim-translate-function加入标点符号转译控制
;; Read punctuation by `lim-read-punctuation'
;; Translate char to punctuation by `lim-punc-translate'

(defun lim-read-punctuation (scheme)
  "Read punctuation."
  (let ((lim-current-scheme scheme)
        buf punc-list punc)
    (setq buf (cdr (assoc "buffer" (car (lim-buffer-list)))))
    (save-excursion
      (set-buffer buf)
      (save-restriction
        (widen)
        (let ((region (lim-section-region "Punctuation")))
          (goto-char (car region))
          (while (< (point) (cdr region))
            (setq punc (lim-line-content))
            (if (> (length punc) 3)
                (error "标点不支持多个转换"))
            (add-to-list 'punc-list punc)
            (forward-line 1)))))
    punc-list))

(defun lim-punc-translate (punc-list char)
  "Translate punctuation."
  (if lim-punc-translate-status
      (cond
       ;; ((< char ? ) "")
       ((and lim-ascii-char
             (= char (car lim-ascii-char)))
        (char-to-string char))
       (t
        ;; (message "return: ok")
        (let ((str (char-to-string char))
              punc)
          (if (and (not (member (char-before) lim-punc-exception-list))
                   (setq punc (cdr (assoc str punc-list))))
              (progn
                (if (char-before)
                    ;; note :: Determine if the cursor is at the file header
                    (if (= char (char-before))
                                     (delete-char -1)))
                (if (= (safe-length punc) 1)
                    (car punc)
                  (setcdr (cdr punc) (not (cddr punc)))
                  (if (cddr punc)
                      (car punc)
                    (nth 1 punc))))
            str))))
    (char-to-string)))

;; ==============================================================================
(defsubst lim-delete-line ()
  (delete-region (line-beginning-position) (min (+ (line-end-position) 1)
                                                (point-max))))

(defun lim-bulid-table ()
  "Bulid table by this function. Require lim-core"
  (interactive)
  (save-restriction
    (let ((table (lim-section-region "Table"))
          (param (lim-section-region "Parameter"))
          (lastw "")
          first-char total-char currw)
      (narrow-to-region (car table) (cdr table))
      (perform-replace "[ \t]+$" "" nil t nil nil nil (point-min) (point-max))
      (sort-lines nil (point-min) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at "^[ \t]*$")
            (lim-delete-line)
          (setq currw (lim-encode-at-point))
          (add-to-list 'first-char (aref currw 0))
          (mapc (lambda (c) (add-to-list 'total-char c)) (append currw nil))
          (if (string= currw lastw)
              (delete-region (1- (point)) (+ (point) (length currw))))
          (setq lastw currw)
          (forward-line 1)))
      (narrow-to-region (car param) (cdr param))
      (goto-char (point-min))
      (insert "first-char=" (concat first-char) "\n"
              "total-char=" (concat total-char) "\n")
      (while (not (eobp))
        (if (or (looking-at "^first-char=")
                (looking-at "^total-char="))
            (lim-delete-line)
          (forward-line 1)))
      (if (looking-at "^$")
          (delete-backward-char 1)))))

(provide 'lim-advanced)
