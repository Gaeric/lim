;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; lim.el -- Ligthly Input Architecture for lantian-xixi input method

;; Compatibility: Emacs 26.1
;; Copyright 2018
;; Author: lantian
;; version 0.07
;; Description: Ligthly Input Architecture

;; Fork from Eim but refactor all code
;; All-version 0.07.000
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
(defcustom lim-page-length 6 "the length of guidance every page")
;; Variable declare
(defun lim-punctuation-translate (char)
  (lim-punc-translate lim-punctuation-list char))

(defvar lim-overlay nil "lim的overlay")
(defvar lim-punc-exception-list (number-sequence ?0 ?9) "不进行编码转译的特殊情况")
(defvar lim-punc-translate-status t "标点转译控制开关")
(defvar lim-guidance-status nil "候选栏控制开关 ")
(defvar lim-prompt-number "" "编码对应可选词条的数目")
;;  (c . 出楚材))
(defvar lim-evil-char-cn-lib nil "储存由码表得到的字符汉字对应关系表")


;; ------------------------------------------------------------------------------
;;; Overlay for lim
;; Setup overlay by `lim-setup-overlay'
;; Clear overlay by `lim-clear-overlay'
;; Delete overlay and the content by `lim-delete-overlay'
;; Show the overlay by `lim-show-overlay'


(defface lim-string-face '((t (:underline t)))
  "Face to show current string"
  :group 'lim)

(defun lim-setup-overlay ()
  "Build overlay for lim."
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
  (if (and (overlayp lim-overlay) (overlay-start lim-overlay))
      (delete-region (overlay-start lim-overlay)
                     (overlay-end   lim-overlay))))

(defun lim-show ()
  "Show the Input process presentation"
  (unless enable-multibyte-characters
    (setq lim-current-string ""
          lim-current-word "")
    (error "Can't input in unibyte buffer"))
  (lim-delete-overlay)
  (if lim-current-word
      (insert (substring-no-properties lim-current-word 0 1)))
  ;; (lim-completion-prompt)
  (move-overlay lim-overlay (overlay-start lim-overlay) (point))
  (if lim-guidance-status
      ;; "控制是否显示候选栏"
        (lim-guidance)))

;; ------------------------------------------------------------------------------
;;; Guidance for lim
;; Format the possible phrase by `lim-format'
;; Show the guidance by `lim-guidance'
;; Toggle the guidance status by `lim-guidance-status-toggle'
(defun lim-guidance-status-toggle ()
  "toggle the status "
  (interactive "*")
  (if lim-guidance-status
      (setq lim-guidance-status nil)
    (setq lim-guidance-status t)))

(defun lim-guidance ()
  "展示候选栏"
      (let* ((phrase
              (if (= (length lim-current-string) 1)
                  (car lim-optional-result)
                (mapcar
                 (lambda (a) (concat (car a) (cdr a)))
                 (cdr (car lim-optional-result)))))
             (total (length phrase))
             ;; (pos lim-current-pos)
             (pos 1))
        (setq lim-prompt-number total)
        ;; 将可选词条的数目值赋给全局变量`lim-prompt-number', 且不影响原函数值
        ;; (lim-subseq (lim-guidance) (lim-page-start) (lim-page-end))
        ;; (message "%s" lim-optional-result)
        (setq lim-guidance-item
              (lim-format lim-current-string pos total ;; phrase
                          (lim-subseq phrase (1- (lim-page-start)) (lim-page-end)))))
      (let ((message-log-max nil))
        (message "%s" lim-guidance-item)))

(defun lim-format (key pos total phrase)
  (let ((i 0))
    (format "%s[%d/%d]: %s"
            key pos total
            (mapconcat 'identity
                       (mapcar
                        (lambda (c)
                          (format "%d.%s " (setq i (1+ i)) c)) phrase)
                       " "))))

(defun lim-subseq (list from &optional to)
  (if (null to)
      (nthcdr from list)
    (butlast (nthcdr from list) (- (length list) to))))

(defun lim-mod (x y)
  "like `mod', but when result is 0, return Y" 
  (let ((base (mod x y)))
    (if (= base 0)
        y
      base)))

(defun lim-current-page ()
  "计算当前词条所在的页数"
  (1+ (/ (1- lim-current-pos) lim-page-length)))

(defun lim-total-page ()
  (1+ (/ (1- lim-prompt-number) lim-page-length)))

(defun lim-page-start ()
  "计算当前所在页第一个词条的位置"
  (let ((pos (min lim-prompt-number lim-current-pos)))
    (1+ (- pos (lim-mod pos lim-page-length)))))

(defun lim-page-end ()
  "计算当前所在页的最后一个词条的位置"
  (let* ((whole lim-prompt-number)
         (len lim-page-length)
         (pos lim-current-pos)
         (last (+ (- pos (lim-mod pos len)) len)))
    (if (< last whole)
        last
      whole)))

;; ------------------------------------------------------------------------------
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
       ((if (boundp 'lim-ascii-char)
            (= char (car lim-ascii-char)))
        (char-to-string char))
       (t
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

(defun lim-build-table ()
  "Build table by this function. Require lim-core"
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

(defun lim-build-char-table ()
  "Output a library for lim-evil-find-char."
  (interactive)
  (save-restriction
    ;; 先置空处理
    (setq lim-evil-char-cn-lib nil)
    (let ((table (lim-section-region "Table"))
          (lim-char-string-temp  "") ;; 结构为 (b . 不避弼)
          currchar lastchar
          currline currword)
      (narrow-to-region (car table) (cdr table))
      (perform-replace "[ \t]+$" "" nil t nil nil nil (point-min) (point-max))
      (sort-lines nil (point-min) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at "^[ \t]*$")
            (lim-delete-line)
          (setq currline (lim-line-content))
          (setq currchar (substring-no-properties (car currline) 0 1))
          (setq currword (mapconcat
                          (lambda (x) (substring-no-properties x 0 1))
                          (cdr currline) ""))
          (if (string= currchar lastchar)
              ;; lim-char-string-temp 为一个字符的所有currword的拼接
              (setq lim-char-string-temp
                    (concat lim-char-string-temp currword))
            ;; 初始条件为空
            ;; 其余条件下，lim-char-string-temp均不为空
            ;; 在其为空时，不应做出任何改变
            ;; 在其不为空时，应当更新lim-evil-char-cn-lib
            (unless (string-empty-p lim-char-string-temp)
              (setq lim-evil-char-cn-lib (push
                                          (cons lastchar
                                                (delete-duplicates lim-char-string-temp))
                                          lim-evil-char-cn-lib)))
            ;; 在首字符变换时，重置lastchar和currword
            (setq lastchar currchar)
            (setq lim-char-string-temp  currword))
          (forward-line 1)))
      ;; 处理最后一组词
      ;; (setq lim-evil-char-cn-lib (push  lim-char-string-temp lim-evil-char-cn-lib))
      (setq lim-evil-char-cn-lib (push
                                  (cons lastchar (delete-duplicates  lim-char-string-temp))
                                  lim-evil-char-cn-lib)))))

(defun lim-evil-bulid-lib ()
  (interactive)
  (progn
    (insert (format "(defvar lim--evil-char-cn-lib"))
    (progn
      (insert "\n '(\n")
      (mapc
       (lambda (x) (insert (format "  (\"%s\" . \"%s\")\n" (car x) (cdr x))))
       lim-evil-char-cn-lib)
      (insert ")"))
    (insert (format "  \"字符汉字对应关系表\")"))))
;; 词库之中每一行可能有多个词
;; 只取每个词的第一个字
;; 根据每一行首字母判断是否要增加新的行

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


(defun lim-insert-org-verbatim ()
  (interactive)
  (if current-input-method
      (let (c)
        (message "verbatim(~): ")
        (setq c (read-char))
        (cond ((= c ?\r)
               (insert "~"))
              (t
               (setq unread-command-events (list last-input-event))
               (insert
                (concat "~" (read-from-minibuffer "verbatim: ") "~")))))
    (call-interactively 'self-insert-command)))


(provide 'lim-advanced)
