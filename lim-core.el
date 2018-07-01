;;; -*- coding: utf-8 -*-
;;; lim.el -- Ligthly Input Architecture for lantian-xixi input method

;; Compatibility: Emacs 26
;; Copyright 2018
;; Author: lantian
;; version 0.01
;; Description: Ligthly Input Architecture

;; Fork from Eim but refactor all code
;; All-version 0.01.003
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
;;; 输入法变量声明

;;;_. group declare
(defgroup lim nil
  "lim: Ligthly input method"
  :group 'leim)

(defvar lim "v0.1")

;;;_. variable declare
;;===============================================================================
(defvar lim-package-list nil "所有可用的输入方案")
(defvar lim-first-char (number-sequence ?a ?z) "Table 中首字符列表")
(defvar lim-total-char (number-sequence ?a ?z) "Table 中所有字符列表")
;;===============================================================================
;;;_. My declare
(defvar lim-current-scheme (make-vector 5 nil)
  "The current input scheme. A vector consists of five parts.
当前使用的输入方案，一个向量[vector]，由五个部分组成:
package-name, buffer-list, history, keymap, active-function.

buffer-list 中的每一个buffer为如下形式的association List:
-------------------------------------------------------
buffer    对应的buffer名
param     Parameter 部分的参数
file      对应的文件名
-------------------------------------------------------
history 暂且不用
")

(defvar lim-current-string ""
  "The encoding being currently translated, which is a global variable.
即当前正在转译的编码，这是一个全局变量")

(defvar lim-optional-result nil "All optional terms.

可选单元组：包含可选的词条，以及相应的其它属性：
这个 list 的 CAR 是可选的词条，一般是一个字符串列表，但是也可以含有
list。但是这个 list 的第一个元素必须是将要插入的字符串。

CDR 部分是一个 Association list。通常含有这样的内容：
---------------------------
pos         上次选择的位置
completion  下一个可能的字符（如果 lim-completion-status 为 t）
")

(defvar lim-possible-char "" "下一个可能的字符")
(defvar lim-current-word "" "当前编码转译结果")
(defvar lim-current-pos nil "当前选择的词条在 lim-optional-reslut 中的位置")
(defvar lim-completion-status t "Control whether to complete. 控制是否进行补全")
(defvar lim-translate-status nil "转换状态控制开关")

(defvar lim-load-hook nil "载入输入法时调用的hook")
(defvar lim-active-hook nil "激活输入法时调用的hook")

(defvar lim-translate-function nil "额外的转换函数")
(defvar lim-stop-function nil "额外的控制函数，控制handle-function")
(defvar lim-handle-function 'lim-handle-string "控制函数，lim-handle-string
为字符串控制函数，用于处理输入字符串")

;;===============================================================================

(defvar lim-local-variable-list
  '(lim-current-scheme
    lim-first-char
    lim-total-char
    lim-completion-status

    lim-current-string
    lim-current-word
    lim-optional-result
    lim-current-pos
    lim-translate-status

    lim-load-hook
    lim-active-hook

    lim-handle-function
    lim-stop-function

    input-method-function
    deactivate-current-input-method-function)
  "A list of buffer local variable")

(dolist (var lim-local-variable-list)
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

;;;_. scheme contents
(defsubst lim-scheme-name ()
  "输入方案的具体名称，对应 lim-current-scheme 的第一个元素"
  (aref lim-current-scheme 0))

(defsubst lim-buffer-list ()
  "输入方案指定的buffer-list，对应lim-current-scheme的第二个元素"
  (aref lim-current-scheme 1))

(defsubst lim-history ()
  "保存输入过的词，以及当时选择的位置；对应lim-current-scheme的第三个元素
一方面，置入hash表加快搜索，另一方面，处理标点；Hash Table中的元素格式如下：
((list WORD) other-properties)
OTHER-PROPERTIES 是一些其它的属性，比如：上次的位置，用来优化标点输入"
  (aref lim-current-scheme 2))

(defsubst lim-mode-map ()
  "Lightly input method minor mode map.对应lim-current-scheme的第四个元素"
  (aref lim-current-scheme 3))

(defsubst lim-active-function ()
  "Active function. lim-current-scheme的第五个元素"
  (aref lim-current-scheme 4))

(defsubst lim-set-scheme-name (name)
  (aset lim-current-scheme 0 name))

(defsubst lim-set-buffer-list (buflist)
  (aset lim-current-scheme 1 buflist))

(defsubst lim-set-history (history)
  (aset lim-current-scheme 2 history))

(defsubst lim-set-mode-map (map)
  (aset lim-current-scheme 3 map))

(defsubst lim-set-active-function (act-func)
  (aset lim-current-scheme 4 act-func))




;; ==============================================================================
;;; 输入法核心功能 - core function

;; ------------------------------------------------------------------------------
;; Create and activate input method functions by `lim-use-package'
;; Check if all bufs exist in all buffer-lists by `lim-check-buffers'
;; Bulid variables by `lim-install-variable'
;; Deactivate the lim-input-mehtod by `lim-deactivate'

;;;_. lim interface
(defun lim-check-buffers ()
  "Check if all bufs exist in all buffer-lists.
If not, reopen the file. If the file does not exist, remove the buffer from the buffer-list.
检查所有buffer-list中的所有buf是否存在，
如果不存在，重新打开文件，如果文件不存在，则从buffer-list中删除这个buffer"
  (let ((buflist (lim-buffer-list))
        (bufname (lim-scheme-name))
        buffer file)
    (dolist (buf buflist)
      (unless (buffer-live-p (cdr (setq buffer (assoc "buffer" buf))))
        (if (file-exists-p (setq file (cdr (assoc "file" buf))))
            (with-current-buffer (format "*%s*" (generate-new-buffer bufname))
              (insert-file-contents file)
              (setcdr buffer (current-buffer)))
          (message "%s for %s is not exists!" file bufname)
          (setq buflist (remove buf buflist)))))
    t))

(defun lim-install-variable ()
  (let ((param (cdr (assoc "param" (car (lim-buffer-list))))))
    (mapc (lambda (p)
            (let ((sym (intern-soft (concat "lim-" (car p)))))
              (if sym
                  (set sym (mapconcat 'identity (cdr p) "=")))))
          param)
;;     (if (stringp lim-page-length)
;;       (setq lim-page-length (string-to-number lim-page-length)))
    (setq lim-first-char (append lim-first-char nil)
          lim-total-char (append lim-total-char nil))))

(defun lim-use-package (scheme-name &optional word-file active-func)
  "Create and activate input method functions.
Accept two parameters: word-file and active-func
word-file, given a dictionary file
active-funct, called every time you switch.
   建立并激活当前输入框架，接受两个参数，
word-file 为输入法指定的词库文件，
active-func 为每次切换至输入法时调用的相关函数。"
  (interactive)
  (mapc 'kill-local-variable lim-local-variable-list)
  (mapc 'make-local-variable lim-local-variable-list)
  ;; note :: rebulid all local variable
  (if (assoc scheme-name lim-package-list)
      (setq lim-current-scheme (cdr (assoc
                                     scheme-name lim-package-list)))
    ;; make lim-current-scheme
    (setq lim-current-scheme (make-vector 9 nil)))
  (if (functionp active-func)
      (funcall active-func))
  ;; note :: 如果相关函数存在，调用之
  (unless (and (lim-scheme-name)
               (lim-check-buffers))
    ;; note :: the result of check is nil, and scheme-name is nil
    (if (and word-file
             (if (file-exists-p (expand-file-name word-file))
                 (setq word-file (expand-file-name word-file))
               (setq word-file (locate-file word-file load-path))))
        (progn
          (lim-set-scheme-name scheme-name)
          (lim-set-buffer-list (lim-load-file word-file))
          (lim-set-history (make-hash-table :test 'equal))
          (lim-set-mode-map (let ((map (make-sparse-keymap)))
                              (set-keymap-parent map lim-mode-map)
                              map))
          (add-to-list 'lim-package-list (cons scheme-name lim-current-scheme))
          (let ((param (cdr (assoc "param" (car (lim-buffer-list))))))
            (if (assoc "lib" param)
                (load (cadr (assoc "lib" param)))))
          (run-hooks 'lim-load-hook)
          (message nil))
      (error "没有这个文件： %s" word-file)))
  (lim-install-variable)
  (setq input-method-function 'lim-input-method)
  (setq deactivate-current-input-method-function 'lim-deactivate)
  (setq describe-current-input-method-function 'lim-help)
  ;; IF we are in minibuffer, turn off the current input mehtod
  ;; before exiting
  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'lim-exit-from-minibuffer))
  (run-hooks 'lim-active-hook)
  (if (functionp (lim-active-function))
      (funcall (lim-active-function))))

(defun lim-deactivate ()
  (setq lim-translate-status nil)
  (setq input-method-function nil)
	(kill-local-variable 'input-method-function))

;; ------------------------------------------------------------------------------
;; Create and define keymap by `lim-mode-map'
;; Decided to switch actions
;; Find the corresponding entry in char list by `lim-entry-command'
;; Choose the default term by `lim-select-current-term'
;; Delete the last char by `lim-delete-last-char'
;; Exit this translation and insert the entered code by `lim-quit-reserved'
;; Exit this translation and clear the entered code by `lim-quit-clear'
;; Terminate this translation by `lim-terminate-translation'

(defvar lim-mode-map
  (let ((map (make-sparse-keymap))
        ;; make-sparse-keymap :: Construct and return a new sparse keymap.
        (i ?\ ))
    ;; i :: 将i设置为32
    (while (< i 127)
      (define-key map (char-to-string i) 'lim-entry-command)
      (setq i (1+ i)))
    (setq i 128)
    (while (< i 256)
      (define-key map (vector i) 'lim-entry-command)
      ;; vector :: set vertor to lim-entry-command
      (setq i (1+ i)))
    (dolist (i (number-sequence ?1 ?9))
      (define-key map (char-to-string i) 'lim-entry-command))
    (define-key map " "         'lim-select-current-term)
    (define-key map [backspace] 'lim-delete-last-char)
    (define-key map [delete]    'lim-delete-last-char)
    (define-key map "\177"      'lim-delete-last-char)
    (define-key map "\C-m"      'lim-quit-reserved)
    (define-key map "\C-c"      'lim-quit-clear)
    (define-key map "\C-g"      'lim-quit-clear)
    map)
  "Keymap")

(defun lim-entry-command ()
  "Find the corresponding entry in char list.
otherwise stop the conversion,then insert the corresponding character.
在char列表中找到相应的条目，否则停止转换，然后插入相应的字符。"
  (interactive "*")
  ;; (message "%s" (current-buffer))
  (if (if (string-empty-p lim-current-string)
          (member last-command-event lim-first-char)
        ;; lim-first-char :: 局部变量中，值为word-file中的参数
        ;; note :: 判断第一个输入字符是否在first-char列表中
        (member last-command-event lim-total-char))
      ;; note :: 判断输入字符是否在total-char列表中
      (progn
        (setq lim-current-string (concat lim-current-string (char-to-string last-command-event)))
        (funcall lim-handle-function))
    (setq lim-current-word (char-to-string last-command-event))
    ;; 处理(插入)不经转译的字符
    (lim-insert-current-word)))

(defun lim-select-current-term ()
  "Choose the default term.
提请插入当前默认的词条，视情况处理字符串，这个函数默认绑定在空格上，绑定关系见 lim-mode-map"
  (interactive)
  (if (string-empty-p lim-current-string)
      ;; lim-current-string 为空 :: 直接转为空格，然后赋给lim-current-word
      (setq lim-current-word (char-to-string last-command-event))
    (if (null (car lim-optional-result))
        ;; lim-current-string :: 可选词条为空，则置空值赋给lim-current-word
        ;; 然后插入lim-current-word
        ;; 如果不为空，则不管
        (setq lim-current-word "")))
    (lim-insert-current-word))

      ;; (null (car lim-optional-result))
      ;; t :: lim-optional-result中CAR部分为空
      ;; note :: 即默认词条为空
  ;; (setq lim-current-word "")
  ;; (setq lim-current-string "")

(defun lim-insert-current-word ()
  (unless (string-empty-p lim-current-word)
    ;; 只有当 lim-current-word 不为空时，执行插入操作
      (insert lim-current-word))
  ;; terminate :: 插入结束后，重新开始转译
  (lim-terminate-translation))

(defun lim-delete-last-char ()
  "如果lim-current-string值不存在时，直接返回相应的 (list key)
系统本身会对字符进行处理"
  (interactive)
  (if (> (length lim-current-string) 1)
      (progn
        (setq lim-current-string (substring lim-current-string 0 -1))
        (funcall lim-handle-function))
    (setq lim-current-string "")
    (lim-terminate-translation)))

(defun lim-quit-reserved ()
  "Exit this translation and insert the entered code."
  (interactive)
  (setq lim-current-word lim-current-string)
  (insert lim-current-word)
  (lim-terminate-translation))

(defun lim-quit-clear ()
  "Exit this translation and clear the entered code."
  (interactive)
  (setq lim-current-word "")
  (lim-terminate-translation))

(defun lim-terminate-translation ()
  "Terminate this translation."
  (setq lim-translate-status nil)
  (setq lim-optional-result nil)
  (setq lim-current-word "")
  (setq lim-current-string ""))
  ;; (lim-deactivate)

;; ------------------------------------------------------------------------------
;; load directory file
;; Decompose the current row by `lim-line-content'.
;; Gets positions of a section by `lim-section-region'
;; Loading file for lim package by `lim-load-file'
;; Reading FILE into buffer NAME by `lim-read-parameters'
;; Get parameter in [Parameter] section by `lim-read-parameters'

(defun lim-section-region (section)
  "Gets the start and end positions of a section, without last blank row.
得到一个部分的起点和终点位置，忽略未尾的空行；
各个部分之间以 [section] 的形式划分。"
  (let ((regexp-section (concat "^\\[" section "\\]\n")))
    (save-excursion
      (if (not (re-search-forward regexp-section nil t))
          (if (re-search-backward regexp-section nil t)
              (forward-line 1)
            (error "文件类型错误！没有 %s 部分！" section)))
      (cons (point) (progn
                      (if (re-search-forward "^\\[\\sw+\\]\n" nil t)
                          ;; search: Find the next part, if it's not exist
                          ;; jump to the end of the file
                          (forward-line -1)
                        (goto-char (point-max)))
                      (re-search-backward "[^  \t\n]" nil t)
                      (1+ (point)))))))

(defun lim-line-content (&optional seperaters omit-null)
  "Decompose the current row by split-string.
调用split-string 函数，以 seperaters 为分隔符分解当前行"
  (let ((items (split-string
                (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))
                seperaters)))
    (if omit-null
        (setq items (lim-condition-delete 'string-empty-p items))
      items)))

(defun lim-condition-delete (predicate seq)
  "Remove all items satisfying in SEQ.
The function emms-delete-if has some Bug."
  (let ((pilot nil)
        (len (length seq))
        (count 0)
        ele)
    (while (< count len)
      (setq ele (nth count seq))
      (if (not (funcall predicate ele))
          (setq pilot (append pilot (list ele))))
      (setq count (1+ count)))
    pilot))


;;;_. read file functions
(defun lim-load-file (file)
  "Loading file for lim scheme."
  (let ((bufname (format " *%s*" (lim-scheme-name)))
        ;; bufname: 空格可使缓冲区不可见
        buflist buf param other-files)
    (save-excursion
      (setq buf (lim-read-file file bufname t))
      (setq param (cdr (assoc "param" buf)))
      (setq buflist (append buflist (list buf)))
      (when (cdr (setq other-files (assoc "other-files" param)))
        ;; note :: 增加判断other-file=为空的情况
        (setq other-files (split-string (cadr other-files) "|"))
        ;; split-string: 用`|'分隔各个词库，中间不留空格
        (dolist (other-file other-files)
          ;; dolist :: 将ohter-files列表中的每一个元素赋给other-file，然后执行 body 函数
          (if
              ;; if :: 增加判断 other-files 的参数格式和文件是否存在
              (if (file-exists-p (expand-file-name other-file))
              ;; file-exists-p :: Return t if file FILENAME exists.
              ;; expand-file-name :: Convert filename NAME to absolute, and canonicalize it.
              (setq other-file (expand-file-name other-file))
            (setq other-file (locate-file other-file load-path)))
              ;; note :: 用于读取所有的other-files中写的所有other-file
              (setq buflist (append buflist (list (lim-read-file other-file bufname))))
            (error "other-files 参数格式错误或文件不存在"))))
      buflist)))

(defun lim-read-file (file name &optional read-param)
  "Reading FILE into buffer NAME"
  (let (param region)
    (save-excursion
      (set-buffer (generate-new-buffer name))
      (insert-file-contents word-file nil nil nil t)
      ;; insert-file-contents: (insert-file-contents FILENAME &optional VISIT BEG END REPLACE)
      (if read-param
          (setq param (lim-read-parameters)))
      (setq region (lim-section-region "Table"))
      (narrow-to-region (car region) (cdr region))
      `(("buffer" . ,(current-buffer))
        ;; ` :: ,: means insert the VAR
        ("param" . ,param)
        ("file" . ,file)))))

(defun lim-read-parameters ()
  "Get parameter in [Parameter] section, return assoc list.
得到 [Parameter] 部分的参数，以assoc list 的形式返回"
  (let* ((region (lim-section-region "Parameter"))
         param pair)
    (goto-char (car region))
    (while (< (point) (cdr region))
      (when (setq pair (lim-line-content "=" t))
        (add-to-list 'param pair))
      (forward-line 1))
    param))

;; ------------------------------------------------------------------------------
;; Search code, return word and completion.
;; Get the next possible input character by `lim-completions'
;; Seek word by `lim-seek-word'
;; Get the encoding of the current line by `lim-encode-at-point'
;; Search code, return word and completion by `lim-get'

(defun lim-completions (code completions)
  "Get the next possible input character"
  (let ((maxln 200)
        (count 0)
        (len (length code))
        (reg (concat "^" (regexp-quote code))))
    ;; regexp-quote :: Return a regexp string which matches exactly STRING and nothing else.
    (save-excursion
      (forward-line 1)
      ;; ?? :: why need to forward-line 1
      (while (and (looking-at reg)
                  ;; looking-at :: Return t if text after point matches regular expression REGEXP.
                  (< count maxln))
        (add-to-list 'completions
                     (buffer-substring-no-properties
                      (+ (point) len)
                      (+ (point) len 1)))
        (forward-line 1)
        (setq count (1+ count)))
      completions)))

(defun lim-seek-word (code start end)
  "Seek the word by code at start and end"
  (let ((mid (/ (+ start end) 2))
        encode)
    (goto-char mid)
    (setq encode (lim-encode-at-point))
    (if (string= code encode)
        (lim-line-content)
      (if (> mid start)
          (if (string< encode code)
              (lim-seek-word code mid end)
            (lim-seek-word code start mid))))))

(defun lim-encode-at-point ()
  "Get the encoding of the current line.
得到当前行的编码"
  (beginning-of-line)
  ;; note :: Be sure the point is at the beginning of line
  (save-excursion
    (if (re-search-forward "[ \t]" (line-end-position) t)
        (buffer-substring-no-properties (line-beginning-position) (1- (point)))
      (error "文件格式错误！%s 的第 %d 行没有词条！" (buffer-name) (line-number-at-pos)))))


(defun lim-get (code)
  (when (and (stringp code) (not (string-empty-p code)))
    (let ((history (gethash code (lim-history)))
          ;; gethash :: Look up Key in TABLE and return its associated value.
          pos words completions)
      (if (and (car history) (assoc "completions" (cdr history)))
          ;; history :: get history from hash table
          history
        (dolist (buf (lim-buffer-list))
          (with-current-buffer (cdr (assoc "buffer" buf))
            (setq words (append words
                                 (cdr
                                  (lim-seek-word code
                                                 (point-min)
                                                 (point-max)))))
            (if lim-completion-status
                (setq completions (lim-completions code completions)))))
        (setq words (delete-dups words))
        ;; delete-dups: delete the duplicate items
        (puthash code (list words
                            (cons "pos" (or (cdr (assoc "pos" (cdr history))) 1))
                            (cons "completions" completions))
                (lim-history))))))

;; ------------------------------------------------------------------------------
;; Core functions to complete the core functions of the input method.
;; Control input-flow by `lim-input-method'
;; Get standard input by `lim-obtain-string'
;; Splicing encoding strings and get entry  by `lim-handle-string'
;; Conversion char to string by `lim-translate'

(defun lim-input-method (key)
  "The core function of input-method,
Call obtain function to get the encode string
handle-string to control the translation string
complete the related function control completion
and call to get the related function to obtain the word translation result.
输入法核心函数：
调用obtain相关函数获取编码字符串
调用handle相关函数控制转译状态并分割转译字符串
调用get相关函数获取转译后的词条
调用completion相关函数获取补全结果"
  (if (or buffer-read-only
          overriding-terminal-local-map
          overriding-local-map)
      (list key)
    (let ((modified-p (buffer-modified-p)))
      ;; (buffer-undo-list t)
      ;; (inhibit-modification-hooks t)
      ;; note :: maybe they are not nessccary
      (unwind-protect
          (let ((input-string (lim-obtain-string key)))
            ;; first step :: Get input-string ?
            (when (and (stringp input-string)
                       (> (length input-string) 0))
              (if input-method-exit-on-first-char
                  (list (aref input-string 0))
                ;; (lim-input-string-to-events input-string)
                ;; string-to-events :: delete this function
                )))
        ;; (lim-delete-overlays)
        ;; delete-overlays :: 删除overlay, 保留插入字符
	      (run-hooks 'input-method-after-insert-chunk-hook)))))

(defun lim-obtain-string (key)
  "start translation of the typed character KEY by the Current Scheme.
Return the input string."
  ;; Check the possibility of translation KEY.
  ;; If KEY is nil, we can anyway start obtain.
  (if (or (integerp key) (null key))
      ;; condition :: Key is non-nil or an integer character
      ;; OK, we can start obtain the input-string
      (let* ((echo-keystrokes 0)
             (help-char nil)
             (overriding-terminal-local-map (lim-mode-map))
             (input-method-function nil)
             (modified-p (buffer-modified-p))
             last-command-event last-command this-command)
        (setq lim-current-word ""
              lim-current-string ""
              lim-translate-status t)
        ;; note :: Initialize related variables
        (if key
            ;; condition :: key is non-nil
            (setq unread-command-events
                  (cons key unread-command-events)))
        (while lim-translate-status
          ;; translate-status :: 控制是否继续从标准输入读取编码
          (set-buffer-modified-p modified-p)
          (let* ((prompt (if t
                             ;; input-method-use-echo-area
                             (format "[%s]:%s %s"
                                     ;; (or input-method-previous-message "")
                                     lim-current-string
                                     lim-current-word
                                     lim-current-pos
                                     )))
                 (keyseq (read-key-sequence prompt nil nil t))
                 (cmd (lookup-key (lim-mode-map) keyseq)))
            (if (if key
                    (commandp cmd)
                  (eq cmd 'lim-entry-command))
                (progn
                  ;; (message "keyseq: %s" keyseq)
                  (setq last-command-event (aref keyseq (1- (length keyseq)))
                        last-command this-command
                        this-command cmd)
                  (setq key t)
                  (condition-case err
                      (call-interactively cmd)
                    (error (message "%s" (cdr err)) (beep))))
              ;; KEYSEQ is not defined in the lim-mode-map.
              ;; Let's return the event(s) to the caller.
              (setq unread-command-events
                    (string-to-list (this-single-command-raw-keys)))
              ;; (message "unread-command-events: %s" unread-command-events)
              (lim-terminate-translation))))
        lim-current-word)
    ;; Since KEY doesn't start nay translation, just return it.
    ;; But translate key if necessary
    (char-to-string key)))

(defun lim-handle-string ()
  (if (and (functionp lim-stop-function)
           (funcall   lim-stop-function))
      ;; (if (> (length lim-current-string) 2)
      ;;     (not (member (char-to-string last-command-event) lim-possible-char))
      ;;   (> (length lim-current-string) 5))
      (progn
        (message "success")
        (setq unread-command-events
              (list (aref lim-current-string (1- (length lim-current-string)))))
        ;; lim-current-encode :: 已经输入的编码
        (lim-select-current-term))
    (setq lim-optional-result (lim-get lim-current-string)
          lim-current-word (car (car lim-optional-result))
          lim-possible-char (cdr (assoc "completions" lim-optional-result))
          lim-current-pos 1)))

(defun lim-translate (char)
  (if (functionp lim-translate-function)
      (funcall lim-translate-function)
    (char-to-string char)))
;; ==============================================================================
(provide 'lim-core)



(defun lim-overflow ()
   (interactive)
   (if (> (length lim-current-string) 2)
       (not (member (char-to-string last-command-event) lim-possible-char))
     (> (length lim-current-string) 5)))

 (setq lim-stop-function 'lim-overflow)


;; ==============================================================================
(defsubst lim-delete-line ()
  (delete-region (line-beginning-position) (min (+ (line-end-position) 1)
                                                (point-max))))

(defun lim-bulid-table ()
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
