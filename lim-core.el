;;; -*- coding: utf-8 -*-
;;; lim.el -- Ligthly Input Architecture

;; Compatibility: Emacs 26.1
;; Copyright 2018
;; Author: lantian
;; version 0.07
;; Description: Ligthly Input Architecture

;; Fork from Eim but refactor all code
;; All-version 0.07.001
;;; License: GPLv3

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; ==============================================================================
(defvar lim-version "0.07")

(defgroup lim nil
  "lim: Ligthly input method"
  :group 'leim)

;;===============================================================================
(defvar lim-package-list nil "所有可用的输入方案")
(defvar lim-first-char (number-sequence ?a ?z) "Table 中首字符列表")
(defvar lim-total-char (number-sequence ?a ?z) "Table 中所有字符列表")
;;===============================================================================

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

(defvar lim-current-word "" "当前编码转译结果")
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
(defvar lim-possible-phrase nil "可能的字词组合")
(defvar lim-current-pos nil "当前选择的词条在 lim-optional-reslut 中的位置")

(defvar lim-completion-status t "Control whether to complete. 控制是否进行补全")
(defvar lim-translate-status nil "转换状态控制开关")
(defvar lim-completion-increase t "增强补全控制开关
不仅影响到补全函数是否查找下一个可能的字符对应的词组
还影响是否将其拼接入lim-optional-result")

(defvar lim-completion-limit 2 "补全控制条件")

(defvar lim-load-hook nil "载入输入法时调用的hook")
(defvar lim-active-hook nil "激活输入法时调用的hook")

(defvar lim-translate-function nil "额外的转换函数，目前用来处理标点")
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
    lim-possible-char
    lim-possible-phrase

    lim-load-hook
    lim-active-hook

    lim-handle-function
    lim-stop-function
    lim-overlay
    lim-guidance-item
    lim-prompt-number
    ;; lim-stop-function

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
    (if (stringp lim-page-length)
         (setq lim-page-length (string-to-number lim-page-length)))
    (setq lim-first-char (append lim-first-char nil)
          lim-total-char (append lim-total-char nil))))

(defun lim-use-package (scheme-name &optional word-file active-func)
  "Create and activate input method functions.
Accept two parameters: word-file and active-func
word-file, given a dictionary file
active-funct, called every time you switch."
  (interactive)
  (mapc 'kill-local-variable lim-local-variable-list)
  (mapc 'make-local-variable lim-local-variable-list)
  (if (assoc scheme-name lim-package-list)
      (setq lim-current-scheme (cdr (assoc
                                     scheme-name lim-package-list)))
    (setq lim-current-scheme (make-vector 9 nil)))
  (if (functionp active-func)
      (funcall active-func))
  (unless (and (lim-scheme-name) (lim-check-buffers))
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
          (run-hooks 'lim-load-hook))
      (error "File not exists： %s" word-file)))
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
;; load directory file
;; Decompose the current row by `lim-line-content'.
;; Gets positions of a section by `lim-section-region'
;; Loading file for lim package by `lim-load-file'
;; Reading FILE into buffer NAME by `lim-read-parameters'
;; Get parameter in [Parameter] section by `lim-read-parameters'
(defsubst string-empty-p (string)
  "Check whether STRING is empty."
  (string= string ""))

(defun lim-section-region (section)
  "Gets the start and end positions of a section, without last blank row."
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
  "Decompose the current row by split-string."
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
  (let ((bufname (format " *%s*" (lim-scheme-name))) ;; bufname: blank make buf hidden
        buflist buf param other-files)
    (save-excursion
      (setq buf (lim-read-file file bufname t))
      (setq param (cdr (assoc "param" buf)))
      (setq buflist (append buflist (list buf)))
      (when (cdr (setq other-files (assoc "other-files" param)))
        (setq other-files (split-string (cadr other-files) "|"))
        (dolist (other-file other-files)
          (if
              (if (file-exists-p (expand-file-name other-file))
                  (setq other-file (expand-file-name other-file))
                (setq other-file (locate-file other-file load-path)))
              (setq buflist (append buflist (list (lim-read-file other-file bufname))))
            (error "other-files invalid"))))
      buflist)))

(defun lim-read-file (file name &optional read-param)
  "Reading FILE into buffer NAME"
  (let (param region)
    (save-excursion
      (set-buffer (generate-new-buffer name))
      (insert-file-contents word-file nil nil nil t)
      (if read-param
          (setq param (lim-read-parameters)))
      (setq region (lim-section-region "Table"))
      (narrow-to-region (car region) (cdr region))
      `(("buffer" . ,(current-buffer))
        ("param" . ,param)
        ("file" . ,file)))))

(defun lim-read-parameters ()
  "Get parameter in [Parameter] section, return assoc list."
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
  "Get the next possible input character and word."
  (let ((maxln 200)                     ; search 200 times for speed
        (count 0)
        (len (length code))
        (reg (concat "^" (regexp-quote code)))
        item phrase)
  ;; regexp-quote :: Return a regexp string which matches exactly STRING and nothing else.
    (save-excursion
      (beginning-of-line)
      ;; note :: 由于前置已经执行了lim-seek-word
      ;; 此时光标处于当前输入编码对应的词条下，由于补全需要，从当前行开始进行补全
      (if lim-completion-increase
          (while (and (looking-at reg)
                      (< count maxln))
            (setq item (lim-line-content))
            (add-to-list 'completions
                         (buffer-substring-no-properties
                          (+ (point) len)
                          (+ (point) len 1)))
            ;; note :: Basic function
            (mapc (lambda (c)
                    (when (or (>= len lim-completion-limit)
                              ;; (= (length c) 1)
                              )
                      (push (cons c (substring (car item) len))
                            phrase)))
                  (cdr item))
            (forward-line 1)
            (setq count (1+ count)))
        (while (and (looking-at reg)
                    (< count maxln))
          (add-to-list 'completions
                       (buffer-substring-no-properties
                        (+ (point) len)
                        (+ (point) len 1)))
          (forward-line 1)
          (setq count (1+ count))))
      ;; note :: completion finish
      (setq lim-possible-phrase (sort (delete-dups (nreverse phrase))
                                      ;; note :: If phrase is nil, lim-possible-phrase is nil.
                                      (lambda (a b)
                                        (< (length (cdr a)) (length (cdr b))))))
      (if (= count maxln)
          (setq lim-completion-completed-status nil)
        (setq lim-completion-completed-status t))
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
  "Get the encoding of the current line."
  (beginning-of-line)
  ;; note :: Be sure the point is at the beginning of line
  (save-excursion
    (if (re-search-forward "[ \t]" (line-end-position) t)
        (buffer-substring-no-properties (line-beginning-position) (1- (point)))
      (error "File format Invalid ！Line %d is blank！" (buffer-name) (line-number-at-pos)))))


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
                                (list
                                 (cadr
                                  (lim-seek-word code
                                                 (point-min)
                                                 (point-max))))))
            (if lim-completion-status
                (setq completions (lim-completions code completions)))
            ;; (message "this is lim-possible-phrase: %s" lim-possible-phrase)
            ;; 由于切换了buffer，必须将增强补全函数写在dolist函数中，否则超出了变量的作用域
            (if lim-completion-increase (setq words (append words lim-possible-phrase)))))
        ;; note :: 必须将 lim-possible-phrase 赋给 words
        ;; 否则无法在下次查询到相同单词时刷候选栏
        ;; (message "this is words: %s" words)
        (setq words (delete-dups words))
        ;; delete-dups: delete the duplicate items
        (puthash code (list words
                            (cons "pos" (or (cdr (assoc "pos" (cdr history))) 1))
                            (cons "completions" completions))
                (lim-history))))))

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
        (i ?\ ))
    (while (< i 127)
      (define-key map (char-to-string i) 'lim-entry-command)
      (setq i (1+ i)))
    (setq i 128)
    (while (< i 256)
      (define-key map (vector i) 'lim-entry-command)
      (setq i (1+ i)))
    (dolist (i (number-sequence ?1 ?9))
      (define-key map (char-to-string i) 'lim-select-num-term))
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
otherwise stop the conversion,then insert the corresponding character."
  (interactive "*")
  (if (if (string-empty-p lim-current-string)
          (member last-command-event lim-first-char)
        (member last-command-event lim-total-char))
      (progn
        (setq lim-current-string (concat lim-current-string (char-to-string last-command-event)))
        (funcall lim-handle-function))
    ;; (setq lim-current-word (char-to-string last-command-event))
    (setq lim-current-word (lim-translate last-command-event))
    ;; (message "return word: %s" lim-current-word)
    (lim-terminate-translation)))

(defun lim-select-current-term ()
  "Choose the default term.
提请插入当前默认的词条，视情况处理字符串，这个函数默认绑定在空格上，绑定关系见 lim-mode-map"
  (interactive)
  (if (string-empty-p lim-current-string)
      ;; lim-current-string 为空 :: 直接转为空格，然后赋给lim-current-word
      (setq lim-current-word (char-to-string last-command-event))
    (if (null (car lim-optional-result))
        ;; t :: lim-optional-result中CAR部分为空
        ;; lim-current-string :: 可选词条为空，则置空值赋给lim-current-word
        ;; 如果不为空，则不管
        (setq lim-current-word "")))
  (lim-terminate-translation))

(defun lim-select-num-term ()
  "Choose the term by num."
  (interactive)
  (if (string-empty-p lim-current-string)
      (progn
        (setq lim-current-word (char-to-string last-command-event))
        (lim-terminate-translation))
    (let ((num (- last-command-event 48)))
      (message "%d" num)
      (if (and (car lim-optional-result)
               (nth num (car lim-optional-result)))
          (progn
            (setq lim-current-word (car (nth num (car lim-optional-result))))
            (lim-terminate-translation))
        (user-error "Ivalid number")))))

(defun lim-delete-last-char ()
  "如果lim-current-string值不存在时，直接返回相应的 (list key)"
  (interactive)
  (if (> (length lim-current-string) 1)
      (progn
        (setq lim-current-string (substring lim-current-string 0 -1))
        (funcall 'lim-handle-delete))
    (setq lim-current-word "")
    (lim-terminate-translation)))

(defun lim-quit-reserved ()
  "Exit this translation and insert the entered code."
  (interactive)
  (setq lim-current-word lim-current-string)
  ;; (insert lim-current-word)
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
  (setq lim-current-string "")
  (if (functionp 'lim-delete-overlay) (funcall 'lim-delete-overlay)))

;; ------------------------------------------------------------------------------
;; Core functions to complete the core functions of the input method.
;; Control input-flow by `lim-input-method'
;; Get standard input by `lim-obtain-string'
;; Splicing encoding strings and get entry  by `lim-handle-string'
;; Splicing encoding strings when delete by `lim-handle-delete'
;; Conversion input-string (current-word) to events by `lim-input-events'
;; Advise input characters by `lim-advice'
;; Conversion char to special string by `lim-translate'
;; If don't use overlay, the input method does not need to insert the post-translation entry itself
;; All insertion actions should generate input events and pass them to input-method-after-insert-chunk-hook to satisfy other features.

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
    (if (functionp 'lim-setup-overlay)
        (funcall 'lim-setup-overlay))
    (let ((modified-p (buffer-modified-p)))
      ;; (buffer-undo-list t)
      ;; (inhibit-modification-hooks t)
      ;; note :: maybe they are not nessccary
      (unwind-protect
          (let ((input-string (lim-obtain-string key)))
            (when (and (stringp input-string)
                       (> (length input-string) 0))
              (if input-method-exit-on-first-char
                  (list (aref input-string 0))
                ;; use lim-input-events to handle insert and other hook.
                (lim-input-events input-string))))
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
          (let* ((prompt (if input-method-use-echo-area
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
  "字符串控制函数，如果可能，转译当前编码，否则处理已拼接的编码，将最后一次输入的字符移交至unread-events处理"
  (if (and (functionp lim-stop-function)
           (funcall   lim-stop-function))
      (progn
        (setq unread-command-events
              (list (aref lim-current-string (1- (length lim-current-string)))))
        (lim-select-current-term))
    (setq lim-optional-result (lim-get lim-current-string)
          lim-current-word (car (car lim-optional-result))
          lim-possible-char (cdr (assoc "completions" lim-optional-result))
          lim-current-pos 1)
    (if (functionp 'lim-show) (funcall 'lim-show))))

(defun lim-handle-delete ()
  "字符串删减控制函数，转译删除后的编码"
    (setq lim-optional-result (lim-get lim-current-string)
          lim-current-word (car (car lim-optional-result))
          lim-possible-char (cdr (assoc "completions" lim-optional-result))
          lim-current-pos 1)
    (if (functionp 'lim-show) (funcall 'lim-show)))

(defun lim-input-events (str)
  "Convert input string STR to a list of events.
If STR has `advice' text property, append the following special event:
\(lim-advice STR)"
  (let ((events (mapcar
                 (lambda (l) l)
                 str)))
    (if (or (get-text-property 0 'advice str)
            (next-single-property-change 0 'advice str))
        (setq events
              (nconc events (list (list 'lim-advice str)))))
    events))

(defun lim-advice (args)
  "Advise users about the characters input by the current lim scheme."
  (interactive "e")
  (let* ((string (nth 1 args))
         (func (get-text-property 0 'advice string)))
    (if (functionp func)
        (funcall func string))))

(defun lim-translate (char)
  (if (functionp lim-translate-function)
      (funcall lim-translate-function char)
    (char-to-string char)))

;; ==============================================================================
(provide 'lim-core)
