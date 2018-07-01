(require 'lim-core)
(require 'lim-advanced)

(defun lim-overflow ()
  (interactive)
  (if (> (length lim-current-string) 2)
      (not (member (char-to-string last-command-event) lim-possible-char))
    (> (length lim-current-string) 5)))

(setq lim-stop-function 'lim-overflow)

(register-input-method
 "lim-xixi" "euc-cn" 'lim-use-package
 "淅淅" "淅淅顶功输入法" "lim-xixi.txt")

(provide 'lim-xixi)
