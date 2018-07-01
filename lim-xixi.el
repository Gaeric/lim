(require 'lim-core)
(require 'lim-advanced)

(setq lim-stop-function 'lim-overflow)

(defun lim-overflow ()
  (interactive)
  (if (> (length lim-current-string) 2)
      (not (member (char-to-string last-command-event) lim-possible-char))
    (> (length lim-current-string) 5)))

(provide 'lim-xixi)
