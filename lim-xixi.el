(defun lim-overflow ()
  (interactive)
  (if (> (length lim-current-string) 2)
      (not (member (char-to-string last-command-event) lim-possible-char))
    (> (length lim-current-string) 5)))

(setq lim-stop-function 'lim-overflow)
