
;;; Global key bindings.

;;; switching buffers
(global-set-key [C-tab]    #'other-window)
(global-set-key [C-S-tab]  #'other-window-backwards)

;;; indentation
(defun indent-block (amount)
  "indent block by amount"
  (interactive "Nindentation amount: ")
  (indent-rigidly (region-beginning) (region-end) amount))
(global-set-key [M-left]   '(lambda () (interactive) (indent-block (* -1 standard-indent))))
(global-set-key [M-right]  '(lambda () (interactive) (indent-block (*  1 standard-indent))))

;;; cursor navigation
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert arg."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key "^"   'match-paren)
(global-set-key [C-^] 'match-paren)

(global-set-key [f1]    'help-for-help)
(global-set-key [M-f1]  'info)

(global-set-key [f2]    'save-buffer)
(global-set-key [f3]    'find-file)
(global-set-key [f4]    'kill-this-buffer)

(global-set-key [f5]    'hs-toggle-hiding)
(global-set-key [f6]    'hs-hide-level)
(global-set-key  (kbd "C-c <left>") 'hs-hide-block)
(global-set-key  (kbd "C-c <right>") 'hs-show-block)

(global-set-key [f8]    'next-error)
(global-set-key [f9]    'compile)
(global-set-key [f11]   'toggle-fullscreen)
(global-set-key [f12]   'save-buffers-kill-terminal)

(global-set-key "\C-v"  'x-clipboard-yank)

(global-set-key [home]  'beginning-of-visual-line)
(global-set-key [end]   'end-of-visual-line)

(global-set-key [f10]   'magit-status)
