; auto-complete mode
; http://cx4a.org/software/auto-complete/manual.html
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  (setq ac-auto-start 3) ; auto-start completion with this number of chars
  (setq ac-auto-show-menu 0.8) ; delay [s] for showing completion menu
  (ac-flyspell-workaround))