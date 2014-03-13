; auto-complete mode
; http://cx4a.org/software/auto-complete/manual.html
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  (setq ac-auto-start 3) ; auto-start completion with this number of chars
  (setq ac-auto-show-menu 0.8) ; delay [s] for showing completion menu
  (ac-flyspell-workaround))

; https://github.com/brianjcj/auto-complete-clang
(require 'auto-complete-clang)

; https://github.com/Golevka/emacs-clang-complete-async
;(require 'auto-complete-clang-async)
;(defvaralias 'ac-source-clang 'ac-source-clang-async)
;(defvaralias 'ac-clang-flags 'ac-clang-cflags)
;(add-hook 'c-mode-common-hook 'ac-clang-launch-completion-process)

(custom-set-variables
 '(ac-clang-flags '("-DHAVE_QT")))
 
