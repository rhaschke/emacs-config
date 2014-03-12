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
(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
               "
 /usr/include/c++/4.6
 /usr/include/c++/4.6/x86_64-linux-gnu/.
 /usr/include/c++/4.6/backward
 /usr/lib/gcc/x86_64-linux-gnu/4.6.1/include
 /usr/local/include
 /usr/lib/gcc/x86_64-linux-gnu/4.6.1/include-fixed
 /usr/include/x86_64-linux-gnu
 /usr/include
"
               )))
