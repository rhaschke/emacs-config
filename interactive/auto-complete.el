;; auto-complete mode
;; http://cx4a.org/software/auto-complete/manual.html
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories (concat config-dir "ac-dict"))
  (ac-config-default)
  (setq ac-auto-start 3) ; auto-start completion with this number of chars
  (setq ac-auto-show-menu 0.8) ; delay [s] for showing completion menu
  (ac-flyspell-workaround))

;; https://github.com/brianjcj/auto-complete-clang
(require 'auto-complete-clang)

;; https://github.com/Golevka/emacs-clang-complete-async
;(require 'auto-complete-clang-async)
;(defvaralias 'ac-source-clang 'ac-source-clang-async)

;; define some default command-line options for clang
(setq-default ac-clang-cflags '("-DHAVE_QT"))

;; https://github.com/mooz/auto-complete-c-headers
(defun rhaschke/ac-c-headers-init ()
  "setup auto-complete for C headers"
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (setq achead:include-directories 
		  (append (achead:get-include-directories-from-options
					  (semantic-clang-args-from-project))
					 semantic-dependency-system-include-path))
)
(add-hook 'ede-minor-mode-hook 'rhaschke/ac-c-headers-init)