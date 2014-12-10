;; auto-complete mode
;; http://cx4a.org/software/auto-complete/manual.html
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories (concat config-dir "ac-dict"))
  (ac-config-default)
  (setq ac-auto-start 3) ; auto-start completion with this number of chars
  (setq ac-auto-show-menu 0.8) ; delay [s] for showing completion menu
  (ac-flyspell-workaround))

;; https://github.com/brianjcj/auto-complete-clang
(when (featurep 'auto-complete-config)
  (require 'auto-complete-clang))

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
                 (and (featurep 'semantic-clang-args-from-project)
                      (semantic-clang-args-from-project)))
                semantic-dependency-system-include-path))
)

(defun mapprepend (prefix list)
  "prepend every string in list with prefix"
  (mapcar (lambda (x) (concat prefix x)) list))

(defun apply-ac-clang-settings (&optional buffer)
  (interactive)
  "setup ac-clang-flags from project project and system settings"
  (with-current-buffer (or buffer (current-buffer))
    (set 'ac-clang-cflags
         (append ac-clang-cflags 
                 semantic-clang-system-includes
                 (mapprepend "-include" semantic-lex-c-preprocessor-symbol-file)
                 (mapprepend "-I" semantic-dependency-system-include-path)
                 (semantic-clang-args-from-project)
                 semantic-clang-arguments))
    
    (when (fboundp 'ac-clang-launch-completion-process)
      (ac-clang-launch-completion-process))))
  

(when (featurep 'auto-complete-config)
  (add-hook 'ede-minor-mode-hook 'rhaschke/ac-c-headers-init))
(when (featurep 'semantic-clang-args-from-project)
  (add-hook 'ede-minor-mode-hook 'apply-ac-clang-settings))
