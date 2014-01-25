(add-hook 'c-mode-common-hook 'hs-minor-mode)

; allow jumping to previously visited code blocks
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode) 
; maintain tag database
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode) 
; reparse buffer when idle
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode) 
; show completions when idle
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode) 
; highlight current tag
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode) 
; summarize tag at point
;(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode) 
; show current function in header line
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode) 

;; Activate semantic
(semantic-mode 1)

; allow gnu global database as backend for semanticdb
(semanticdb-enable-gnu-global-databases 'c++-mode)

; auto-complete mode
; http://cx4a.org/software/auto-complete/manual.html
(require 'auto-complete-config)
(ac-config-default)

; allow folding of tags/functions
; (require 'semantic-tag-folding) ; doesnt work

;; add system-wide include paths
(semantic-add-system-include "/vol/xcf/include" 'c++-mode)
(semantic-add-system-include "/vol/nirobots/include" 'c++-mode)
(semantic-add-system-include "/vol/nivision/include/icl-8.4" 'c++-mode)
;(semantic-add-system-include "/vol/rsb/include" 'c++-mode)

; Qt4 settings
(setq qt4-base-dir "/usr/include/qt4/")
(semantic-add-system-include qt4-base-dir 'c++-mode)
;(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "Qt/qconfig.h"))
;(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "Qt/qconfig-dist.h"))
;(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "Qt/qglobal.h"))

;; display information for tags & classes
;(require 'semantic/ia)
;(require 'semantic/decorate/include)
;(require 'semantic/lex-spp)

; cedet hook for c-mode: define auto-complete sources
(defun my-c-mode-cedet-hook ()
  ; add global/gtags, semantic as source for auto-completion
  (add-to-list 'ac-sources 'ac-source-gtags)
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;; customisation of modes
(defun install-common-cedet-keys ()
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-co" 'semantic-decoration-include-visit)

  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cb" 'semantic-mrub-switch-tags)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c <left>") 'semantic-tag-folding-fold-block)
  (local-set-key (kbd "C-c <right>") 'semantic-tag-folding-show-block))

;; load cedet key map
(add-hook 'c-mode-common-hook 'install-common-cedet-keys)
(add-hook 'lisp-mode-hook 'install-common-cedet-keys)
(add-hook 'scheme-mode-hook 'install-common-cedet-keys)
(add-hook 'emacs-lisp-mode-hook 'install-common-cedet-keys)

(defun install-c-mode-cedet-keys ()
  ; allow auto-completion for variable. or variable->
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref))

(add-hook 'c-mode-common-hook 'install-c-mode-cedet-keys)

;; have nice decorations by default
(global-semantic-decoration-mode 1)

;; EDE
;(global-ede-mode 1)
;(ede-enable-generic-projects)

;;; cedet.el ends here

