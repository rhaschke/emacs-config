(add-hook 'c-mode-common-hook 'hs-minor-mode)

(require 'ecb)

(unless (featurep 'auto-complete-config)
  (message "auto-complete package not available")
  ; show completions when idle
  (add-to-list 'semantic-default-submodes 
					'global-semantic-idle-completions-mode))

; allow jumping to previously visited code blocks
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode) 
; maintain tag database
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode) 
; reparse buffer when idle
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode) 
; highlight current tag
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode) 
; summarize tag at point
;(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode) 
; show current function in header line
;(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode) 

;; Activate semantic
(semantic-mode 1)

;; submodules of semantic
(require 'semantic/bovine/c) ; support c/c++ parsing
(require 'semantic/bovine/make) ; support makefile parsing
(require 'semantic/bovine/el); support elisp parsing
(require 'semantic/ia)       ; interactive functions for semantic analyzer
(require 'semantic/decorate/include) ; decoration modes for include statements
(require 'semantic/db-ebrowse) ; ebrowse backend for semanticdb
(require 'semantic/idle)       ; idle scheduler

; show breadcrumbs in header line
(global-semantic-idle-breadcrumbs-mode 1)
(setq semantic-idle-breadcrumbs-format-tag-function 'semantic-format-tag-name)
(setq semantic-idle-breadcrumbs-format-tag-list-function 'semantic-idle-breadcrumbs--format-innermost-first)

; limit semantic search to these items
(setq semanticdb-find-default-throttle '(local project unloaded system))

; allow gnu global as backend for semanticdb
(when (and (require 'cedet-global nil 'noerror)
			  (cedet-gnu-global-version-check t))
  (semanticdb-enable-gnu-global-databases 'c++-mode))

; potentially intersting other packages: 
; (require 'semantic-tag-folding) ; allow folding of tags/functions
; (require 'eassist) ; http://www.emacswiki.org/emacs/EAssist

;; add system-wide include paths
(semantic-add-system-include "/vol/xcf/include" 'c++-mode)
(semantic-add-system-include "/vol/nirobots/include" 'c++-mode)
(semantic-add-system-include "/vol/nivision/include/icl-8.4" 'c++-mode)
;(semantic-add-system-include "/vol/rsb/include" 'c++-mode)

; Qt4 settings
(setq qt4-base-dir "/usr/include/qt4/")
(semantic-add-system-include qt4-base-dir 'c++-mode)
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "Qt/qconfig.h"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "Qt/qconfig-dist.h"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "Qt/qglobal.h"))

; cedet hook for c-mode: define auto-complete sources
(defun my-c-mode-cedet-hook ()
  ; use clang or semantic for auto-completion if available
  (cond 
	((boundp 'ac-source-clang) (add-to-list 'ac-sources 'ac-source-clang))
	((boundp 'ac-source-semantic) (add-to-list 'ac-sources 'ac-source-semantic)))
  (ecb-activate))
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
  (local-set-key "\C-c\C-r" 'semantic-symref-symbol)
)

;; load cedet key map
(add-hook 'c-mode-common-hook 'install-common-cedet-keys)
(add-hook 'lisp-mode-hook 'install-common-cedet-keys)
(add-hook 'scheme-mode-hook 'install-common-cedet-keys)
(add-hook 'emacs-lisp-mode-hook 'install-common-cedet-keys)

;; have nice decorations by default
(global-semantic-decoration-mode 1)

;;; cedet.el ends here

