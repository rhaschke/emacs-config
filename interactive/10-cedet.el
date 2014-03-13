;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html

(add-hook 'c-mode-common-hook 'hs-minor-mode)

(unless (featurep 'auto-complete-config)
  (message "auto-complete package not available")
  ; show completions when idle
  (add-to-list 'semantic-default-submodes 
					'global-semantic-idle-completions-mode))

(dolist (submode 
  '(global-semantic-mru-bookmark-mode ; navigation to previously visited tags
	 global-semanticdb-minor-mode ; maintain tag database
	 global-semantic-idle-scheduler-mode ; automatic parsing (+ other stuff) in idle time
	 global-semantic-highlight-func-mode ; highlight current tag
	 global-semantic-idle-summary-mode ; summarize tag at point
	 global-cedet-m3-minor-mode ; activate right-mouse context menu
	 global-semantic-idle-local-symbol-highlight-mode ; highlight local names matching current tag
	 ))
  (add-to-list 'semantic-default-submodes submode))

;; Activate semantic
(semantic-mode 1)

;; submodules of semantic
(require 'semantic/bovine/c) ; support c/c++ parsing
(require 'semantic/bovine/clang)
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

; allow gnu global as backend for semanticdb
(when (and (require 'cedet-global nil 'noerror)
			  (cedet-gnu-global-version-check t))
  (semanticdb-enable-gnu-global-databases 'c-mode t)
  (semanticdb-enable-gnu-global-databases 'c++-mode t))

; potentially intersting other packages: 
(require 'semantic-tag-folding) ; allow folding of tags/functions
(require 'eassist) ; http://www.emacswiki.org/emacs/EAssist

;; add system-wide include paths
(semantic-add-system-include "/vol/xcf/include" 'c++-mode)
(semantic-add-system-include "/vol/nirobots/include" 'c++-mode)
(semantic-add-system-include "/vol/nivision/include/icl-8.4" 'c++-mode)
;(semantic-add-system-include "/vol/rsb/include" 'c++-mode)

; Qt4 settings
(setq qt4-base-dir "/usr/include/qt4/")
(semantic-add-system-include qt4-base-dir 'c++-mode)
(dolist (file '("Qt/qconfig.h" "Qt/qconfig-dist.h" "Qt/qglobal.h"))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir file)))

; cedet hook for c-mode: define auto-complete sources
(defun my-c-mode-cedet-hook ()
  ; limit semantic search to these items
  (setq semanticdb-find-default-throttle '(local project unloaded system))
  ; use clang as semantic source
  (semantic-clang-activate)
  ; set c++ auto-completion source
  (add-to-list 'ac-sources 'ac-source-clang)
;  (add-to-list 'ac-sources 'ac-source-semantic)

  (make-local-variable 'ac-clang-flags)
  (set 'ac-clang-flags
		 (append ac-clang-flags 
					(semantic-clang-args-from-project)
					semantic-clang-system-includes))
)
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook t)

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

;; ecb
(require 'ecb)
(custom-set-variables
 '(ecb-auto-activate t))

;;; cedet.el ends here

