;;; Drupal mode
(defun drupal-mode ()
  (require 'php-mode)
  (interactive)
  (php-mode)
  (setq c-basic-offset 3)
  (setq indent-tabs-mode nil)
  (setq fill-column 78)
  (c-set-offset 'case-label 2)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'arglist-intro 2))

(add-to-list 'auto-mode-alist '("/drupal.*\\.\\(php\\|module\\|inc\\|test\\|install\\)$" . drupal-mode))
(add-to-list 'auto-mode-alist '("/drupal.*\\.info" . conf-windows-mode))


;; XML mode
; replace xml-mode with nxml-mode
(fset 'xml-mode 'nxml-mode)
(add-to-list 'auto-mode-alist '("\\.me$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.hsm$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rnc$" . rnc-mode))
(require 'rnc-mode)
(require 'nxml-speedbar)
(add-to-list 'speedbar-supported-extension-expressions ".xml\\|.hsm")
(require 'nxml-script)
(require 'nxml-where)
(nxml-where-global-mode 1)

; hide-show for nxml mode
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"         ;; regexp for start block
               "-->\\|</[^/>]*[^/]>"         ;; regexp for end block
					"<!--"                        ;; regexp for comment start.
               nxml-forward-element
               nil))
(defun rhaschke/nxml-mode-hook ()
  (hs-minor-mode)
  (add-to-list 'rng-schema-locating-files "/vol/nirobots/share/schemas.xml")
  (rng-auto-set-schema))
(add-hook 'nxml-mode-hook 'rhaschke/nxml-mode-hook)

;; google protocol buffer mode
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; Helge's txf files
(add-to-list 'auto-mode-alist '("\\.txf$" . org-mode))

(defun rhaschke/org-mode-hook ()
  (if (and (buffer-file-name)
				 (string= "txf" (file-name-extension (buffer-file-name))))
		  (setq outline-regexp "[+\f]+"))
  (require 'org-exp-bibtex))
(add-hook 'org-mode-hook 'rhaschke/org-mode-hook)

;; elisp mode hook
(defun rhaschke/elisp-mode-hook ()
  (local-set-key [C-return ] 'complete-symbol))
(add-hook 'emacs-lisp-mode-hook 'rhaschke/elisp-mode-hook)

;; octave mode
(defun rhaschke/octave-mode-hook ()
  (local-set-key [f9] 'octave-send-region))
(add-hook 'octave-mode-hook 'rhaschke/octave-mode-hook)
;; auto-associate .m files
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
