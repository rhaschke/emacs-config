;; emacs-lisp
(defun rhaschke/elisp-mode-hook ()
  (eldoc-mode 1))
(add-hook 'emacs-lisp-mode-hook 'rhaschke/elisp-mode-hook)

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


;; nxml-mode
(add-to-list 'auto-mode-alist '("\\.me$"  . nxml-mode))

(require 'rnc-mode)
(add-to-list 'auto-mode-alist '("\\.rnc$" . rnc-mode))

(require 'rng-loc)
(add-to-list 'rng-schema-locating-files "/vol/nirobots/share/schemas.xml")

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
   http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
   this.  The function inserts linebreaks to separate tags that
   have nothing but whitespace between them.  It then indents the
   markup by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end)))

(custom-set-variables
 '(nxml-auto-insert-xml-declaration-flag nil)
 '(nxml-slash-auto-complete-flag t))

; hide-show for nxml mode
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"         ;; regexp for start block
               "-->\\|</[^/>]*[^/]>"         ;; regexp for end block
					"<!--"                        ;; regexp for comment start.
               nxml-forward-element
               nil))

; enable auto-completion in nxml
(add-to-list 'ac-modes 'nxml-mode)
(defun rhaschke/nxml-mode-hook ()
  (hs-minor-mode))
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

;; octave mode
(defun rhaschke/octave-mode-hook ()
  (local-set-key [f9] 'octave-send-region))
(add-hook 'octave-mode-hook 'rhaschke/octave-mode-hook)
;; auto-associate .m files
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
