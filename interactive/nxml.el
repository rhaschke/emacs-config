;; nxml-mode
(add-to-list 'auto-mode-alist '("\\.me$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rnc$" . rnc-mode))
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

(eval-after-load "nxml-mode"
  '(progn 
	  (require 'semantic/bovine/xml)
	  (require 'auto-complete-nxml)))

; enable auto-completion in nxml
(add-to-list 'ac-modes 'nxml-mode)

(defun rhaschke/nxml-mode-hook ()
  (hs-minor-mode))
  
(add-hook 'nxml-mode-hook 'rhaschke/nxml-mode-hook)
