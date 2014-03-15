;;; Helge's txf files
(add-to-list 'auto-mode-alist '("\\.txf$" . org-mode))

(defun org-latex-export-to-pdf-and-open ()
  "Export to pdf and open the file"
  (interactive)
  (save-excursion
	 (goto-char (point-min))
	 (if (re-search-forward "#\\+LATEX_CLASS: *beamer" nil t)
		  (org-open-file (org-beamer-export-to-pdf))
		(org-open-file (org-latex-export-to-pdf)))))

(defun rhaschke/org-mode-hook ()
  (if (and (buffer-file-name)
			  (string= "txf" (file-name-extension (buffer-file-name))))
		(setq outline-regexp "[+\f]+"))
  (require 'ox-latex) ; load latex export functions
  (require 'ox-beamer) ; enable beamer export
  (local-set-key [f9] 'org-latex-export-to-pdf-and-open))
(add-hook 'org-mode-hook 'rhaschke/org-mode-hook)

(eval-after-load "org"
  '(progn
	  ;; Change .pdf association directly within the alist
	  (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))