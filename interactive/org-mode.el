;;; Helge's txf files
(add-to-list 'auto-mode-alist '("\\.txf$" . org-mode))

(defun rhaschke/org-latex-export-to-pdf (&optional sync)
  "Export to pdf and open the file"
  (interactive)
  (save-excursion
	 (setq sync t)
	 (goto-char (point-min))
	 ;; decide on which export to use
	 (if (re-search-forward "#\\+LATEX_CLASS: *beamer" nil t)
		  (org-beamer-export-to-pdf (not sync))
		(org-latex-export-to-pdf (not sync)))))

(defun rhaschke/org-latex-export-to-pdf-and-open ()
  "Export to pdf and open the file"
  (interactive)
  (org-open-file (rhaschke/org-latex-export-to-pdf)))

(defun rhaschke/org-mode-hook ()
  (if (and (buffer-file-name)
			  (string= "txf" (file-name-extension (buffer-file-name))))
		(setq outline-regexp "[+\f]+"))
  (local-set-key [f9]   'rhaschke/org-latex-export-to-pdf)
  (local-set-key [S-f9] 'rhaschke/org-latex-export-to-pdf-and-open))
(add-hook 'org-mode-hook 'rhaschke/org-mode-hook)

(eval-after-load "org"
  '(progn
	  ;; Change .pdf association directly within the alist
	  (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))