(letf ((f (concat config-site-dir "magit/magit-autoloads.el"))
		 (magit-dir (concat config-site-dir "magit/")))
  (when (file-exists-p f)
	 (add-to-list 'load-path (concat config-site-dir "git-modes/"))
	 (add-to-list 'load-path magit-dir)
	 (eval-after-load 'info
		'(progn (info-initialize)
				  (add-to-list 'Info-directory-list magit-dir)))
	 (load-file f)))
