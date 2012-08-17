;; use default /etc/alternatives/x-www-browser or w3 text browser
(if window-system
	 (setq browse-url-browser-function 'browse-url-generic
			 browse-url-generic-program "x-www-browser")
  (setq browse-url-browser-function 'browse-url-w3))
