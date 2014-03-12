;; use default /etc/alternatives/x-www-browser or w3 text browser
(if window-system
	 (setq browse-url-browser-function 'browse-url-generic
			 browse-url-generic-program "x-www-browser")
  (setq browse-url-browser-function 'browse-url-w3))

(defun rhaschke/setup-frame (&optional frame)
  (when window-system
	 (if (< (frame-pixel-width frame) 1500)
		  (setq ecb-windows-width 0.2)
		(setq ecb-windows-width 0.25))))

(add-hook 'window-size-change-functions 'rhaschke/setup-frame)
	 
;; turn off abbrev-mode
(abbrev-mode 0)