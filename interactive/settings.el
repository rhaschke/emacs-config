;; use chromium or w3 text browser
(if window-system
	 (setq browse-url-browser-function 'browse-url-generic
			 browse-url-generic-program "chromium-browser")
  (setq browse-url-browser-function 'browse-url-w3))

(defun rhaschke/setup-frame (&optional frame)
  (when window-system
	 (if (< (frame-pixel-width frame) 1500)
		  (setq ecb-windows-width 0.2)
		(setq ecb-windows-width 0.25))))

(add-hook 'window-size-change-functions 'rhaschke/setup-frame)

(defun rhaschke/common-text-mode-hook ()
  "function added to all text modes"
  (smerge-start-session)
  (goto-address-mode))

(add-hook 'text-mode-hook 'rhaschke/common-text-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'rhaschke/common-text-mode-hook)
(add-hook 'c-mode-common-hook 'rhaschke/common-text-mode-hook)
(add-hook 'python-mode-hook 'rhaschke/common-text-mode-hook)

;; turn off abbrev-mode
(abbrev-mode 0)