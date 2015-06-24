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
  (goto-address-mode)
  (whitespace-mode))

(add-hook 'text-mode-hook 'rhaschke/common-text-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'rhaschke/common-text-mode-hook)
(add-hook 'c-mode-common-hook 'rhaschke/common-text-mode-hook)
(add-hook 'python-mode-hook 'rhaschke/common-text-mode-hook)
(add-hook 'cmake-mode-hook 'rhaschke/common-text-mode-hook)
(add-hook 'sh-mode-hook 'rhaschke/common-text-mode-hook)

;; delete trailing spaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; turn off abbrev-mode
(abbrev-mode 0)

;; customize whitespace mode
(custom-set-variables
 '(whitespace-style '(face tabs tab-mark space-before-tab empty)))
(custom-set-faces
 '(whitespace-tab ((t (:background "white smoke" :foreground "lightgray")))))

;; customize yasnippet 
(custom-set-variables
 '(yas/root-directory (let ((add (list (concat config-dir "snippets")))
                            (orig (if (listp yas/root-directory)
                                      yas/root-directory (list yas/root-directory))))
                        (nconc add orig))))
