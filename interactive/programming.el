;;; emacs-lisp
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
  (c-set-offset 'case-label 2)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'arglist-intro 2))

(add-to-list 'auto-mode-alist '("/drupal.*\\.\\(php\\|module\\|inc\\|test\\|install\\)$" . drupal-mode))
(add-to-list 'auto-mode-alist '("/drupal.*\\.info" . conf-windows-mode))


;;; google protocol buffer mode
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))


;;; octave mode
(defun rhaschke/octave-mode-hook ()
  (local-set-key [f9] 'octave-send-region))
(add-hook 'octave-mode-hook 'rhaschke/octave-mode-hook)
;; auto-associate .m files
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
