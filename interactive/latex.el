;; Makes AUCTeX guess mode from file extension instead of content
(setq TeX-force-default-mode t)

;; auto-completion
(eval-after-load "auctex"
  '(progn (require 'ac-math)))

(when (featurep 'auto-complete-config)
  (add-to-list 'ac-modes 'LaTeX-mode)
  (add-to-list 'ac-modes 'latex-mode))

(defun rhaschke/latex-mode-hook ()
  (flyspell-mode 1)
  (local-set-key [C-tab] 'TeX-complete-symbol)
  (local-set-key [f8] 'TeX-next-error)
  (local-set-key [f9] 'TeX-command-master)
  (setq ac-sources '(ac-source-dictionary 
          ac-source-words-in-same-mode-buffers
          ac-source-math-unicode
          ac-source-math-latex 
          ac-source-latex-commands
          ac-source-filename))
)
(add-hook 'LaTeX-mode-hook 'rhaschke/latex-mode-hook)

(setq TeX-auto-save nil)
(setq TeX-parse-self t)
(setq-default TeX-master 'shared)
(setq TeX-PDF-mode t)

;; default pdf viewer
(setq TeX-view-program-selection '((output-pdf "Evince")))

; enabling PDF inverse search, listening to dbus signals:
; http://www.emacswiki.org/emacs/AUCTeX#toc19
(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (url-unhex-string
   (if (string= (substring fname-or-url 0 8) "file:///")
       (substring fname-or-url 7)
     fname-or-url)))
(defun evince-sync (url linecol &rest ignored)
  "callback method for dbus messages from evince"
  (let* ((file (un-urlify url))
         (buf (find-buffer-visiting file))
         (line (car linecol))
         (col (cadr linecol)))
    (if (null buf) (find-file file) (switch-to-buffer buf))
    (goto-line (car linecol))
    (unless (= col -1) (move-to-column col))
    ;(select-frame-set-input-focus (window-frame (selected-window)))))
    (raise-frame (window-frame (selected-window)))))

(require 'dbus)
(when (and (eq window-system 'x)
           (fboundp 'dbus-register-signal))
  (progn
    ; include source specials into PDF to allow inverse search, using synctex
    (setq TeX-source-correlate-method 'synctex)
    (condition-case nil 
        (dbus-register-signal
         :session nil "/org/gnome/evince/Window/0"
         "org.gnome.evince.Window" "SyncSource"
         'evince-sync)
      (dbus-error (message "dbus error: inverse PDF search not available")))))


;; reftex
;(add-hook 'latex-mode-hook 'turn-on-reftex)
;(setq reftex-plug-into-AUCTeX t)
;(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
;(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)

;; BiBTeX variables
(setq bibtex-entry-format '(opts-or-alts realign last-comma))

;;; Latex word count
(setq latex-word-count-options "-brief") ;;short output
;;(setq latex-word-count-options "") ;;long output
;;(setq latex-word-count-options "-v -nc") ;;VERY verbose
;;Add -inc if you want latex-word-count to include .tex files included in the document with the \include command

;;Perl script for latex-word-count
(setq latex-word-count-executable "/vol/ni/bin/tex-wordcount.pl")

(defun latex-word-count ()  
"Count words in a latex buffer"  
(interactive)  
(let ((start (if mark-active (region-beginning) (point-min)))        
(end (if mark-active (region-end) (point-max))))    
  (shell-command-on-region     
        start end     
                (concat "cat > /tmp/latex-word-count.tex; "             
                               latex-word-count-executable " "             
                               latex-word-count-options             
                               " /tmp/latex-word-count.tex"))))
