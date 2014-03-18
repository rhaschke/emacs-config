;; set some styles based on the mode
(setq c-default-style
		'((java-mode . "java")
		  (awk-mode . "awk")
		  (other . "linux")))

;; these are my default style settings (style user)
(setq c-basic-offset 3)
(dolist (name '(case-label 
					 innamespace 
					 inline-open)) 
  (c-set-offset name 0))

(defvar rsc-style
  '((c-basic-offset             . 4)
    (c-comment-only-line-offset . 0)
    (c-offsets-alist
     (statement-block-intro . +)
     (substatement-open     . 0)
     (substatement-label    . 0)
     (label                 . 0)
     (statement-cont        . +)

     (namespace-close       . 0)
     (namespace-open        . 0)
     (innamespace           . 0))))

(c-add-style "rsc" rsc-style)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
