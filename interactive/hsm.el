(require 'nxml-script)

(define-derived-mode hsm-mode nxml-mode "hsm"
  "HSM major mode"
)

(add-to-list 'auto-mode-alist '("\\.hsm$" . hsm-mode))

;; hsm error display
(defun display-hsm-errors (message)
  (with-current-buffer (get-buffer-create "*hsm errors*")
    (erase-buffer)
    (insert message)
    (pop-to-buffer (current-buffer))))

;; jump to buffer for python errors
(defun hsm-pythonError (errors file row col &rest ignored)
  "callback method for dbus messages from hsm (for python errors)"
  (message "HSM python error in %s:%d" file row)
;  (display-hsm-errors errors)
  (when (and file (file-exists-p file))
	 (let ((buf (find-buffer-visiting file)))
		(if buf 
		  (let ((w (get-buffer-window buf)))
			 (if w 
				(select-window w) ; make window w active
				(switch-to-buffer buf))) ; show buf in currently active window
		  (find-file file))
		(when (> row 0) (goto-line row))
		(when (> col 0) (move-to-column col))
;		(raise-frame (window-frame (selected-window)))
)))

(require 'dbus)
(when (and (eq window-system 'x) (fboundp 'dbus-register-signal))
  (condition-case nil 
		(dbus-register-signal
		 :session nil "/de/unibi/agni/hsm"
		 "de.unibi.agni.hsm" "pythonError"
		 'hsm-pythonError)
	 (dbus-error (message "registering dbus hsm messages failed"))))

(defun rhaschke/hsm-mode-hook ()
  (set (make-local-variable 'compile-command)
		 (concat "PYTHONPATH=/vol/nirobots/demo/vdemo-cfg/hsm/python "
					"dynamicHSM -m xcf:ShortTerm "
					buffer-file-name))
)
(add-hook 'hsm-mode-hook 'rhaschke/hsm-mode-hook)


(defun semantic-hsm-parse-region (start end &rest ignore)
  "Parse the current hsm buffer for semantic tags."
  (let ((tags (semantic-nxml-parse-region start end)))
	 (destructuring-bind (states events) (semantic-hsm-transform-tags tags)
		(nconc (nreverse states) (nreverse events)))))
		
(defun semantic-hsm-tag-components (tag)
  "Return list of components of TAG"
  (semantic-tag-get-attribute tag :states))

(defun semantic-hsm-tag-components-with-overlays (tag)
  "Return list of components of TAG"
  (semantic-hsm-tag-components tag))

(defvar state-regexp ".*STATE\\|REGION")

(defmacro let-attrs (tag bindings &rest body)
  "(let-attrs tag-form ((name \"name\" \"default\")) name)"
  (declare (indent 2))
  (let ((atts-var (gensym)))
	 (flet ((do-binding (binding)
			    (destructuring-bind (name attribute-name &optional default) binding
					(let ((attr-tag (gensym)))
					  `(,name (let ((,attr-tag (semantic-find-first-tag-by-name 
														 ,attribute-name ,atts-var)))
									(if ,attr-tag
										 (semantic-tag-get-attribute ,attr-tag :value)
									  ,default)))))))
		`(let* ((,atts-var (semantic-tag-get-attribute ,tag :attributes))			 
				  ,@(mapcar 'do-binding bindings))
			,@body))))

(defun xpath-from-sender-and-name (name &optional sender)
  (when name
	 (unless sender 
		(assert (string-match "\\(?:\\([^:]+\\):\\)?\\(.*\\)" name))
		(setq sender (match-string 1 name)
				name (match-string 2 name)))
	 (when (string= sender "*") (setq sender nil))
	 (when sender 
		(setq sender (concat " and ("
									(mapconcat (lambda (s) (format "@sender='%s'" s))
												  (split-string sender ", *")
												  " or ")
									")")))
	 (format "/EVENT[@name='%s'%s]" name (or sender ""))))

;(xpath-from-sender-and-name "s1,s2, s3:foo") 
;(xpath-from-sender-and-name "s1:foo")
;(xpath-from-sender-and-name "*:foo")
(assert (null (xpath-from-sender-and-name nil)))
(assert (string= (xpath-from-sender-and-name "foo") "/EVENT[@name='foo']"))
							
(defun semantic-hsm-transform-tags (tags)
  "Remap nxml tags into (states events) list
Both, states and events are in reverse order"
  (let (states events)
	 (dolist (tag tags)
		(let ((class (semantic-tag-class tag))
				(name  (semantic-tag-name tag))
				(atts  (semantic-tag-get-attribute tag :attributes))
				(children (semantic-tag-get-attribute tag :children)))
		  (when (eq class 'element)
			 (cond 
			  ;; handle states
			  ((string-match-p state-regexp name)
				(let-attrs tag ((name "name"))
				  (let ((stateTag (semantic-tag name 'state)))
					 (apply 'semantic-tag-set-bounds stateTag (semantic-tag-bounds tag))
					 (destructuring-bind (s e) (semantic-hsm-transform-tags children)
						(semantic-tag-put-attribute stateTag :states (nreverse s))
						(setq events (append e events)))
					 (push stateTag states))))
			  ;; events
			  ((string= "EVENT" name)
				(let-attrs tag ((name    "name")
									 (sender  "sender")
									 (alias   "alias" name)
									 (actions "actions" "INSERT")
									 (xpath   "xpath" (xpath-from-sender-and-name name sender))
									 (internal "internal")
									 (rate    "rate"))
				  (let ((eventTag (semantic-tag alias 'event :actions actions :xpath xpath)))
					 (apply 'semantic-tag-set-bounds eventTag (semantic-tag-bounds tag))
					 (push eventTag events))))
			  ;; other elements
			  (t
				(destructuring-bind (s e) (semantic-hsm-transform-tags children)
				  (setq events (append e events)
						  states s)))))))
	 (list states events)))
					 
(defun semantic-hsm-format-tag-abbreviate
  (tag &optional parent color)
  "Return an abbreviated string describing TAG."
  (let* ((class  (semantic-tag-class tag))
			(name   (semantic-format-tag-name tag parent color))
			(prefix (case class
						 (include  "include ")
						 (t        "")))
			(suffix (case class
						 (t         ""))))
    (concat prefix name suffix)))

(semantic-install-function-overrides
 '(
	(parse-region . semantic-hsm-parse-region)
	(tag-components . semantic-hsm-tag-components)
	(tag-components-with-overlays . semantic-hsm-tag-components-with-overlays)
	(format-tag-abbreviate . semantic-hsm-format-tag-abbreviate)
	)
 t 'hsm-mode)

(require 'ecb)
;; define how the new tag classes should be displayed in ecb-methods-buffer
(let ((defaults (car (cdar (get 'ecb-show-tags 'standard-value))))
		(hsm-settings '(hsm-mode (state flattened nil)
										 (event collapsed nil)
										 (include collapsed nil))))
  (nconc defaults (list hsm-settings)))

(let ((defaults (car (cdar (get 'ecb-tag-display-function 'standard-value))))
		(hsm-settings '(hsm-mode . ecb-format-tag-abbreviate)))
  (nconc defaults (list hsm-settings)))

;; define faces for states and events
(nconc semantic-format-face-alist '((state . font-lock-function-name-face)
												(event . font-lock-variable-name-face)))

;;;###autoload
(defun semantic-default-hsm-setup ()
  "Setup hook function for hsm and semantic."
  (setq
	semantic-parser-name  "hsm"
   semantic-symbol->name-assoc-list '((state . "states")
												  (event . "events"))
	semantic-idle-breadcrumbs-separator ":"))

;;;###autoload
(add-hook 'hsm-mode-hook 'semantic-default-hsm-setup)
