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
		;; summarize several states within HSM node
		(when (> (length states) 1)
		  (setq start (semantic-tag-start (first states))
				  end (semantic-tag-end (car (last states)))
				  states (semantic-tag "HSM" 'state :states states))
		  (semantic-tag-set-bounds states start end))
		(nconc states (nreverse events)))))
		
(defun semantic-hsm-tag-components (tag)
  "Return list of components of TAG"
  (let ((states (semantic-tag-get-attribute tag :children))
		  (events (semantic-tag-get-attribute tag :attributes)))
	 (append states events)))

(defun semantic-hsm-tag-components-with-overlays (tag)
  "Return list of components of TAG"
  (semantic-hsm-tag-components tag))

(defvar state-regexp ".*STATE\\|REGION")

(defun semantic-hsm-transform-tags (tags)
  "Remap nxml tags into (states events) list
states is in correct order, events is reverse"
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
				(let* ((att (semantic-find-first-tag-by-name "name" atts))
						 (name (semantic-tag-get-attribute att :value))
						 (stateTag (semantic-tag name 'state)))
				  (apply 'semantic-tag-set-bounds stateTag (semantic-tag-bounds tag))
				  (destructuring-bind (s e) (semantic-hsm-transform-tags children)
					 (semantic-tag-put-attribute stateTag :states s)
					 (setq events (append e events)))
				  (push stateTag states)))
			  ;; other elements
			  (t
				(destructuring-bind (s e) (semantic-hsm-transform-tags children)
				  (assert (not states))
				  (setq events (append e events)
						  states s)))))))
			 
	 (list (nreverse states) events)))
					 

(semantic-install-function-overrides
 '(
	(parse-region . semantic-hsm-parse-region)
	(tag-components . semantic-hsm-tag-components)
	(tag-components-with-overlays . semantic-hsm-tag-components-with-overlays)
	(format-tag-abbreviate . semantic-nxml-format-tag-abbreviate)
	(format-tag-summarize . semantic-nxml-format-tag-summarize)
	)
 t 'hsm-mode)

(require 'ecb)
;; define how the new tag classes should be displayed in ecb-methods-buffer
(let ((defaults (car (cdar (get 'ecb-show-tags 'standard-value))))
		(hsm-settings '(hsm-mode (element flattened nil)
										 (attribute hidden nil)
										 (include collapsed nil))))
  (nconc defaults (list hsm-settings)))

(let ((defaults (car (cdar (get 'ecb-tag-display-function 'standard-value))))
		(hsm-settings '(hsm-mode . ecb-format-tag-summarize)))
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
												  (event . "events"))))

;;;###autoload
(add-hook 'hsm-mode-hook 'semantic-default-hsm-setup)
