;;; hsm.el ---
;;
;; Copyright (C) 2014 Robert Haschke, Jan Moringen
;;
;; Author: Robert Haschke <rhaschke@techfak.uni-bielefeld.de>
;; Keywords: parsing
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;


;;; Code:
;;

(require 'nxml-script)

(define-derived-mode hsm-mode nxml-mode "hsm"
  "HSM major mode"
)

(add-to-list 'auto-mode-alist '("\\.hsm$" . hsm-mode))
(push '("\\(<\\?xml.*?\\?>\\s-*\\)?<HSM" . hsm-mode) magic-mode-alist)

(when (featurep 'auto-complete-config)
  (define-child-mode hsm-mode nxml-mode)
  (add-to-list 'ac-modes 'hsm-mode))

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
	 (destructuring-bind (nodes events) (semantic-hsm-transform-tags tags)
		(nconc (nreverse nodes) (nreverse events)))))
		
(defun semantic-hsm-tag-components (tag)
  "Return list of components of TAG"
  (semantic-tag-get-attribute tag :children))

(defun semantic-hsm-tag-components-with-overlays (tag)
  "Return list of components of TAG"
  (semantic-hsm-tag-components tag))

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

(defun semantic-hsm-get-xpath (name &optional sender)
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

;(semantic-hsm-get-xpath "s1,s2, s3:foo") 
;(semantic-hsm-get-xpath "s1:foo")
;(semantic-hsm-get-xpath "*:foo")
;(assert (null (semantic-hsm-get-xpath nil)))
;(assert (string= (semantic-hsm-get-xpath "foo") "/EVENT[@name='foo']"))
(defun semantic-hsm-parse-boolean (string)
  "parse string into a boolean value: t / nil"
  (if (and string (string-match-p "^ *\\(true\\|1\\) *$" string)) t nil))

(defvar state-regexp ".*STATE\\|REGION")
(defvar recursive-regexp "HSM\\|VAR\\|EVENTS")

(defun semantic-hsm-transform-tags (tags)
  "Remap nxml tags into (nodes events) list
Both, nodes and events are in reverse order"
  (let (nodes events)
	 (dolist (tag tags)
		(let ((class (semantic-tag-class tag))
				(name  (semantic-tag-name tag))
				(atts  (semantic-tag-get-attribute tag :attributes))
				(children (semantic-tag-get-attribute tag :children)))
		  (case class 
			 ;; element nodes
			 (element
			  (cond 
				;; handle states
				((string-match-p state-regexp name)
				 (let-attrs tag ((name "name"))
					(let ((stateTag (semantic-tag name 'state)))
					  (apply 'semantic-tag-set-bounds stateTag (semantic-tag-bounds tag))
					  (destructuring-bind (n e) (semantic-hsm-transform-tags children)
						 (semantic-tag-put-attribute stateTag :children (nreverse n))
						 (setq events (append e events)))
					  (push stateTag nodes))))
				;; events
				((string= "EVENT" name)
				 (let-attrs tag ((name    "name")
									  (sender  "sender")
									  (alias   "alias" name)
									  (actions "actions" "INSERT")
									  (xpath   "xpath" (semantic-hsm-get-xpath name sender))
									  (internal "internal")
									  (rate    "rate"))
					(let* ((internal (semantic-hsm-parse-boolean internal))
							 (xpath   (if internal nil xpath))
							 (actions (if internal nil actions))
							 (eventTag (semantic-tag alias 'event 
															:actions actions 
															:xpath xpath
															:internal internal
															:rate rate)))
					  (apply 'semantic-tag-set-bounds eventTag (semantic-tag-bounds tag))
					  (push eventTag events))))
				;; other elements
				((string-match-p recursive-regexp name)
				 (destructuring-bind (n e) (semantic-hsm-transform-tags children)
					(setq events (append e events)
							nodes (append n nodes))))))

			 ;; include nodes
			 (include (push tag nodes))
			 )))
	 (list nodes events)))

(defun semantic-hsm-format-event (tag &optional parent color)
  "format event tag"
  (let* ((xpath (semantic--format-colorize-text
					  (semantic-tag-get-attribute tag :xpath) 'function))
			(actions (semantic--format-colorize-text
						 (semantic-tag-get-attribute tag :actions) 'comment))
			(rate (semantic-tag-get-attribute tag :rate))
			(rate (when rate (semantic--format-colorize-text
									(concat "rate=" rate) 'keyword)))
			(internal (semantic-tag-get-attribute tag :internal))
			(name (semantic-format-tag-name tag parent color)))
	 (when (and color (featurep 'font-lock) internal)
		(setq name (semantic--format-colorize-merge-text name 'static)))
	 (mapconcat 'identity 
					(remove-if (lambda (s) (string= s ""))
								  (remove-if 'null (list name xpath actions rate)))
					" ")))

(defun semantic-hsm-format-tag-abbreviate
  (tag &optional parent color)
  "Return an abbreviated string describing TAG."
  (let ((class  (semantic-tag-class tag)))
	 (case class
		(event (semantic-format-tag-name tag parent color))
		(state (semantic-format-tag-name tag parent color))
		(t (semantic-nxml-format-tag-abbreviate tag parent color)))))

(defun semantic-hsm-format-tag-summarize
  (tag &optional parent color)
  "Return an abbreviated string describing TAG."
  (let ((class  (semantic-tag-class tag)))
	 (case class
		(event (semantic-hsm-format-event tag parent color))
		(t (semantic-hsm-format-tag-abbreviate tag parent color)))))

;(semantic-hsm-format-tag-summarize
; (semantic-tag "myevent" 'event :internal t :rate "1.2") nil t)

(semantic-install-function-overrides
 '(
	(parse-region . semantic-hsm-parse-region)
	(tag-components . semantic-hsm-tag-components)
	(tag-components-with-overlays . semantic-hsm-tag-components-with-overlays)
	(format-tag-abbreviate . semantic-hsm-format-tag-abbreviate)
	(format-tag-summarize . semantic-hsm-format-tag-summarize)
	)
 t 'hsm-mode)

(eval-after-load "ecb"
  '(progn
	  ;; define how the new tag classes should be displayed in ecb-methods-buffer
	  (let ((defaults (car (cdar (get 'ecb-show-tags 'standard-value))))
			  (hsm-settings '(hsm-mode (state flattened nil)
												(event collapsed name)
												(t collapsed nil))))
		 (nconc defaults (list hsm-settings)))

	  (let ((defaults (car (cdar (get 'ecb-tag-display-function 'standard-value))))
			  (hsm-settings '(hsm-mode . ecb-format-tag-summarize)))
		 (nconc defaults (list hsm-settings)))
	  ))

;; define faces for states and events
(nconc semantic-format-face-alist '((state . font-lock-function-name-face)
												(event . font-lock-type-face)))

;;;###autoload
(defun semantic-default-hsm-setup ()
  "Setup hook function for hsm and semantic."
  (setq
	semantic-parser-name  "hsm"
   semantic-symbol->name-assoc-list '((state . "states")
												  (event . "events")
												  (include . "includes"))
	semantic-idle-breadcrumbs-separator ":"))

;;;###autoload
(add-hook 'hsm-mode-hook 'semantic-default-hsm-setup)
