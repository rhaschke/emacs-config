;;; xml.el ---
;;
;; Copyright (C) 2014 Jan Moringen, Robert Haschke
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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


;;; History:
;;


;;; Code:
;;

(require 'cl)
(defalias 'cl-gensym 'gensym)
(defalias 'cl-dolist 'dolist)
(defalias 'cl-return 'return)
(defalias 'cl-case 'case)

(require 'semantic/tag)
(require 'semantic/format)
(require 'nxml-mode)


;;; Lexical features and setup
;;

(defmacro semantic-nxml--doattribs (name-value &rest body)
  "Bind NAME-VALUE which has to be of the form

  (NAME-VAR VALUE-VAR &optional START-VAR END-VAR)

to successive attribute names and values of the current tag (via
`xmltok-attributes') and execute BODY for each binding."
  (let ((name   (first name-value))
		  (value  (second name-value))
		  (start  (or (third name-value) (cl-gensym)))
		  (end    (or (fourth name-value) (cl-gensym)))
		  (attrib (cl-gensym)))
    `(dolist (,attrib xmltok-attributes)
       (let ((,name  (xmltok-attribute-local-name ,attrib))
				 (,value (xmltok-attribute-value ,attrib))
				 (,start (xmltok-attribute-name-start ,attrib))
				 (,end   (xmltok-attribute-value-end ,attrib)))
			,@body))))

(defun semantic-nxml--find-href ()
  (semantic-nxml--doattribs (name value)
									 (when (string= name "href")
										(cl-return value))))

(defun semantic-nxml-parse-region (start end &rest ignore)
  "Parse the current nxml buffer for semantic tags.
Each tag returned is of the form:
 (\"NAME\" include|element (:attributes ATTRIBUTES :children CHILDREN))

It is an override of 'parse-region and must be installed by the
function `semantic-install-function-overrides'."
  (save-excursion
    (goto-char start)
    (xmltok-forward-prolog)
    (let (stack 
			 (children '(nil)) 
			 result)
		(while (and (xmltok-forward) (< (point) end))
		  ;; start-tag or empty element: push onto stack
		  (when (or (eq xmltok-type 'start-tag)
						(eq xmltok-type 'empty-element))
			 (let ((name   (xmltok-start-tag-local-name))
					 (start  xmltok-start)
					 (attributes))
				;; parse the attributes
				(semantic-nxml--doattribs (name value start end)
												  (let ((tag (semantic-tag name 'attribute :value value)))
													 (semantic-tag-set-bounds tag start end)
													 (push tag attributes)))

				;; create new element tag with given attributes
				(let ((tag (semantic-tag name 'element :attributes (reverse attributes))))
				  (when (and (string= (semantic-tag-name tag) "include")
								 (semantic-nxml--find-href))
					 (let ((href (semantic-nxml--find-href)))
						;; replace tag by include-type tag
						(setq tag (semantic-tag-new-include href nil))))

				  (semantic-tag-set-bounds tag start (1+ start))
				  ;; push this tag into current list of children (first)
				  (push tag (first children))
				  ;; push an empty list to front in order to accumulate children of this element
				  (push '() children)
				  ;; push tag onto stack
				  (push tag stack))))

		  ;; end-tag or empty element: pop from stack
		  (when (or (eq xmltok-type 'end-tag)
						(eq xmltok-type 'empty-element))
			 (let* ((tag   (pop stack))
					  (start (semantic-tag-start tag))
					  (mychildren (pop children)))
				
				(when mychildren
				  (semantic-tag-put-attribute tag :children (nreverse mychildren)))
				(semantic-tag-set-bounds tag start (point))

				(when (semantic-tag-of-class-p tag 'include)
				  ;; push includes always as elements into result
				  (push tag result))

				(when (not stack)
				  (push tag result)))))
		(nreverse result))))

(defun semantic-nxml-parse-changes ()
  "Parse changes in the current nxml buffer."
  ;; NOTE: For now, just schedule a full reparse.
  ;;       To be implemented later.
  (semantic-parse-tree-set-needs-rebuild))

(defun semantic-nxml-tag-components (tag)
  "Return list of components of TAG"
  (let ((children (semantic-tag-get-attribute tag :children))
		  (attributes (semantic-tag-get-attribute tag :attributes)))
	 (append attributes children)))

(defun semantic-nxml-tag-components-with-overlays (tag)
  "Return list of components of TAG"
  (semantic-nxml-tag-components tag))


;;;
;;

(defun semantic-nxml-format-tag-abbreviate
  (tag &optional parent color)
  "Return an abbreviated string describing TAG."
  (let* ((class  (semantic-tag-class tag))
			(name   (semantic-format-tag-name tag parent color))
			(prefix (case class
						 (include  "include ")
						 (t        "")))
			(suffix (case class
						 (attribute (concat "=" (semantic-tag-get-attribute tag :value)))
						 (t         ""))))
    (concat prefix name suffix)))

(defun semantic-nxml-format-tag-summarize
  (tag &optional parent color)
  "Return a string describing TAG."
  (let ((result (semantic-nxml-format-tag-abbreviate tag parent color))
		  (attribs (when (semantic-tag-of-class-p tag 'element)
						 (mapconcat (lambda (c) (semantic-nxml-format-tag-abbreviate c tag color))
										(semantic-tag-get-attribute tag :attributes)
										" "))))
	 (if attribs (concat result ": " attribs) result)))


;;; Enable Semantic in `nxml-mode'.
;;

(semantic-install-function-overrides
 '(
	(parse-region . semantic-nxml-parse-region)
	(parse-changes . semantic-nxml-parse-changes)
	(tag-components . semantic-nxml-tag-components)
	(tag-components-with-overlays . semantic-nxml-tag-components-with-overlays)
	(format-tag-abbreviate . semantic-nxml-format-tag-abbreviate)
	(format-tag-summarize . semantic-nxml-format-tag-summarize)
	)
 t 'nxml-mode)

(eval-after-load "ecb"
  '(progn
	  ;; define how the new tag classes should be displayed in ecb-methods-buffer
	  (let ((defaults (car (cdar (get 'ecb-show-tags 'standard-value))))
			  (nxml-settings '(nxml-mode (element flattened nil)
												  (attribute hidden nil)
												  (include collapsed nil))))
		 (nconc defaults (list nxml-settings)))
	  
	  (let ((defaults (car (cdar (get 'ecb-tag-display-function 'standard-value))))
			  (nxml-settings '(nxml-mode . ecb-format-tag-summarize)))
		 (nconc defaults (list nxml-settings)))
	  ))

;; define faces for elements and attributes
(nconc semantic-format-face-alist '((element . font-lock-function-name-face)
												(attribute . font-lock-variable-name-face)))

;;;###autoload
(defun semantic-default-nxml-setup ()
  "Setup hook function for nxml and semantic."
  (make-variable-buffer-local 'semantic-idle-breadcrumbs-format-tag-list-function)
  (make-variable-buffer-local 'semantic-idle-breadcrumbs-separator)

  (setq
   semantic-parser-name  "nxml"
	;; setup a dummy parser table to enable parsing
   semantic--parse-table t

   semantic-symbol->name-assoc-list '((element   . "elements")
												  (attribute . "attributes")
												  (include   . "includes"))

   semantic-idle-breadcrumbs-format-tag-list-function 
	#'semantic-idle-breadcrumbs--format-linear
   semantic-idle-breadcrumbs-separator " "))

;;;###autoload
(add-hook 'nxml-mode-hook 'semantic-default-nxml-setup)

(require 'semantic/idle)       ; idle scheduler

;;; redefine semantic-idle-breadcrumbs to fix a bug
(define-semantic-idle-service semantic-idle-breadcrumbs
  "Display breadcrumbs for the tag under point and its parents."
  (let* ((scope    (semantic-calculate-scope))
	 (tag-list (if (and scope (oref scope parents))
		       ;; If there is a scope, extract the tag and its
		       ;; parents.
		       (append (oref scope parents)
			       (when (oref scope tag)
				 (list (oref scope tag))))
		     ;; Fall back to tags by overlay
		     (semantic-find-tag-by-overlay))))
    ;; Display the tags.
    (funcall semantic-idle-breadcrumbs-display-function tag-list)))


(provide 'semantic/bovine/xml)

;;; semantic/bovine/xml.el ends here
