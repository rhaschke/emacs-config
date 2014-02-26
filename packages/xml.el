;;; xml.el ---
;;
;; Copyright (C) 2012, 2014 Jan Moringen
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

(defmacro semantic-xml--doattribs (name-value &rest body)
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

(defun semantic-xml--find-href ()
  (semantic-xml--doattribs (name value)
									(when (string= name "href")
									  (cl-return value))))

(defun semantic-nxml-parse-region 
  (start end &optional nonterminal depth returnonerror)
  "Bla"
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
				(message "-> element: %s, stack: %d" name (length stack))
				;; parse the attributes
				(semantic-xml--doattribs (name value start end)
												 (let ((tag (semantic-tag name 'attribute :value value)))
													(semantic-tag-set-bounds tag start end)
													(push tag attributes)))

				;; create new element tag with given attributes
				(let ((tag (semantic-tag name 'element :attributes (reverse attributes))))
				  (when (and (string= (semantic-tag-name tag) "include")
								 (semantic-xml--find-href))
					 (let ((href (semantic-xml--find-href)))
						;; replace tag by include-type tag
						(setq tag (semantic-tag-new-include href nil)))
					 ;; push includes always as elements into results
					 (push tag result))

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
				(message "<- element: %s, stack: %d" (semantic-tag-name tag) (length stack))
				
				(when mychildren
				  (semantic-tag-put-attribute tag :children (nreverse mychildren)))
				(semantic-tag-set-bounds tag start (point))

				(when (not stack)
					 (push tag result)))))
		(nreverse result))))


;;;
;;

(define-mode-local-override semantic-format-tag-abbreviate
  nxml-mode (tag &optional parent color)
  "Return an abbreviated string describing TAG."
  ;; Do lots of complex stuff here.
  (let* ((class  (semantic-tag-class tag))
			(name   (semantic-format-tag-canonical-name
						tag parent color))
			(prefix (case class
						 (attribute "@")
						 (element   "<")
						 (include  "<include ")
						 (t        "")))
			(suffix (case class
						 (attribute "")
						 (element   ">")
						 (include   ">")
						 (t         ""))))
    (concat prefix
				name
				(when (eq class 'element)
				  (let (members)
					 (mapc (lambda (child)
								(push
								 (concat
								  (semantic-format-tag-name child nil color)
								  "="
								  (propertize
									(concat
									 "\""
									 (semantic-tag-variable-default child)
									 "\"")
									'face 'font-lock-string-face))
								 members))
							 (semantic-find-tags-by-name-regexp
							  "\\(:?name\\|id\\)$"
							  (semantic-find-tags-by-type
								"attribute"
								(semantic-tag-get-attribute tag :attributes))))
					 (when members
						(apply #'concat " " (reverse members)))))
				suffix)))


;;;
;;

(define-mode-local-override semantic-analyze-unsplit-name
  nxml-mode (namelist)
  "Bla"
  (concat "/" (mapconcat 'identity namelist "/")))


;;; Enable Semantic in `nxml-mode'.
;;

;;;###autoload
(defun semantic-default-xml-setup ()
  "Setup hook function for Lisp files and Semantic."
  (semantic-install-function-overrides
   '((parse-region . semantic-nxml-parse-region)))
  (make-variable-buffer-local 'semantic-idle-breadcrumbs-format-tag-list-function)
  (make-variable-buffer-local 'semantic-idle-breadcrumbs-separator)

  (setq
   semantic-parser-name "nxml"

   semantic--parse-table t

   ;; Character used to separation a parent/child relationship
   semantic-type-relation-separator-character '(".")

   ;;
   semantic-symbol->name-assoc-list-for-type-parts
   '((element   . "elements")
     (attribute . "attributes"))

   semantic-symbol->name-assoc-list '((element   . "elements")
												  (attribute . "attributes")
												  (include   . "includes"))

   ;; Tag formatting.
   semantic-format-parent-separator "/"

   semantic-idle-breadcrumbs-format-tag-list-function
   #'semantic-idle-breadcrumbs--format-linear
   semantic-idle-breadcrumbs-separator
   " "))

;;;###autoload
(push (cons 'nxml-mode 'semantic-default-xml-setup)
      semantic-new-buffer-setup-functions)

(provide 'semantic/bovine/xml)

;;; semantic/bovine/xml.el ends here
