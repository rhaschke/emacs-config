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
;; (require 'cl-lib)
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
 (semantic-xml--doattribs (name value start end)
   (when (string= name "href")
     (cl-return value))))

(define-mode-local-override
  semantic-parse-region nxml-mode
  (start end &optional nonterminal depth returnonerror)
  "Bla"
  (save-excursion
    (goto-char start)
    (xmltok-forward-prolog)
    (let (stack
	  (children '(nil))
	  result)
     (while (and (xmltok-forward) (< (point) end))
       (case xmltok-type
	 (start-tag
	  (let ((name       (xmltok-start-tag-local-name))
		(start      xmltok-start)
		(attributes))
	    (cond
	     ;; XI include tag
	     ((and (string= name "include")
		   (semantic-xml--find-href))
	      (let* ((href (semantic-xml--find-href))
		     (tag  (semantic-tag-new-include href nil)))
		(push tag (first children))
		(push tag stack)))
	     ;; "Normal" tag
	     (t
	      (semantic-xml--doattribs (name value start end)
	        (let ((tag (semantic-tag-new-variable
			    name "attribute" value)))
		  (semantic-tag-set-bounds tag start end)
		  (push tag attributes)))

	      (let ((tag (semantic-tag-new-type
			  name "element" (reverse attributes) (cons nil nil))))
		(semantic-tag-set-bounds tag start (1+ start))
		(push tag (first children))
		(push '() children)

		(push tag stack))))))

	 (end-tag
	  (let* ((tag  (pop stack))
		 (start (semantic-tag-start tag))
		 (children1 (pop children)))
	    (cond
	     ;; XI include tag
	     ((semantic-tag-of-class-p tag 'include)
	      (semantic-tag-set-bounds tag start (point))
	      (push tag result))
	     ;; Normal tag
	     (t
	      (setq tag (semantic-tag-new-type
			 (semantic-tag-name tag)
			 "element"
			 (append (semantic-tag-type-members tag) children1)
			 (cons nil nil)))
	      (semantic-tag-set-bounds tag start (point))
	      (if (= 1 (length children))
		  (push tag result)
		(progn
		  (pop (first children))
		  (push tag (first children))))))))

	 (empty-element
	  (let* ((name       (xmltok-start-tag-local-name))
		 (start      xmltok-start)
		 (attributes))
	    (cond
	     ;; XI include tag
	     ((and (string= name "include")
		   (semantic-xml--find-href))
	      (let* ((href (semantic-xml--find-href))
		     (tag  (semantic-tag-new-include href nil)))
		(semantic-tag-set-bounds tag start (point))
		(push tag result)))
	     ;; Normal tag
	     (t
	      (semantic-xml--doattribs (name value start end)
	        (let ((tag (semantic-tag-new-variable
			    name "attribute" value)))
		  (semantic-tag-set-bounds tag start end)
		  (push tag attributes)))

	      (let ((tag (semantic-tag-new-type
			  name "element" (reverse attributes)
			  (cons nil nil))))
		(semantic-tag-set-bounds tag start (point))
		(push tag (first children)))))))))

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
		   (variable "@")
		   (type     "<")
		   (include  "<include ")
		   (t        "")))
	 (suffix (case class
		   (variable "")
		   (type     ">")
		   (include  ">")
		   (t        ""))))
    (concat prefix
	    name
	    (when (eq class 'type)
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
			(semantic-tag-type-members tag))))
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
  (make-variable-buffer-local 'semantic-idle-breadcrumbs-format-tag-list-function)
  (make-variable-buffer-local 'semantic-idle-breadcrumbs-separator)

  (setq
   semantic-parser-name "nxml"

   semantic--parse-table t

   ;; Character used to separation a parent/child relationship
   semantic-type-relation-separator-character '(".")

   ;;
   semantic-symbol->name-assoc-list-for-type-parts
   '((variable . "Attributes")
     (function . "Methods"))

   semantic-symbol->name-assoc-list '((type     . "Elements")
				      (variable . "Attributes")
				      (function . "Functions")
				      (include  . "Imports")
				      (package  . "Namespace")
				      (code     . "Code"))

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
