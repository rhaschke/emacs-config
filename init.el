;;; loader.el --- Emacs initialization code
;;
;; Copyright (C) 2008, 2009, 2010, 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: Enacs, init
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
;;
;; This file is based on TODO's Holy grail loader for Emacs.

(eval-when-compile
  (require 'cl))

(defvar config-dir user-emacs-directory
  "The root of the user's Emacs settings directory tree.")

(defvar config-packages-dir
  "packages/"
  "The directory containing startup files for third party Emacs
  packages.")

(defvar config-common-dir
  "common/"
  "The directory containing startup files common to all Emacs
  versions and sessions.")

(defvar config-noninteractive-dir
  "noninteractive/"
  "The directory containing startup files for noninteractive
  Emacs sessions.")

(defvar config-interactive-dir
  "interactive/"
  "The directory containing startup files for interactive Emacs
  sessions.")

(defvar config-site-dir
  "/vol/ni/share/lib/emacs/"
  "Directory containg site-lisp files")

(defun robust-load-elisp (file)
  "load file, catching errors"
  (condition-case condition
    (load file)
    (error (let ((error-message (format "Errors loading %s: %s" file condition))
		 (comment-start (or comment-start "; ")))
             (message error-message)
             (with-current-buffer "*scratch*"
               (goto-char (point-max))
               (insert (concat comment-start " " error-message "\n")))
             nil))))

(defun load-user-elisp (file)
  "load file, given as basename"
  (let ((absolute-file 
	 (if (file-name-absolute-p file) file (concat config-dir file))))
    (when (or (file-readable-p (concat absolute-file ".el"))
	      (file-readable-p (concat absolute-file ".elc")))
      (robust-load-elisp absolute-file))))

(defun load-user-elisp-directory (directory &optional basedir)
  "load files from basedir/directory or config-dir/directory"
  (let ((absolute-directory (concat (or basedir config-dir) directory)))
    (when (file-accessible-directory-p absolute-directory)
      (progn
	(message "Loading files in directory %s..." absolute-directory)
	(dolist (file (directory-files absolute-directory t "^[^.].*\\.el$"))
	  (robust-load-elisp (substring file 0 -3)))
	(message "Loading files in directory %s...done" absolute-directory)
	t))))

;; add package dirs to emacs load-path
(dolist (dir (list config-site-dir
					(concat config-site-dir "ecb")
					(concat config-site-dir "org-mode/lisp")
					(concat user-emacs-directory "lib")))
  (when (file-exists-p dir) (push dir load-path)))
				  
;; load custom settings
(setq custom-file (concat config-dir "custom.el"))


;;; First we load version-specific stuff, packages and some other stuff.
;; + emacs${version} version specific stuff
;; + packages        add additional package directories to load path and
;;                   load autoloads
;; + common          load stuff common to all emacs versions
;;

(let ((version-directory (format "emacs%d" emacs-major-version)))
  (load-user-elisp-directory version-directory))

(load-user-elisp-directory config-packages-dir)

(load-user-elisp-directory config-common-dir)


;;; Now packages and everything should be in place. So we can load
;; customizations.
;;

(robust-load-elisp custom-file)



;;; We load (non)interactive startup files after customizations are in
;; place.
;;

(if noninteractive
  (load-user-elisp-directory config-noninteractive-dir)
  (load-user-elisp-directory config-interactive-dir))
