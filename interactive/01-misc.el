(defun sm-try-smerge ()
  "Enable `smerge-mode' if the current buffer contains conflict markers."
  (unless (derived-mode-p '(special-mode))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
	(smerge-mode 1)))))

(defun other-window-backwards ()
  "Like `other-window' but visit windows in reverse order."
  (interactive)
  (other-window -1))

(defun fullscreen-p (&optional frame)
  ""
  (frame-parameter frame 'fullscreen))

(defun toggle-fullscreen (&optional frame)
  "Toggle fullscreen state of FRAME.
If FRAME is nil, apply to current frame."
  (interactive)
  (set-frame-parameter frame 'fullscreen
		       (if (fullscreen-p frame) nil 'fullboth)))
