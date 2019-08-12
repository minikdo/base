
;; Deleting a single word backwards
(defadvice kill-region (before unix-werase activate compile)
        "When called interactively with no active region, delete a single word
    backwards instead."
	(interactive
	 (if mark-active (list (region-beginning) (region-end))
	   (list (save-excursion (backward-word 1) (point)) (point)))))



(defun kill-to-end:b ()
  "Kills text from the cursor postion to the end of the buffer. 
  This command adds the killed text to the kill-ring"
    (interactive)
      (save-excursion
	(let ((beg (point)) (end (point-max)))
          (kill-region beg end))))


(defun ff/comment-and-go-down (arg)
  "Comments and goes down ARG lines."
  (interactive "p")
  (condition-case nil
      (comment-region (point-at-bol) (point-at-eol)) (error nil))
  (next-line 1)
  (if (> arg 1) (ff/comment-and-go-down (1- arg))))


(defun ff/uncomment-and-go-up (arg)
  "Uncomments and goes up ARG lines."
  (interactive "p")
  (condition-case nil
      (uncomment-region (point-at-bol) (point-at-eol)) (error nil))
  (next-line -1)
  (if (> arg 1) (ff/uncomment-and-go-down (1- arg))))


;; (defun select-next-window ()
  ;; "Switch to the next window"
  ;; (interactive)
  ;; (select-window (next-window)))

;; (defun select-previous-window ()
  ;; "Switch to the previous window"
  ;; (interactive)
  ;; (select-window (previous-window)))


;; Helper to find the best project root
(defun aw/guess-best-root-for-buffer (buf repo-sentry &optional init-sentry)
  "Guesses that the python root is the less 'deep' of either:
     -- the root directory of the repository, or
     -- the directory before the first directory after the root
        having an __init__.py file."
  
  ;; make list of directories from root, removing empty
  (defun make-dir-list (path)
    (delq nil (mapcar (lambda (x) (and (not (string= x "")) x))
                      (split-string path "/"))))
  ;; convert a list of directories to a path starting at "/"
  (defun dir-list-to-path (dirs)
    (mapconcat 'identity (cons "" dirs) "/"))
  ;; a little something to try to find the "best" root directory
  (defun try-find-best-root (base-dir buffer-dir current)
    (cond
     (base-dir ;; traverse until we reach the base
      (try-find-best-root (cdr base-dir) (cdr buffer-dir)
                          (append current (list (car buffer-dir)))))
     
     (buffer-dir ;; try until we hit the current directory
      (let* ((next-dir (append current (list (car buffer-dir))))
             (sentry-file (concat (dir-list-to-path next-dir) "/" init-sentry)))
        (if (file-exists-p sentry-file)
            (dir-list-to-path current)
          (try-find-best-root nil (cdr buffer-dir) next-dir))))
     
     (t nil)))
  
  (let* ((buffer-dir (expand-file-name (file-name-directory (buffer-file-name buf))))
         (vc-root-dir (vc-find-root buffer-dir repo-sentry)))
    (if (and init-sentry vc-root-dir)
        (try-find-best-root
         (make-dir-list (expand-file-name vc-root-dir))
         (make-dir-list buffer-dir)
         '())
      vc-root-dir))) ;; default to vc root if sentry not given



;; Dired

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
              "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
              (interactive "P")
              (let* ((fn-list (dired-get-marked-files nil arg)))
                         (mapc 'find-file fn-list)))))
