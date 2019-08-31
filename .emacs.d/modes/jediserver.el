(defun project-directory (buffer-name)
  "Return the root directory of the project that contain the
given BUFFER-NAME. Any directory with a .git or .jedi file/directory
is considered to be a project root."
  (interactive)
  (let ((root-dir (file-name-directory buffer-name)))
    (while (and root-dir
                (not (file-exists-p (concat root-dir ".git")))
                (not (file-exists-p (concat root-dir ".jedi"))))
      (setq root-dir
            (if (equal root-dir "/")
                nil
              (file-name-directory (directory-file-name root-dir)))))
    root-dir))

(defun project-name (buffer-name)
  "Return the name of the project that contain the given BUFFER-NAME."
  (let ((root-dir (project-directory buffer-name)))
    (if root-dir
        (file-name-nondirectory
         (directory-file-name root-dir))
      nil)))


(defun jedi-setup-venv ()
  "Activates the virtualenv of the current buffer."
  (interactive)
  (let ((project-name (project-name buffer-file-name)))
    (when project-name (venv-workon project-name))))



(setq python-environment-virtualenv
      '("virtualenv" "--no-site-packages" "--python" "python3"))


(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;; (setq flymake-python-pyflakes-executable "flake8")
