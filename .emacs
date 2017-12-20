;; Global
;; Never, ever use tabs
(setq-default indent-tabs-mode nil)

;; Org-mode settings
(setq org-directory "~/.agenda")

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-index-file (org-file-path "dominik.org"))
(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))

(setq org-agenda-files (list org-index-file))

(setq org-default-notes-file (concat org-directory "/dominik.org"))

;; Hitting C-c C-x C-s will mark a todo as done and move it to an appropriate
;; place in the archive.

(defun hrs/mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

;; Record the time that a todo was archived.

(setq org-log-done 'time)

(setq calendar-latitude 20.92)
(setq calendar-longitude 52.26)


(setq org-todo-keywords
   '((sequence "TODO" "WAITING" "|" "DONE" )))

(setq org-todo-keyword-faces
                 '(("WAITING" . "violet")))

;; Keybindings

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda) 
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)


(setq org-capture-templates
      (quote (("t" "todo" entry (file+headline (org-file-path "dominik.org") "Tasks")
	       "* TODO %?\n %U\n")
	      ("n" "note" entry (file+headline (org-file-path "dominik.org") "Notes")
	       "* %? :NOTE:\n %U\n")
	      ("j" "Journal" entry (file (org-file-path "journal.org"))
	       "* %U %?\n" )
	      )))

;; to clock capture entry
;; :clock-in t :clock-resume t

;;(add-hook 'org-mode-hook 'abbrev-mode)
;;(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-capture-mode-hook 'flyspell-mode)
(add-hook 'org-capture-mode-hook 'abbrev-mode)

;; disable backup
(setq backup-inhibited t)

;; unset page up and down
(global-unset-key (kbd "<prior>"))
(global-unset-key (kbd "<next>"))

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'flatland t)

;; Other settings
(setq-default fill-column 80)
(setq auto-fill-mode t)
(setq-default auto-fill-function 'do-auto-fill)

;; Line numbers
(global-linum-mode t)
(set-window-margins nil 1)
(column-number-mode t)
(setq linum-format "%4d ")

;; Deleting a single word backwards
(defadvice kill-region (before unix-werase activate compile)
        "When called interactively with no active region, delete a single word
    backwards instead."
	(interactive
	 (if mark-active (list (region-beginning) (region-end))
	   (list (save-excursion (backward-word 1) (point)) (point)))))

;; save cursor position in files
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; Change dictionary to polish
(setq ispell-dictionary "polish")


(defun kill-to-end:b ()
  "Kills text from the cursor postion to the end of the buffer. 
  This command adds the killed text to the kill-ring"
    (interactive)
      (save-excursion
	(let ((beg (point)) (end (point-max)))
          (kill-region beg end))))


(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook 'autopair-mode) ;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook (lambda () (auto-fill-mode -1))) ;; disable autofill
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.


(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

; Initialize package mode along with all the installed packages
(package-initialize)
;; Enable elpy mode
(elpy-enable)
; Fixing a key binding bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)



;; which-key
;;(require 'which-key)
(which-key-mode)




;; ;; Helper to find the best project root
;; (defun aw/guess-best-root-for-buffer (buf repo-sentry &optional init-sentry)
;;   "Guesses that the python root is the less 'deep' of either:
;;      -- the root directory of the repository, or
;;      -- the directory before the first directory after the root
;;         having an __init__.py file."
  
;;   ;; make list of directories from root, removing empty
;;   (defun make-dir-list (path)
;;     (delq nil (mapcar (lambda (x) (and (not (string= x "")) x))
;; 		      (split-string path "/"))))
;;   ;; convert a list of directories to a path starting at "/"
;;   (defun dir-list-to-path (dirs)
;;     (mapconcat 'identity (cons "" dirs) "/"))
;;   ;; a little something to try to find the "best" root directory
;;   (defun try-find-best-root (base-dir buffer-dir current)
;;     (cond
;;      (base-dir ;; traverse until we reach the base
;;       (try-find-best-root (cdr base-dir) (cdr buffer-dir)
;; 			                              (append current (list (car
;; 									     buffer-dir)))))
     
;;      (buffer-dir ;; try until we hit the current directory
;;       (let* ((next-dir (append current (list (car buffer-dir))))
;; 	     (sentry-file (concat (dir-list-to-path next-dir) "/"
;; 				  init-sentry)))
;; 	(if (file-exists-p sentry-file)
;; 	    (dir-list-to-path current)
;; 	  (try-find-best-root nil (cdr buffer-dir) next-dir))))
     
;;      (t nil)))
  
;;   (let* ((buffer-dir (expand-file-name (file-name-directory
;; 					    (buffer-file-name buf))))
;; 	 (vc-root-dir (vc-find-root buffer-dir repo-sentry)))
;;     (if (and init-sentry vc-root-dir)
;; 	(try-find-best-root
;; 	 (make-dir-list (expand-file-name vc-root-dir))
;; 	 (make-dir-list buffer-dir)
;; 	 '())
;;       vc-root-dir))) ;; default to vc root if sentry not given







;; from: https://stackoverflow.com/questions/21246218/how-can-i-make-emacs-jedi-use-project-specific-virtualenvs

(defun project-directory (buffer-name)
  "Return the root directory of the project that contain the
given BUFFER-NAME. Any directory with a .git or .jedi file/directory
is considered to be a project root."
  (interactive)
  (let ((root-dir (file-name-directory buffer-name)))
    (while (and root-dir
                (not (file-exists-p (concat root-dir ".git")))
                (not (file-exists-p (concat root-dir
                                            ".jedi"))))
      (setq root-dir
            (if (equal root-dir "/")
                nil
              (file-name-directory (directory-file-name
                                    root-dir)))))
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
  (let ((project-name (project-name buffer-file-name)))
    (when project-name (venv-workon project-name))))

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi-setup-venv)
(add-hook 'python-mode-hook 'jedi:setup)










;; projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'ido)

(require 'auto-complete-config)
(ac-config-default)
;; ac menu (optional)
;;(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-auto-show-menu (* ac-delay 2))

;;(require 'jedi)
;;(add-to-list 'ac-sources 'ac-source-jedi-direct)
;;(add-hook 'python-mode-hook 'jedi:setup)

;;(defvar jedi-config:vcs-root-sentinel ".git")
;;(defvar jedi-config:python-module-sentinel "__init__.py")


;; from https://github.com/wernerandrew/drewmacs2

;; Jedi
;; This is a bit of a doozy
;; Requires the aw/guess-best-root-for-buffer defined in
;; custom-functions.el

(defun setup-jedi-extra-args ()
  (let ((project-base (aw/guess-best-root-for-buffer
		       (current-buffer) ".git"
		       "__init__.py")))
    (make-local-variable 'jedi:server-args)
    (when project-base (set 'jedi:server-args (list "--sys-path"
						    project-base)))))

(require 'jedi)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
;; Only manually see in function tooltip
(setq jedi:get-in-function-call-delay 10000000)
(setq jedi:server-command
      (list (executable-find "python")
	    (cadr jedi:server-command)))
(add-to-list 'ac-sources 'ac-source-jedi-direct)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'setup-jedi-extra-args)
;; jedi-specific keybindings
(add-hook 'python-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "M-?") 'jedi:show-doc)
	     (local-set-key (kbd "M-.") 'jedi:goto-definition)
	     (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
	     (local-set-key (kbd "M-/")
			    'jedi:get-in-function-call)))



;; python hooks
(add-hook 'python-mode-hook (lambda () (auto-fill-mode -1))) ;; disable autofill
(add-hook 'python-mode-hook 'autopair-mode)

(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; php hooks
(add-hook 'php-mode-hook (lambda () (auto-fill-mode -1) (abbrev-mode -1))) ;; disable autofill
(add-hook 'php-mode-hook 'autopair-mode)

;; web-mode
(add-hook 'web-mode-hook 'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook 'autopair-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook (lambda () (auto-fill-mode -1))) ;; disable autofill

;; lua-mode
(add-hook 'lua-mode-hook (lambda () (auto-fill-mode -1))) ;; disable autofill

;; Ido
;;(setq ido-everywhere t)
;;(ido-mode t)


(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq flx-ido-use-faces 1)


;; Display ido results vertically, rather than horizontally
(setq ido-decorations
      (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]"
              " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines)
                                           nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  ;; and include our custom ones also
  (define-key ido-completion-map (kbd "M-k") 'ido-next-match)
  (define-key ido-completion-map (kbd "M-i") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

;; ag, if we can find the executable
(if (executable-find "ag") ; Require only if executable exists
    (progn
      (require 'ag)
      ;; same buffer for every search
      (setq ag-reuse-buffers t)
      (setq ag-results-pane nil))) ;; disable for now




;; Smex
(require 'smex)
(setq smex-completion-method 'ivy) 

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; apt install emacs-goodies-el for filladapt
(require 'filladapt)
(add-hook 'org-mode-hook 'turn-on-filladapt-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (flatland)))
 '(jedi:server-command
   (quote
    ("/home/domino/sites/machines/.virtualenv/default/bin/jediepcserver")))
 '(org-agenda-files (quote ("~/.agenda/dominik.org")) t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-deadline-warning-days 10)
 '(projectile-git-command "git ls-files -zc --exclude-standard")
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".pyc" "__pycache__")))
 '(python-environment-virtualenv
   (quote
    ("virtualenv" "--no-site-packages" "--quiet" "--python" "python3"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
