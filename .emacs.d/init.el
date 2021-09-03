;; Install Debian packages:
;; sudo apt install $(grep -ri 'Debian packages:' ~/.emacs.d/modes | awk -F:  '{print $3}' | tr '\n' ' ')

;; Disable garbage collection during the startup time
(setq gc-cons-threshold 536870912
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook (lambda ()
                             ;; restore after startup
                             (setq gc-cons-threshold 16777216
                                   gc-cons-percentage 0.1)))

;; Package configuration
(add-to-list 'package-archives
             '("melpa" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(setq debug-on-error nil)

;; This is only needed once, near the top of the file
;; Debian packages: elpa-use-package
(eval-when-compile
 (require 'use-package))

;; Debian packages: elpa-clues-theme
(load-theme 'clues t)

;; Downloaded from https://github.com/be5invis/iosevka
(set-frame-font "iosevka 12" nil t)
(set-face-attribute 'default nil :height 130)

;; Custom colors
(custom-set-faces
 ;; Default clues color was illegible
 '(isearch-fail ((t (:background "red"))))
 '(hl-line ((t (:background "gray21")))))

;; Inhibit startup messages
(setq inhibit-startup-message t)

(setq initial-major-mode 'text-mode)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))

;; Enable automatic updating a buffer if a file changes on disk
(global-auto-revert-mode 1)

;; Disable backups
(setq backup-inhibited t)

;; Save cursor position in files
(save-place-mode 1)
(setq save-place-file "~/.emacs.d/saveplace")
;; Version 24.5 or older
;; (setq-default save-place t)
;; (require 'saveplace)

;; Custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Dictionary
(setq ispell-dictionary "polish")

;; Fill column
(setq fill-column 79)

;; Mouse
(setq mouse-yank-at-point t)

;; Line and column numbers
(setq column-number-mode t)
(setq line-number-mode t)

;; Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Margins
(set-window-margins nil 1)

;; For better scrolling
(setq scroll-conservatively 1000)
(setq scroll-margin 3)

;; Disable bars
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Handy alias
(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          ;;;;
;;;; === CUSTOM FUNCTIONS === ;;;;
;;;;                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))


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


(defun minikdo/switch-to-scratch-end ()
  "Switch to *scratch*. If in *scratch*, go to end."
  (interactive)
  (if (equal (current-buffer) (get-buffer "*scratch*"))
      (end-of-buffer))
  (switch-to-buffer "*scratch*")
  (recenter-top-bottom))


(defun minikdo/logcheck-clean ()
  "Removes sensitive metadata from logs."
  (interactive)
  (let (
        (p1 (region-beginning))
        (p2 (region-end)))
    (save-restriction
      (narrow-to-region p1 p2)
      (goto-char (point-min))
      (while (search-forward-regexp "^\\w\\{3\\} [ :[:digit:]]\\{11\\} [._[:alnum:]-]+" nil t)
        (replace-match "Jan  1 00:00:00 debian" nil t))
      (goto-char (point-min))
      (while (search-forward-regexp "[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}" nil t)
        (replace-match "1.2.3.4" nil t))
      (goto-char (point-min))
      (while (search-forward-regexp "[._[:alnum:]-]+@[._[:alnum:]-]+" nil t)
        (replace-match "john.doe@do-main.com" nil t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  ;;;;
;;;; === BIND-KEY === ;;;;
;;;;                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Debian packages: elpa-bind-key
;; Unset page up and down. Too close to arrows on ThinkPads
(global-unset-key (kbd "<prior>"))
(global-unset-key (kbd "<next>"))
;; Unser suspend
(global-unset-key (kbd "C-z"))

(use-package bind-key
  ;; Use C-h B to show a list of user-defined bindings
  :bind ("C-h B" . describe-personal-keybindings))

(bind-key* "C-c d" 'diff-buffer-with-file)
(bind-key* "C-c R" 'revert-buffer)
(bind-key* "C-x k" 'kill-this-buffer)
(bind-key* "M-Q" 'unfill-paragraph)
(bind-key* "<C-return>" 'other-window)
(bind-key* "ESC <down>" 'ff/comment-and-go-down)
(bind-key* "ESC <up>" 'ff/uncomment-and-go-up)
(bind-key* "<f5>" '(lambda() (interactive) (minikdo/switch-to-scratch-end)))
(bind-key* "<f8>" '(lambda() (interactive) (find-file "~/.emacs.d/init.el")))
;; (bind-key* "<f8>" '(lambda() (interactive)(dired-other-window "~/.emacs.d/")))

;;;;;;;;;;;;;;;;;;;;;;;
;;;;               ;;;;
;;;; === DIRED === ;;;;
;;;;               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun toggle-hidden-in-dired ()
  (interactive)
  (setq dired-actual-switches
        (if (equal dired-actual-switches
                   "-l --group-directories-first")
            "-la --group-directories-first"
          "-l --group-directories-first"))
  (dired-readin))

(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq wdired-allow-to-change-permissions t)
  ;; (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-GFhlv --group-directories-first")
  (setq dired-dwim-target t)
  ;; Hooks' syntax is controlled by the `use-package-hook-name-suffix'
  ;; variable.  The "-hook" suffix is intentional.
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode-hook . hl-line-mode))
  :bind ("C-." . toggle-hidden-in-dired))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ;;;;
;;;; === PARENS === ;;;;
;;;;                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package paren
  :config
  (show-paren-mode t)
  (setq show-paren-delay 0))

(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package autopair-mode
  :disabled t
  :hook (python-mode))

(use-package electric-pair
  :hook
  (python-mode . electric-pair-local-mode)
  (latex-mode . electric-pair-local-mode)
  (js-mode . electric-pair-local-mode)
  (yaml-mode . electric-pair-local-mode)
  (scss-mode . electric-pair-local-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  ;;;;
;;;; === FCI-MODE === ;;;;
;;;;                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package fill-column-indicator
  :disabled t
  :config
  ;; Set column to show fill-column-indicator
  (setq fci-rule-column '80)
  ;; Set fill-column-indicator color
  (setq fci-rule-color "gray1")
  :hook (python-mode . fci-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       ;;;;
;;;; === AUTO-COMPLETE === ;;;;
;;;;                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Debian packages: elpa-auto-complete
;; (require 'auto-complete-config)
;; (ac-config-default)

;; (setq ac-auto-show-menu (* ac-delay 2))
(use-package auto-complete
  :disabled t
  :init
  (progn
    ;; (ac-config-default)
    ;; (global-auto-complete-mode t)
    )
  :config
  (setq ac-ignore-case nil)
  (setq ac-auto-show-menu (* ac-delay 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          ;;;;
;;;; === COMPANY-COMPLETE === ;;;;
;;;;                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Debian packages: elpa-company
(use-package company
  :disabled t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (global-company-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   ;;;;
;;;; === GIT-ANNEX === ;;;;
;;;;                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; autoloaded?
;; Debian packages: elpa-git-annex
;; (require 'git-annex)
(use-package git-annex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            ;;;;
;;;; === PERSISTENT-SCRATCH === ;;;;
;;;;                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Installed from melpa-stable by package-install
;; https://github.com/Fanael/persistent-scratch
(persistent-scratch-setup-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          ;;;;
;;;; === MULTIPLE-CURSORS === ;;;;
;;;;                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/unmark-next-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   ;;;;
;;;; === WHICH-KEY === ;;;;
;;;;                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Debian packages: elpa-which-key
(use-package which-key
  :config (which-key-mode))

;; Debian packages: elpa-smex
(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (setq smex-completion-method 'ivy)
  (smex-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ;;;;
;;;; === AUCTEX === ;;;;
;;;;                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package latex
  :mode
  ("\\.tex\\'" . latex-mode)
  :config
  (add-to-list 'TeX-command-list
               '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (setq TeX-command-default "XeLaTeX")
  (setq TeX-save-query nil)
  (setq TeX-show-compilation t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    ;;;;
;;;; === PROJECTILE === ;;;;
;;;;                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/modes/jediserver.el")

;; Debian packages: elpa-projectile
(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ido)
  ;; Bind key to projectile-command-map
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ;;;;
;;;; === PYTHON === ;;;;
;;;;                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Debian packages: python-mode
(add-hook 'python-mode-hook (lambda () (auto-fill-mode -1))) ;; disable autofill
;; (add-hook 'python-mode-hook 'show-paren-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi-setup-venv)

;; Debian packages: elpa-elpy
(elpy-enable)
(add-hook 'elpy-mode-hook (lambda ()
                            (highlight-indentation-mode -1)
                            (setq display-line-numbers 1)))


(add-hook 'python-mode-hook (lambda () (auto-complete-mode -1)))
(setq elpy-rpc-python-command "/usr/bin/python3")

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 ;;;;
;;;; === JS-MODE === ;;;;
;;;;                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package js
  ;; :init
  ;; (lambda () (setq display-line-numbers 1)))

(add-hook 'js-mode-hook (lambda ()
                          (yas-minor-mode)
                          (setq display-line-numbers t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  ;;;;
;;;; === WEB-MODE === ;;;;
;;;;                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Debian packages: elpa-web-mode
(require 'web-mode)
(add-hook 'web-mode-hook 'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook (lambda () (autopair-mode -1))) ;; enable Emmet's css abbreviation.
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-pairing t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  ;;;;
;;;; === YAS-MODE === ;;;;
;;;;                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Debian packages: elpa-yasnippet elpa-yasnippet-snippets
;; Fixing a key binding bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
;; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)

;; completing point by some yasnippet key
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
  (let ((original-point (point)))
    (while (and
            (not (= (point) (point-min) ))
            (not
             (string-match "[[:space:]\n]" (char-to-string (char-before)))))
      (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))

(define-key yas-minor-mode-map (kbd "<C-tab>")     'yas-ido-expand)

;;;;
;;;; Load other modes
;;;;

(load-file "~/.emacs.d/modes/flx-ido.el")
(load-file "~/.emacs.d/modes/org.el")

;; (use-package company-jedi
  ;; :config
  ;; (add-hook 'python-mode-hook 'jedi-setup))

;; (defun my/python-mode-hook ()
  ;; (add-to-list 'company-backends 'company-jedi))

;; (add-hook 'python-mode-hook 'my/python-mode-hook)
