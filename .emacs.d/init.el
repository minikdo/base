(server-start)

(add-to-list 'package-archives
             '("melpa" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

(load-theme 'clues t)

(set-frame-font "iosevka 10" nil t)
(set-face-attribute 'default nil :height 130)

;; Custom colors
(custom-set-faces
 ;; Default clues color was illegible
 '(isearch-fail ((t (:background "red"))))
 '(hl-line ((t (:background "gray21")))))

(setq inhibit-startup-message t)

(setq initial-major-mode 'text-mode)

(set-frame-parameter (selected-frame) 'alpha '(95 . 95)) ;; transparency

;; Enable automatic updating a buffer if a file changes on disk
;; needed for storing links via org-protocol
(global-auto-revert-mode 1)

;; Prevent kill emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;; Disable backups
(setq backup-inhibited t)

;; Save cursor position in files
(save-place-mode 1)
(setq save-place-file "~/.emacs.d/saveplace")
;; Version 24.5 or older
;; (setq-default save-place t)
;; (require 'saveplace)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(setq ispell-dictionary "polish")

(setq fill-column 79)

(setq mouse-yank-at-point t)

(setq column-number-mode t)
(setq line-number-mode t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(set-window-margins nil 1)

(setq
 scroll-conservatively 1000                     ;; only 'jump' when moving this far
 scroll-margin 4                                ;; scroll N lines to screen edge
 scroll-step 1                                  ;; keyboard scroll one line at a time
 mouse-wheel-scroll-amount '(6 ((shift) . 1))   ;; mouse scroll N lines
 mouse-wheel-progressive-speed nil              ;; don't accelerate scrolling

 redisplay-dont-pause t                         ;; don't pause display on input

 ;; Always redraw immediately when scrolling,
 ;; more responsive and doesn't hang!
 fast-but-imprecise-scrolling nil
 jit-lock-defer-time 0
 )

;; Handy alias
(defalias 'yes-or-no-p 'y-or-n-p)

;; (spacious-padding-mode -1)

;; ------
;; Custom functions
;; ------

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


(defun my-comment-and-go-down (arg)
  "Comments and goes down ARG lines."
  (interactive "p")
  (condition-case nil
      (comment-region (point-at-bol) (point-at-eol)) (error nil))
  (next-line 1)
  (if (> arg 1) (my-comment-and-go-down (1- arg))))


(defun my-uncomment-and-go-up (arg)
  "Uncomments and goes up ARG lines."
  (interactive "p")
  (condition-case nil
      (uncomment-region (point-at-bol) (point-at-eol)) (error nil))
  (next-line -1)
  (if (> arg 1) (my-uncomment-and-go-down (1- arg))))


(defun my-switch-to-scratch-end ()
  "Switch to *scratch*. If in *scratch*, go to end."
  (interactive)
  (if (equal (current-buffer) (get-buffer "*scratch*"))
      (end-of-buffer))
  (switch-to-buffer "*scratch*")
  (recenter-top-bottom))


(defun my-switch-dictionary (choice) ;; by dogsleg
   "Switch between language dictionaries (optionally switched to CHOICE value)."
   (interactive "cChoose:  (1) English | (2) Polski")
    (cond ((eq choice ?1)
           (setq ispell-dictionary "english")
           (ispell-kill-ispell)
           (message "Switched to English."))
          ((eq choice ?2)
           (setq ispell-dictionary "polish")
           (ispell-kill-ispell)
           (message "Switched to Polish."))
          (t (message "No changes have been made."))))


(setq my-themes '(clues leuven))
(setq my-themes-index 0)

(defun my-cycle-theme () ;; by Ivan
  (interactive)
  (setq my-themes-index (% (1+ my-themes-index) (length my-themes)))
  (my-load-indexed-theme))


(defun my-load-indexed-theme ()
  (my-try-load-theme (nth my-themes-index my-themes)))


(defun my-try-load-theme (theme)
  (if (ignore-errors (load-theme theme :no-confirm))
      (mapcar #'disable-theme (remove theme custom-enabled-themes))
    (message "Unable to find theme file for ‘%s’" theme)))


(defun my-edit-configuration ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))


;; from https://zck.org/balance-emacs-windows
(seq-doseq (fn (list #'split-window #'delete-window))
  (advice-add fn
              :after
              #'(lambda (&rest args) (balance-windows))))

;; ------
;; Printing settings
;; ------

(require 'ps-print)
(setq ps-print-footer nil)
(setq ps-print-header nil)

(setq ps-font-info-database
      (append
       '((Iosevka
          (fonts (normal      . "Iosevka")
                 (bold        . "Iosevka-Bold")
                 (italic      . "Iosevka-Italic")
                 (bold-italic . "Iosevka-Bold-Italic"))
          (size           . 8.0)
          (line-height    . 13.0)
          (space-width    . 6.04688)
          (avg-char-width . 6.04688)))
       ps-font-info-database))
(setq ps-font-family 'Iosevka)
(setq ps-font-size 8)


;; ------
;; Default-text-scale (melpa)
;; ------

(use-package default-text-scale
  :ensure t
  :if window-system
  :init
  (add-hook 'after-init-hook 'default-text-scale-mode))


;; ------
;; Bind-key (external)
;; ------

(global-unset-key (kbd "<prior>")) ;; too close to arrows on ThinkPads
(global-unset-key (kbd "<next>"))
(global-unset-key (kbd "C-z")) ;; unset suspend

(bind-keys*
 ;; built-in functions
 ("C-h B"      . describe-personal-keybindings)
 ("<C-return>" . other-window)
 ("M-Q"        . unfill-paragraph)
 ("C-c d"      . diff-buffer-with-file)
 ("C-c R"      . revert-buffer)
 ("C-x k"      . kill-current-buffer)
 ("<f9>"       . flyspell-mode)
 ("M-<f9>"     . whitespace-mode)
 ;; my custom functions
 ("ESC <down>" . my-comment-and-go-down)
 ("ESC <up>"   . my-uncomment-and-go-up)
 ("<f5>"       . my-switch-to-scratch-end)
 ("<f8>"       . my-cycle-theme)
 ("M-<f8>"     . my-edit-configuration)
 ("C-c M-s"    . my-switch-dictionary))


;; ------
;; Dired
;; ------

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


;; ------
;; Parens (builtin)
;; ------

(use-package paren
  :config
  (show-paren-mode t)
  (setq show-paren-delay 0))


;; ------
;; Paredit (external)
;; ------

(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook                  #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook            #'paredit-mode)
  (add-hook 'ielm-mode-hook                        #'paredit-mode)
  (add-hook 'lisp-mode-hook                        #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))


;; ------
;; Electric-pair (builtin)
;; ------

(use-package electric-pair
  :hook
  (python-mode . electric-pair-local-mode)
  (latex-mode  . electric-pair-local-mode)
  (js-mode     . electric-pair-local-mode)
  (yaml-mode   . electric-pair-local-mode)
  (scss-mode   . electric-pair-local-mode))


;; ------
;; FCI-mode (disabled) (not installed) (external)
;; ------

(use-package fill-column-indicator
  :disabled t
  :config
  ;; Set column to show fill-column-indicator
  (setq fci-rule-column '80)
  ;; Set fill-column-indicator color
  (setq fci-rule-color "gray1")
  :hook (python-mode . fci-mode))


;; ------
;; Git-annex (external)
;; ------

(use-package git-annex)


;; ------
;; Multiple-cursors (melpa)
;; ------

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/unmark-next-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))


;; ------
;; Which-key (external)
;; ------

(use-package which-key
  :config (which-key-mode))


;; ------
;; Latex (builtin)
;; ------

(use-package latex
  :mode
  ("\\.tex\\'" . latex-mode)
  :config
  (add-to-list 'TeX-command-list
               '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (setq TeX-command-default "XeLaTeX")
  (setq TeX-save-query nil)
  (setq TeX-show-compilation t))


;; ------
;; Projectile (external)
;; ------

(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ido)
  ;; Bind key to projectile-command-map
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map)))


;; ------
;; Company (external)
;; ------

(use-package company
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        ;; invert the navigation direction if the the completion popup-isearch-match
        ;; is displayed on top (happens near the bottom of windows)
        company-tooltip-flip-when-above t)
  (global-company-mode -1)
  :hook ((prog-mode      . company-mode)))


;; ------
;; Flymake (disabled)
;; ------

(flymake-mode -1)

;; ------
;; Eldoc
;; ------

(setq eldoc-idle-delay '1)
(setq eldoc-echo-area-use-multiline-p '1)

;; ------
;; Flycheck
;; ------

(use-package flycheck
  :disabled t ;; enabled manually
  :hook
  (prog-mode . flycheck-mode))


;; ------
;; Python (builtin)
;; ------

;; (add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  ;;;;
;;;; === WEB-MODE === ;;;;
;;;;                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'web-mode)
;; (add-hook 'web-mode-hook 'emmet-mode) ;; enable Emmet's css abbreviation.
;; (add-hook 'web-mode-hook (lambda () (autopair-mode -1))) ;; enable Emmet's css abbreviation.
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (setq web-mode-engines-alist
      ;; '(("django" . "\\.html\\'")))
;; (setq web-mode-enable-auto-closing t)
;; (setq web-mode-enable-auto-pairing t)

(use-package web-mode
  :config
  (setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    ;;;;
;;;; === MHTML-MODE === ;;;;
;;;;                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package mhtml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.vue?\\'" . mhtml-mode))
  :config
  (add-hook 'html-mode-hook #'emmet-mode)
  (add-hook 'html-mode-hook #'smartparens-mode))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)


;; ---------
;; mail-mode
;; ---------

(use-package mail
  :init
  (add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
  (add-to-list 'auto-mode-alist '("/neomutt" . mail-mode))
  (setq ispell-dictionary "polish")
  :hook
  (mail-mode . footnote-mode)
  (mail-mode . flyspell-mode)
  (mail-mode . yas-minor-mode)
  (mail-mode . (lambda ()
                 (font-lock-add-keywords nil
                                         '(("^[ \t]*>[ \t]*>[\t]*>.*$"
                                            (0 'match))
                                           ("^[\t]*>[ \t]*>.*$"
                                            (0 'success))))
                 (auto-fill-mode))))

;; ------
;; Yasnippet
;; ------

(use-package yasnippet
  ;; Enable yasnippet globally
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode 1)
  ;; Bind keys
  :bind (("C-c y l" . yas-describe-tables)
         ("C-c y x" . yas-expand)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y d" . yas-load-directory)
         ("C-c y n" . yas-new-snippet)
         ("C-c y a" . yas-reload-all)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y f" . yas-visit-snippet-file))
  :hook (prog-mode . yas-minor-mode)
  )

(use-package yasnippet-snippets
  :disabled t
  ;; Initialize yasnippet-snippets after yasnippet itself
  :after yasnippet
  :config (yasnippet-snippets-initialize))


;; ------
;; Smex (external)
;; 
;; Smex is a M-x enhancement for Emacs. Built on top of Ido,
;; it provides a convenient interface to your recently and most
;; frequently used commands. And to all the other commands, too.
;; ------

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (setq smex-completion-method 'ivy)
  (smex-initialize))


;; ------
;; Ido
;;
;; Ido is part of Emacs, starting with release 22. The ido.el package
;; by KimStorm lets you interactively do things with buffers and
;; files. As an example, while searching for a file with C-x C-f, ido
;; can helpfully suggest the files whose paths are closest to your
;; current string, allowing you to find your files more quickly.
;; ------

(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere t)
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq flx-ido-use-faces 1))


;; ------
;; Flx-ido (external)
;;
;; This package provides a more powerful alternative to `ido-mode''s
;; built-in flex matching.
;; ------

(use-package flx-ido
  :config
  (setq ido-decorations
        (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]"
                " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
  (defun ido-disable-line-truncation ()
    (set (make-local-variable 'truncate-lines) nil))
  :hook
  (ido-minibuffer-setup . ido-disable-line-truncation)
  :bind
  (:map ido-common-completion-map
        ("C-n" . ido-next-match)
        ("C-p" . ido-prev-match)))

(fido-mode 1) ;; an ido-like facsimile built on top of Emacs’s Icomplete engine.

(use-package redtick
  :config
  (setq redtick-play-sound t)
  (setq redtick-sound-volume "1")
  (setq redtick-history-file nil))

(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode))

;; ------
;; Source other modes
;; ------

(load-file "~/.emacs.d/modes/org.el")
