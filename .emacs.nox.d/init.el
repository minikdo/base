(package-initialize)

(setq debug-on-error nil)

(eval-when-compile
 (require 'use-package))

(setq backup-inhibited t) ;; disable backup

(global-auto-revert-mode 1)

(use-package flyspell
  :init
  (progn
    (flyspell-mode -1))
  :config
  (progn 
    (setq ispell-program-name "aspell")
    (setq ispell-dictionary "polish")    
    (setq ispell-list-command "--list") ;; run flyspell with aspell, not ispell
    ))

(defun switch-dictionary (choice)
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

(global-set-key (kbd "C-c M-s") 'switch-dictionary)

(setq fill-column 79)

(setq column-number-mode t)
(setq line-number-mode t)

(setq mouse-yank-at-point t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq scroll-conservatively 1000)
(setq scroll-margin 3)

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq initial-scratch-message "")

(defalias 'yes-or-no-p 'y-or-n-p)

(setq custom-file "~/.emacs.nox.d/custom.el")
(load custom-file 'noerror)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "<prior>"))
(global-unset-key (kbd "<next>"))

(use-package bind-key
  :bind ("C-h B" . describe-personal-keybindings))

(bind-key* "<C-return>" 'other-window) ;; not working
(bind-key* "C-x k" 'kill-this-buffer)
(bind-key* "<f5>" (lambda() (interactive) (switch-to-buffer "*scratch*")))
(bind-key* "M-<f8>" (lambda() (interactive) (find-file "~/.emacs.nox.d/init.el")))
(bind-key* "<f9>" 'flyspell-mode) ;; toggle mode

(use-package paren
  :config
  (show-paren-mode t)
  (setq show-paren-delay 0))


(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere t))


(use-package flx-ido
  :config
  (setq ido-decorations
      (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]"
              " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))))

(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  ;; and include our custom ones also
  ;;(define-key ido-completion-map (kbd "M-k") 'ido-next-match)
  ;;(define-key ido-completion-map (kbd "M-i") 'ido-prev-match)
  )

(add-hook 'ido-setup-hook 'ido-define-keys)


(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (setq smex-completion-method 'ivy) 
  (smex-initialize))


(use-package yasnippet
  ;; Enable yasnippet globally
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.nox.d/snippets"))
  (yas-global-mode 1)
  ;; Bind keys
  :bind (("C-c y l" . yas-describe-tables)
         ("C-c y x" . yas-expand)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y d" . yas-load-directory)
         ("C-c y n" . yas-new-snippet)
         ("C-c y a" . yas-reload-all)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y f" . yas-visit-snippet-file)))

(use-package yasnippet-snippets
  :disabled t
  ;; Initialize yasnippet-snippets after yasnippet itself
  :after yasnippet
  :config (yasnippet-snippets-initialize))


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
  (setq dired-listing-switches
        "-GFhlv --group-directories-first")
  (setq dired-dwim-target t)
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode-hook . hl-line-mode))
  :bind (:map dired-mode-map
              ("C-." . toggle-hidden-in-dired)))


(load-file "~/.emacs.d/modes/org.el")

(load-theme 'modus-vivendi t)
