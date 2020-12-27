(message nil);; Debian packages: elpa-use-package elpa-fill-column-indicator fonts-hack

;; This is only needed once, near the top of the file
(eval-when-compile
 (require 'use-package))

;; (add-to-list 'default-frame-alist
             ;; '(font . "Hack-12"))

(set-default-font "-BE5N-Iosevka-normal-normal-expanded-*-18-*-*-*-d-0-iso10646-1")

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(add-to-list 'package-archives
             '("melpa" . "https://stable.melpa.org/packages/") t)

;;ERROR
;;(require 'fill-column-indicator)

(setq initial-major-mode 'text-mode)

(setq backup-inhibited t) ; disable backup

;; (if (file-directory-p "~/.emacs.d/backups")
    ;; (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
  ;; (message "Directory does not exist: ~/.emacs.d/backups"))

;; (filesets-init)

;; (setq backup-by-copying t    ; Don't delink hardlinks
      ;; delete-old-versions t  ; Clean up the backups
      ;; version-control t      ; Use version numbers on backups,
      ;; kept-new-versions 3    ; keep some new versions
      ;; kept-old-versions 2)   ; and some old ones, too

(global-set-key (kbd "C-c d") 'diff-buffer-with-file)
(global-set-key (kbd "C-c R") 'revert-buffer)

(setq ispell-dictionary "polish")

(setq fill-column 79)
;; (setq auto-fill-mode t)
;; (setq-default auto-fill-function 'do-auto-fill)

;; raul:
(setq mouse-yank-at-point t)
(setq column-number-mode 1)
(setq line-number-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; dominik:
(global-linum-mode t)
(set-face-underline-p 'linum nil)
(set-window-margins nil 1)
(column-number-mode t)
(setq linum-format "%4d ")

;; for better scrolling
(setq scroll-conservatively 1000)
(setq scroll-margin 3)

;; default clues color was illegible
(custom-set-faces '(isearch-fail ((t (:background "red"))))
                  '(hl-line ((t (:background "gray21")))))


;; save cursor position in files
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;;?
;; (setq-default c-basic-offset 4)


(when window-system
  (progn
    (global-unset-key (kbd "C-z"))
    (setq scroll-bar-mode nil)
    (tool-bar-mode -1)
    (menu-bar-mode -1)))

(global-unset-key (kbd "C-z"))
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq initial-scratch-message "")


;; (when (featurep 'scroll-bar) (scroll-bar-mode -1))

(defalias 'yes-or-no-p 'y-or-n-p)

;; (global-set-key (kbd "C-x C-b") 'bs-show)

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq show-paren-delay 0)
(show-paren-mode 1)

;; (setq auto-mode-alist (append '((".*tmp/mutt.*" . mail-mode)) auto-mode-alist))
;; (setq auto-mode-alist (append '((".*tmp/neomutt.*" . mail-mode)) auto-mode-alist))

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


;; unset page up and down. Too close to arrows on ThinkPads
(global-unset-key (kbd "<prior>"))
(global-unset-key (kbd "<next>"))
;; too hard:
;; (global-unset-key (kbd "<up>"))
;; (global-unset-key (kbd "<down>"))
;; (global-unset-key (kbd "<left>"))
;; (global-unset-key (kbd "<right>"))

(global-set-key (kbd "ESC <down>") 'ff/comment-and-go-down)
(global-set-key (kbd "ESC <up>") 'ff/uncomment-and-go-up)
;; (global-set-key (kbd "ESC <right>") 'select-next-window)
;; (global-set-key (kbd "M-[") 'select-previous-window)

(global-set-key (kbd "C-x k") 'kill-this-buffer)


(use-package bind-key
  ;; Use C-h B to show a list of user-defined bindings
  :bind ("C-h B" . describe-personal-keybindings))

(bind-key* "<C-return>" 'other-window)

;; (bind-key* "<f8>" '(lambda() (interactive)(find-file "~/.emacs.d/init.el")))
(bind-key* "<f8>" (lambda() (interactive)(dired-other-window "~/.emacs.d/")))

(bind-key* "<f5>" (lambda() (interactive) (switch-to-buffer "*scratch*")))


;; dired

(defun toggle-hidden-in-dired ()
  (interactive)
  (setq dired-actual-switches  (if (equal dired-actual-switches
                                          "-l --group-directories-first")
                                   "-la --group-directories-first"
                                 "-l --group-directories-first"))
  (dired-readin))

(use-package dired
  :init
  ;; FIXME: not working
  (add-hook 'dired-mode-hook #'linum-mode)
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq wdired-allow-to-change-permissions t)
  ;; (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first")
  (setq dired-dwim-target t)
  ;; Hooks' syntax is controlled by the `use-package-hook-name-suffix'
  ;; variable.  The "-hook" suffix is intentional.
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode-hook . hl-line-mode))
  :bind ("C-." . toggle-hidden-in-dired))

(switch-to-buffer "*scratch*")


(use-package fill-column-indicator
  :config
  ;; Set column to show fill-column-indicator
  (setq fci-rule-column '80)
  ;; Set fill-column-indicator color
  (setq fci-rule-color "gray1")
  :hook (python-mode . fci-mode))
