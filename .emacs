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

(setq org-todo-keywords
   '((sequence "TODO" "WAITING" "|" "DONE" )))

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

;;(require 'jedi)
;;(add-to-list 'ac-source 'ac-source-jedi-direct)
;;(add-hook 'python-mode-hook 'jedi:setup)

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
(setq ido-everywhere t)
(ido-mode t)

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
 '(org-deadline-warning-days 10))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
