(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" "4cbec5d41c8ca9742e7c31cc13d8d4d5a18bd3a0961c18eb56d69972bbcf3071" "b9a5fa17143829d510afd6057d226166ac398973fd3234cbd1e6ef6eae9c5b5f" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(fci-rule-color "#eee8d5")
 '(org-agenda-files (quote ("~/.calendar/dominik.org")))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))

;; org-mode settings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda) 
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)


;; disable backup
(setq backup-inhibited t)

;; Solarized theme
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
;;(load-theme 'solarized)

;; other settings
(setq-default fill-column 80)
(setq auto-fill-mode t)
(setq-default auto-fill-function 'do-auto-fill)

;; line numbers
(global-linum-mode t)
(set-window-margins nil 1)
(column-number-mode t)
(setq linum-format "%4d ")

;; deleting a single word backwards
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

;; change dictionary to polish
(setq ispell-dictionary "polish")

;; load plugins
(add-to-list 'load-path "~/.emacs.d/plugins")

;; lua plugin
(require 'lua-block)
(lua-block-mode t)

(defun kill-to-end:b ()
  "Kills text from the cursor postion to the end of the buffer. 
  This command adds the killed text to the kill-ring"
    (interactive)
      (save-excursion
	(let ((beg (point)) (end (point-max)))
          (kill-region beg end))))



;; html css emmet-mode from
;; https://github.com/smihica/emmet-mode
(add-to-list 'load-path "~/.emacs.d/plugins/emmet-mode")
(require 'emmet-mode)

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook 'autopair-mode) ;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook (lambda () (auto-fill-mode -1))) ;; disable autofill
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;;(require 'jedi)
;;(add-to-list 'ac-source 'ac-source-jedi-direct)
;;(add-hook 'python-mode-hook 'jedi:setup)

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

(setq ido-everywhere t)
(ido-mode t)

(require 'smex)
(setq smex-completion-method 'ivy) 

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;(require 'org-bullets)
;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'org-mode-hook 'abbrev-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))
