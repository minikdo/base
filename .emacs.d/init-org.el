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
	       "* TODO %? %^g\n %U\n")
	      ("n" "note" entry (file+headline (org-file-path "notes.org") "Notes")
	       "* %? :NOTE:\n %U\n")
	      ("j" "journal" entry (file (org-file-path "journal.org"))
	       "* %U %?\n\n" )
	      ("s" "shopping" table-line (file+headline (org-file-path "dominik.org") "Zakupy")
	       "|%U|%?| |" )
	      )))

;; to clock capture entry
;; :clock-in t :clock-resume t

;;(add-hook 'org-mode-hook 'abbrev-mode)
;;(add-hook 'org-mode-hook 'flyspell-mode)
;; (add-hook 'org-capture-mode-hook 'flyspell-mode)
;; (add-hook 'org-capture-mode-hook 'abbrev-mode)

(custom-set-variables
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-deadline-warning-days 10))


;; apt install emacs-goodies-el for filladapt
(require 'filladapt)
(add-hook 'org-mode-hook 'turn-on-filladapt-mode)

