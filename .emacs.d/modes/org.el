;; Debian packages: elpa-htmlize

;; Org-mode settings
(setq org-directory "~/.agenda")

(setq org-agenda-files '("~/.agenda"))

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

;; (setq org-index-file (org-file-path "agenda.org"))

(setq org-archive-location
      (concat "~/.agenda/archive/archive.org" "::* From %s"))

(setq org-default-notes-file (org-file-path "/agenda.org"))

;; Hitting C-c C-x C-s will mark a todo as done and move it to an appropriate
;; place in the archive.
;; FIXME unbind
(defun hrs/mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

;; variables

(setq org-log-done 'time)  ;; Record the time that a todo was archived.

(setq calendar-latitude 52)
(setq calendar-longitude 21)

(setq org-agenda-include-diary t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-deadline-warning-days 10)

;; export setting

(setq org-export-with-author nil)
(setq org-export-with-date nil)
(setq org-export-with-toc nil)
(setq org-latex-default-packages-alist (quote
 (("AUTO" "inputenc" nil)
  ("T1" "fontenc" nil)
  ("" "fixltx2e" nil)
  ("" "graphicx" t)
  ("" "longtable" nil)
  ("" "float" nil)
  ("" "wrapfig" nil)
  ("" "rotating" nil)
  ("normalem" "ulem" t)
  ("" "amsmath" t)
  ("" "textcomp" t)
  ("" "marvosym" t)
  ("" "wasysym" t)
  ;; ("" "amssymb" t)
  ("" "hyperref" nil)
  "\\tolerance=1000"
  ("margin=2.5cm" "geometry" t)
  "\\setlength\\parindent{0pt}")))
 
;; default browser

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox-esr")

;; agenda

;; (info "(org) Setting options")

(setq org-agenda-custom-commands
      '(("o" "TODOs except cyclic tasks"
         ((tags-todo "-cycl")))
        ("O" "TODOs except cyclic tasks with day agenda"
         ((agenda)
          (tags-todo "-cycl"))
         ((org-agenda-span 'day)))
        ("X" agenda ""
         ((org-agenda-span (quote day)))
         ("~/.agenda/agenda.txt"))))


(setq org-todo-keywords
      '((sequence "TODO" "WAIT" "|" "DONE" )))

(setq org-todo-keyword-faces
      '(("WAIT" . "violet")))

;;
;; Capture templates and org-protocol
;;

(require 'org-protocol)

(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/.agenda/agenda.org" "Tasks")
	     "* TODO %?\n%U\n")
	    ("j" "journal" entry (file "~/.agenda/journal.org")
	     "* %U %?\n" )
        ("n" "nie" entry (file "~/.agenda/nie.org")
	     "* TODO %?\n%U\n")
        ("i" "issues" entry (file "~/.agenda/issues.org")
	     "* TODO %?\n%U\n")
        ("m" "Mail" entry
         (file+headline "~/.agenda/agenda.org" "Incoming")
         "* TODO %?\n%U\nSource: %:link\n\n%i"
         :empty-lines 1
         )))

(add-hook 'org-capture-mode-hook 'delete-other-windows)
(setq my-org-protocol-flag nil)
(defadvice org-capture-finalize (after delete-frame-at-end activate)
  "Delete frame at remember finalization"
  (progn (if my-org-protocol-flag (delete-frame))
         (setq my-org-protocol-flag nil)))
(defadvice org-capture-kill (after delete-frame-at-end activate)
  "Delete frame at remember abort"
  (progn (if my-org-protocol-flag (delete-frame))
         (setq my-org-protocol-flag nil)))
(defadvice org-protocol-capture (before set-org-protocol-flag activate)
  (setq my-org-protocol-flag t))


(add-hook 'org-mode-hook 'auto-fill-mode)

;; key bindings

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda) 
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; (global-set-key (kbd "<f5>") (lambda() (interactive)(org-agenda-list)))
;; (global-set-key (kbd "<f6>") (lambda() (interactive)(org-todo-list)))
;; (global-set-key (kbd "<f6>") (lambda() (interactive)(org-agenda nil "o")))

(bind-key* "<f6>" '(lambda() (interactive) (org-agenda-list)))
(bind-key* "<f7>" '(lambda() (interactive) (org-agenda nil "o")))
