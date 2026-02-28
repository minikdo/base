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

(add-hook 'auto-save-hook 'org-save-all-org-buffers)

(setq org-log-done 'time)  ;; Record the time that a todo was archived.

(setq calendar-latitude 52)
(setq calendar-longitude 21)

(setq holiday-bahai-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)

(setq org-agenda-include-diary t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-deadline-warning-days 10)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-src-tab-acts-natively t)

;; export setting

(setq org-latex-table-centered t)

(setq org-export-with-author nil)
(setq org-export-with-date nil)
(setq org-export-with-toc nil)
(setq org-latex-default-packages-alist (quote
 (("AUTO" "inputenc" nil)
  ("T1" "fontenc" nil)
  ("" "fixltx2e" nil)
  ("" "graphicx" t)
  ("" "longtable" t)
  ("" "float" nil)
  ("" "wrapfig" nil)
  ("" "rotating" nil)
  ("normalem" "ulem" t)
  ("" "amsmath" t)
  ("" "textcomp" t)
  ("" "marvosym" t)
  ("" "wasysym" t)
  ("" "parskip" t)
  ("" "libertine" t)
  ;; ("" "amssymb" t)
  ("" "hyperref" nil)
  "\\tolerance=1000"
  ("a4paper,margin=2.5cm" "geometry" t)
  "\\setlength\\parindent{0pt}")))

;; default browser

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox-esr")

;; agenda

;; (info "(org) Setting options")

(setq org-agenda-custom-commands
      '(("o" "TODOs except cyclic tasks"
         ((tags-todo "-cycl-hidden")))
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
        ("e" "estates todo" entry (file+headline "~/.agenda/estates.org" "Tasks")
	     "* TODO %?\n%U\n")
        ("w" "work done" entry (file+headline "~/.agenda/estates.org" "Work")
	     "* DONE %u\n- %?\n")
        ("p" "Protocol" entry (file "~/.agenda/links.org")
         "* TODO [[%:link][%:description]]\n\n  #+BEGIN_QUOTE\n  %i\n  #+END_QUOTE\n\n  Captured On: %u\n"
         :empty-lines 1
         :immediate-finish t)
	    ("L" "Protocol Link" entry (file "~/.agenda/links.org")
         "* TODO [[%:link][%:description]]\n  Captured On: %U"
         :empty-lines 1
         :immediate-finish t)
        ("m" "Mutt TODO" entry (file "~/.agenda/mails.org")
         "* TODO Reply to: %i\n   Subject: %:description\n   Source: [[%:link][message-id]]\n   %U"
         :empty-lines 1
         :immediate-finish t)
        ))


(org-link-set-parameters "message"
 :follow (lambda (msg-id)
           ;; Using vfolder-from-query to find the ID globally
           (let ((command (format "kitty neomutt -e \"push '<vfolder-from-query>id:%s<enter>'\" 2>/dev/null" msg-id)))
             (message "Opening message: %s" msg-id)
             (shell-command command))))



(defvar my-org-protocol-flag nil
  "Flag to track if the current capture was initiated via org-protocol.")

;; Funkcja pomocnicza do sprzątania ramki
(defun my-org-capture-cleanup-frame (&rest _)
  "Delete frame if it was created via org-protocol."
  (when my-org-protocol-flag
    (setq my-org-protocol-flag nil)
    (delete-frame)))

;; Ustawienie flagi przed uruchomieniem capture przez protokół
(define-advice org-protocol-capture (:before (&rest _) set-flag)
  (setq my-org-protocol-flag t))

;; Podpięcie sprzątania pod finalizację ORAZ porzucenie (kill) capture
(advice-add 'org-capture-finalize :after #'my-org-capture-cleanup-frame)
(advice-add 'org-capture-kill :after #'my-org-capture-cleanup-frame)





(add-hook 'org-mode-hook 'auto-fill-mode)

;; key bindings

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda) 
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; (global-set-key (kbd "<f5>") (lambda() (interactive)(org-agenda-list)))
;; (global-set-key (kbd "<f6>") (lambda() (interactive)(org-todo-list)))
;; (global-set-key (kbd "<f6>") (lambda() (interactive)(org-agenda nil "o")))

(bind-key* "<f6>" #'(lambda() (interactive) (org-agenda-list)))
(bind-key* "<f7>" #'(lambda() (interactive) (org-agenda nil "o")))
