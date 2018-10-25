;; fundamental
(add-hook 'fundamental-mode-hook (lambda () (setq fill-column 999))) ;; disable autofill

;; html
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook 'autopair-mode) ;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook (lambda () (auto-fill-mode -1))) ;; disable autofill
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.


;; python
(elpy-enable)
; Fixing a key binding bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)


;; Completing point by some yasnippet key
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


;; Jedi
;; This is a bit of a doozy
;; Requires the aw/guess-best-root-for-buffer defined in
;; custom-functions.el

;; (defun setup-jedi-extra-args ()
  ;; (let ((project-base (aw/guess-best-root-for-buffer
                       ;; (current-buffer) ".git" "__init__.py")))
    ;; (make-local-variable 'jedi:server-args)
    ;; (when project-base (set 'jedi:server-args (list "--sys-path" project-base
                                                    ;; "--virtual-env" "~/.virtualenvs/machines")))))


;; (require 'jedi)
;; (setq jedi:setup-keys t)
;; (setq jedi:complete-on-dot t)
;; ;; Only manually see in function tooltip
;; (setq jedi:get-in-function-call-delay 10000000)
;; (setq jedi:server-command
      ;; (list (executable-find "python")
            ;; (cadr jedi:server-command)))
;; (add-to-list 'ac-sources 'ac-source-jedi-direct)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (add-hook 'python-mode-hook 'setup-jedi-extra-args)
;; ;; jedi-specific keybindings
;; (add-hook 'python-mode-hook
          ;; '(lambda ()
             ;; (local-set-key (kbd "M-?") 'jedi:show-doc)
             ;; (local-set-key (kbd "M-.") 'jedi:goto-definition)
             ;; (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
             ;; (local-set-key (kbd "M-/") 'jedi:get-in-function-call)))


(defun project-directory (buffer-name)
  "Return the root directory of the project that contain the
given BUFFER-NAME. Any directory with a .git or .jedi file/directory
is considered to be a project root."
  (interactive)
  (let ((root-dir (file-name-directory buffer-name)))
    (while (and root-dir
                (not (file-exists-p (concat root-dir ".git")))
                (not (file-exists-p (concat root-dir ".jedi"))))
      (setq root-dir
            (if (equal root-dir "/")
                nil
              (file-name-directory (directory-file-name root-dir)))))
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



;; which-key
;;(require 'which-key)
(which-key-mode)

;; projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'ido)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
;; ac menu (optional)
;;(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-auto-show-menu (* ac-delay 2))

;; python hooks
(add-hook 'python-mode-hook (lambda () (auto-fill-mode -1))) ;; disable autofill
(add-hook 'python-mode-hook 'autopair-mode)

(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; php hooks
(add-hook 'php-mode-hook (lambda () (auto-fill-mode -1) (abbrev-mode -1))) ;; disable autofill
(add-hook 'php-mode-hook (lambda () (highlight-indentation-mode 0))) ;; disable autofill
(add-hook 'php-mode-hook 'autopair-mode)

;; web-mode
(add-hook 'web-mode-hook 'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook 'autopair-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook (lambda () (auto-fill-mode -1))) ;; disable autofill

;; lua-mode
(add-hook 'lua-mode-hook (lambda () (auto-fill-mode -1))) ;; disable autofill

;; ido-mode (disabled)
;;(setq ido-everywhere t)
;;(ido-mode t)

;; flx-ido
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



;; Smex
(require 'smex)
(setq smex-completion-method 'ivy) 
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(require 'git-annex)

(add-hook 'mail-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[ \t]*>[ \t]*>[\t]*>.*$"
                                       (0 'match))
                                      ("^[\t]*>[ \t]*>.*$"
                                       (0 'success))))))

(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
