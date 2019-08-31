;; Debian packages: python-mode

(add-hook 'python-mode-hook (lambda () (auto-fill-mode -1))) ;; disable autofill
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'show-paren-mode)
;; (add-hook 'python-mode-hook 'pyvenv-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi-setup-venv)
