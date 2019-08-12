;; Debian packages: python-mode

(add-hook 'python-mode-hook (lambda () (auto-fill-mode -1))) ;; disable autofill
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'show-paren-mode)
