;; Debian packages: elpa-php-mode

(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; php hooks
(add-hook 'php-mode-hook (lambda () (auto-fill-mode -1) (abbrev-mode -1))) ;; disable autofill
(add-hook 'php-mode-hook (lambda () (highlight-indentation-mode 0))) ;; disable autofill
(add-hook 'php-mode-hook 'autopair-mode)
