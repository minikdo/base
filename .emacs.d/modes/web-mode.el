;; Debian packages: elpa-web-mode
(require 'web-mode)

(add-hook 'web-mode-hook 'emmet-mode) ;; enable Emmet's css abbreviation.
;; (add-hook 'web-mode-hook 'autopair-mode) ;; enable Emmet's css abbreviation.
;;(add-hook 'web-mode-hook (lambda () (auto-fill-mode -1))) ;; disable autofill

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))

(setq web-mode-enable-auto-closing t)

;; ???
;; (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
;; (add-hook 'sgml-mode-hook 'autopair-mode) ;; Auto-start on any markup modes
;; (add-hook 'sgml-mode-hook (lambda () (auto-fill-mode -1))) ;; disable autofill
;; (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook (lambda () (autopair-mode -1))) ;; enable Emmet's css abbreviation.
