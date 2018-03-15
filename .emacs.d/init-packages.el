;; Use a package manager
;; Requires emacs 24+

(require 'package)

;; Initialize package mode along with all the installed packages
(package-initialize)

(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Init packages after load
(defvar local-mode-init-file-path
  (relative-to-full-path "init-mode.el"))

;; Mode specific init
(load local-mode-init-file-path)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'flatland t)
