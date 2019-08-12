(add-to-list 'load-path "~/.emacs.d/vendor/arduino-mode")
(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode)
                            auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)
