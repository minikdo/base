(add-hook 'mail-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[ \t]*>[ \t]*>[\t]*>.*$"
                                       (0 'match))
                                      ("^[\t]*>[ \t]*>.*$"
                                       (0 'success))))
            (auto-fill-mode)))

(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

