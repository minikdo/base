;; unset page up and down
(global-unset-key (kbd "<prior>"))
(global-unset-key (kbd "<next>"))

;; open emacs config file
(global-set-key (kbd "<f6>") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))

;; open dominik.org agenda
(global-set-key (kbd "<f5>") (lambda() (interactive)(find-file "~/.agenda/dominik.org")))

(global-set-key (kbd "ESC <down>") 'ff/comment-and-go-down)
(global-set-key (kbd "ESC <up>") 'ff/uncomment-and-go-up)
(global-set-key (kbd "ESC <right>") 'select-next-window)
;; (global-set-key (kbd "M-[") 'select-previous-window)

