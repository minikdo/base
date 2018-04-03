;; unset page up and down
(global-unset-key (kbd "<prior>"))
(global-unset-key (kbd "<next>"))

;; org-mode
(global-set-key (kbd "<f5>") (lambda() (interactive)(org-todo-list)))
(global-set-key (kbd "<f6>") (lambda() (interactive)(org-agenda-list)))
(global-set-key (kbd "<f7>") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))

(global-set-key (kbd "ESC <down>") 'ff/comment-and-go-down)
(global-set-key (kbd "ESC <up>") 'ff/uncomment-and-go-up)
(global-set-key (kbd "ESC <right>") 'select-next-window)
;; (global-set-key (kbd "M-[") 'select-previous-window)

