;;; package --- Summary

;;; Commentary:


;;; Code:
;; set up the fill column to 80 and the tab width to 2
(setq-default fill-column 80)
(setq-default default-tab-width 2)
(setq-default indent-tab-mode nil)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Bury the scratch buffer but never kill it."
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; syntax highlighting
(global-font-lock-mode t)

;; enable line numbering
(global-linum-mode t)

;; less garbage collection
(setq gc-cons-threshold 20000000)
(setq jit-lock-stealth-time 1
      jit-lock-stealth-load 100
      jit-lock-chunk-size 1000
      jit-lock-defer-time 0.01)

;; don't warn about large files unless larger than 25MB
(setq large-file-warning-threshold (* 25 1024 1024))

;; if you change buffer disable current buffer's mark
(transient-mark-mode t)



(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-%") 'query-replace-regexp)
(global-set-key "\C-cq" 'auto-fill-mod)

;; org
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map [f12] 'org-agenda-list)
(define-key global-map [f11] 'org-clock-current-task)
(global-unset-key (kbd "C-z"))

(setq org-capture-templates
      '(("p" "Project" entry (file+headline "~/git/org/personal.org" "Projects/Tasks")
         "* %?\n  %i\n  %a" :empty-lines-before 1)
        ("n" "Next Action (Todo)" entry (file+headline "~/git/org/personal.org" "Next Actions")
         "* TODO %?\n  %i\n  %a" :empty-lines-before 1)
        ("i" "Inbox item to be processed later" entry (file+headline "~/git/org/personal.org" "INBOX")
         "* %?\n  %i\n  %a" :empty-lines-before 1)
        ("j" "Journal" entry (file+datetree "~/git/org/journal.org")
         "* %?\n\n%i\n\nEntered on %U\n\t%a" :empty-lines-before 1)
        ("m" "Masterthesis" entry (file+headline "~/git/org/university.org" "Tasks")
         "* TODO %?\n%i\nEntered on %U\n\t%a")))


(provide 'init-local)
;;; init-local ends here
