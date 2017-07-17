;;; init.el --- init.el
;;; Commentary:
;;; Code:

;; Load package
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")))
; Added in emacs 25.1
(setq package-archive-priorities
      '(("melpa" . 20)
	("elpy" . 10)
	("gnu" . 0)))
(package-initialize)


;; Load use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;(use-package benchmark-init
;  :ensure t
;  :config
;  (benchmark-init/activate))

;; Load org
(use-package org
  :diminish org-indent-mode
  :init
  (org-babel-load-file "~/.emacs.d/config.org")
  :bind
  ("C-c l" . org-store-link)
  ("C-c c" . org-capture)
  :config
  (setq org-hide-leading-stars t)
  (setq org-ellipsis " ↓")
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  (setq org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "SOMEDAY(s)" "|" "DONE(d)" "WONTDO(w)")))

  (setq org-default-notes-file "~/todo.org")
  (setq org-capture-templates
        '(("w" "Work task" entry (file+headline org-default-notes-file "Work")
           "** TODO %?")
          ("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
           "** TODO %?")))

  (add-hook 'org-mode-hook #'org-indent-mode)
  )

(provide 'init)
;;; init.el ends here
