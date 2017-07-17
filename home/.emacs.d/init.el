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
  :config
  (setq org-todo-keywords '((sequence "TODO(t)" "OPT(o)" "|" "DONE(d)")))
  (setq org-hide-leading-stars t)
  (setq org-ellipsis " ↓")
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (add-hook 'org-mode-hook #'org-indent-mode)
  :bind
  (("C-c l" . org-store-link)))

(provide 'init)
;;; init.el ends here
