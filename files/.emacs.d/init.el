(defvar my-packages '(fsharp-mode
		      markdown-mode
		      rainbow-mode
		      ace-jump-mode
		      expand-region
		      magit
		      ido-ubiquitous
		      haml-mode
		      sass-mode
		      smex))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))
;; check if packages in my-packages are installed; if not, install.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 my-packages)

;;;;;; Non package modules
(add-to-list 'load-path (concat user-emacs-directory
            (convert-standard-filename "modules/")))

;;;;; Settings

;; Hide scrollbars
(scroll-bar-mode -1)
;; Hide toolbar
(tool-bar-mode -1)
(menu-bar-mode 0)
;; Empty scratch buffer
(setq initial-scratch-message "")
;; Don't show startup screen
(setq inhibit-splash-screen t)
;; Highlight matching parentheses, or whole expr if not visible.
(show-paren-mode 1)
(setq show-paren-style 'mixed)
;; Enable region narrowing
(put 'narrow-to-region 'disabled nil)
;; Region is like a tipical selection, type and region is replaced
(pending-delete-mode t)
;; AltGr != Meta
(setq ns-right-alternate-modifier nil)
;; Bar cursor
(setq-default cursor-type 'bar)
;; ispell bin location
(setq ispell-program-name "/usr/local/bin/ispell")
;; Always y/n o p
(fset 'yes-or-no-p 'y-or-n-p)
;; Add final newline on save
(setq require-final-newline t)
;; No backup files
(setq make-backup-files nil)
;; Don't autosave
(setq auto-save-default nil)
;; Show column number
(setq column-number-mode  t)
;; encodig utf-8 is a safe file-local variable value
(setq safe-local-variable-values (quote ((encoding . utf-8))))

;; Load monokai theme
(require 'monokai-theme)
(load-theme 'monokai t)
;; Monokai is a safe theme
(setq custom-safe-themes (quote ("7fde77d5b9fb5b203c2115ddf4dd0b4086390b55cc65975e2321c3d62b1398b1" default)))

(set-face-attribute 'default nil :height 140)

;; css/scss
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
;; (setq css-indent-offset 4)
;; js
;;(setq js-indent-level 4)
;; markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
;; Ruby
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
;; html
(add-to-list 'auto-mode-alist '("\\.html$" . jinja2-mode))
;; fish
(add-to-list 'auto-mode-alist '("\\.fish$" . conf-mode))

;;;;; key bindings and other customizations

;; Delete trailing whitespaces after saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; define the function to kill the characters from the cursor
;; to the beginning of the current
;;(global-set-key [M-delete] 'backward-kill-line)
(defun backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0))

;; insert date into buffer at point
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %Y-%m-%d - %l:%M %p")))

;; org-mode
(setq org-default-notes-file "~/notes.org")
(define-key global-map "\C-cc" 'org-capture)

;; smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; ido-mode
(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;; expand-region
(global-set-key (kbd "M-RET") 'er/expand-region)

;; buffers
(global-set-key (kbd "C-ñ C-ñ") '(switch-to-buffer nil))
(global-set-key (kbd "C-ñ C-n") 'next-buffer)
(global-set-key (kbd "C-ñ C-p") 'previous-buffer)

;; Start server
(require 'server)
(setq server-socket-dir "/tmp/emacs-shared")
(if (display-graphic-p)
    (unless (server-running-p)
      (server-start)))

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Custom magit diff colors
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (set-face-background 'magit-item-highlight "black")))
(put 'scroll-left 'disabled nil)

;; Smart tabs
;;(global-set-key [(tab)] 'smart-tab)
(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (hippie-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (hippie-expand nil)
        (indent-for-tab-command)))))

(global-set-key (kbd "M-SPC") 'hippie-expand)

;; Automatically enable flymake-mode upon opening any file for which
;; syntax check is possible
(add-hook 'find-file-hook 'flymake-find-file-hook)
;; Show flymake error in minibuffer after a delay
(setq help-at-pt-timer-delay 0.7)
(setq help-at-pt-display-when-idle '(flymake-overlay))

;; python syntax check
(defvar pycheck-bin (concat user-emacs-directory
            (convert-standard-filename "bin/pycheckers")))

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-inplace))
       (local-file (file-relative-name
            temp-file
            (file-name-directory buffer-file-name))))
      (list pycheck-bin  (list local-file))))
   (add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pyflakes-init)))

;; grep-ed
(require 'grep-ed)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:underline (:color "red" :style wave))))))
