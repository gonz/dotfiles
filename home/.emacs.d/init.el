;;; init.el --- Emacs configuration of Gonzalo Saavedra
;;
;; Copyright (c) 2010-2016 Gonzalo Saavedra <gonzalosaavedra@gmail.com>
;;
;; Author: Gonzalo Saavedra <gonzalosaavedra@gmail.com>
;; URL: https://gihub.com/gonz/dotfiles/
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:

;; My emacs configuration file

;;; Code:


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)


;; Manually set PATH env var and exec-path
(defvar my-paths '("/usr/local/opt/coreutils/libexec/gnubin"
		   "/usr/local/bin"
		   "/usr/bin"
		   "/bin"
		   "/usr/sbin"
		   "/sbin"
		   "/Users/gonz/Bin"
		   "/Users/gonz/node_modules/.bin"))
(setenv "PATH" (mapconcat 'identity my-paths ":") )
(setq exec-path (append my-paths (list "." exec-directory)))


;;;; Theme

(add-to-list 'custom-theme-load-path
             (convert-standard-filename
              (concat user-emacs-directory "themes")))
(load-theme 'gmonokai t)
(setq custom-safe-themes
      (quote
       ("27c0599626f0a132bbdc06c55e8cd20693d2ca4f07e386a81dad86d57b9e3c64"
        default)))


;;;; Set default frame size/position

(if window-system
    (add-to-list 'default-frame-alist '(fullscreen . maximized)))



;;;; Settings

;; Hide scrollbars
(scroll-bar-mode -1)
;; Hide toolbar
(tool-bar-mode -1)
(menu-bar-mode 0)
;; Empty scratch buffer
(setq initial-scratch-message "")
;; Don't show startup screen
(setq inhibit-splash-screen t)
;; Always y/n o p
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable features disabled by default
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; AltGr != Meta
(setq ns-right-alternate-modifier nil)
;; Bar cursor
(setq-default cursor-type 'bar)
;; Show column number
(setq column-number-mode  t)
;; Font size
(set-face-attribute 'default nil :height 140)

;; No backup files
(setq make-backup-files nil)
;; Don't autosave
(setq auto-save-default nil)

;; Enable matching parentheses highlight
(show-paren-mode 1)
;; highlight brackets if visible, else entire expression
(setq show-paren-style 'mixed)

;; Don't ask for confirm when following symlinks that point to
;; versioned files, always follow.
(setq vc-follow-symlinks t)

;; Highlight line
(global-hl-line-mode)

;; Always use spaces when indenting (unless overridden for buffer)
(setq-default indent-tabs-mode nil)
;; Delete trailing whitespaces after saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Region is like a tipical selection, type and region is replaced
(pending-delete-mode t)

;; ispell bin location
(setq ispell-program-name "/usr/local/bin/ispell")

;; Add final newline on save
(setq require-final-newline t)

;; encodig utf-8 is a safe file-local variable value
(setq safe-local-variable-values (quote ((encoding . utf-8))))

;; org-mode
(define-key global-map "\C-cc" 'org-capture)

;; help-at-pt
(setq help-at-pt-timer-delay 0.3)
(help-at-pt-cancel-timer)
(help-at-pt-set-timer)

;; uniquify
(require 'uniquify)
;; include part of the file directory name at the beginning of the buffer name
(setq uniquify-buffer-name-style 'forward)

;; dired
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))


;; Fix python info-lookup-symbol
(require 'info-look)
(info-lookup-add-help
 :mode 'python-mode
 :regexp "[[:alnum:]_]+"
 :doc-spec
 '(("(python)Index" nil "")))


;;;; Remote Packages

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))


(let ((default-directory (convert-standard-filename (concat user-emacs-directory "modules"))))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))


;; Load use-package (install if needed)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant


;; Server
(use-package server
  :ensure t
  :init
  (server-mode 1)
  :config
  (unless (server-running-p)
    (server-start)))

;; buffers
(define-key global-map (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Python" (mode . python-mode))
         ("Magit" (name . "\*magit"))
         ("emacs-config" (filename . ".emacs.d"))
	 ("Org" (or (mode . org-mode)
		    (filename . "OrgMode")))
	 ("Help" (or (name . "\*Help\*")
		     (name . "\*Apropos\*")
		     (name . "\*info\*"))))
      (ibuffer-vc-generate-filter-groups-by-vc-root)))
(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-switch-to-saved-filter-groups "default")))
(use-package ibuffer-vc
  :ensure t)


(use-package hide-region
  :ensure t)


(use-package switch-window
  :ensure t
  :bind
  ("C-x o" . switch-window)
  ("C-ñ C-ñ" . switch-window)
  ("C-ñ ñ" . switch-window)
  :init
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" "ñ" "w" "e" "i" "o")))


(require 'flycheck)
(global-flycheck-mode)


(use-package ag
  :ensure t
  :init
  (setq ag-highlight-search 1))


(use-package helm-ag
  :ensure t
  :bind
  ("C-c g p" . helm-do-ag-project-root)
  ("C-c g f" . helm-do-ag-this-file)
  ("C-c g b" . helm-do-ag-buffers)
  :config
  (global-set-key (kbd "C-c g d") '(lambda ()
   (interactive)
   (setq current-prefix-arg '(4))
   (helm-ag))))


(use-package ace-jump-mode
  :ensure t
  :bind
  ("C-c SPC" . ace-jump-mode))


(use-package rainbow-mode
  :ensure t)


(use-package syntax-subword
  :ensure t
  :config
  (global-syntax-subword-mode))


(use-package elpy
  :ensure t
  :init
  (elpy-enable))


(use-package magit
  :ensure t
  :bind
  ("C-." . magit-status)
  ("C-:" . magit-list-repositories)
  :init
  (setq magit-repository-directories `(("~/ml/" . 1)))
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (setq magit-repolist-columns
        '(("⬇"      1 magit-repolist-column-unpulled-from-upstream   ())
          ("⬆"      1 magit-repolist-column-unpushed-to-upstream     ())
          ("*"        1 magit-repolist-column-dirty                  ())
          ("Branch"  13 magit-repolist-column-branch                 ())
          ("Name"    31 magit-repolist-column-ident                  ()))))


(use-package yaml-mode
  :ensure t)


(use-package markdown-mode
  :ensure t
  :mode
  ("\\.md$" . markdown-mode)
  ("\\.markdown$" . markdown-mode)
  ("\\.js\\'" . js2-mode)
  ("\\.jsx\\'" . js2-jsx-mode))


(use-package multi-web-mode
  :ensure t
  :init
  (setq mweb-default-major-mode 'jinja2-mode)
  (setq mweb-tags '((js2-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("htm" "html"))
  :config
  (multi-web-global-mode 1))


(use-package js2-mode
  :ensure t
  :mode
  ("\\.js\\'" . js2-mode)
  ("\\.jsx\\'" . js2-jsx-mode)
  :config
  ;; Disable parse errors and strict warnings use flycheck. Highlight most ECMA built-ins
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-highlight-level 3))


(use-package wgrep
  :ensure t)


(use-package wgrep-ag
  :ensure t
  :init
  (defun wgrep-custom-bindings ()
    (local-set-key (kbd "C-x C-e") 'wgrep-change-to-wgrep-mode))
  :config
 (add-hook 'ag-mode-hook 'wgrep-custom-bindings))


(use-package smex
  :bind
  ("M-x" . smex)
  :config
  (smex-initialize))


(use-package ido-ubiquitous
  :ensure t
  :init
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-auto-merge-work-directories-length nil
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t
        ido-handle-duplicate-virtual-buffers 2
        ido-max-prospects 10)
  :config
  (ido-mode t)
  (ido-everywhere 1)
  (ido-ubiquitous t))


(use-package expand-region
  :ensure t
  :bind
  ("M-RET" . er/expand-region))


(use-package sass-mode
  :ensure t
  :mode
  ("\\.scss$" . sass-mode))


(use-package fill-column-indicator
  :ensure t
  :init
  (setq fci-rule-column 100
        fci-rule-color "#595959"
        fci-rule-width 1
        fci-rule-use-dashes t
        fci-dash-pattern 0.4)
  :config
  (add-hook 'python-mode-hook 'fci-mode)
  (add-hook 'js-mode-hook 'fci-mode))


;;;; Automodes


;; less css
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
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
(add-to-list 'auto-mode-alist '("\\.j2$" . jinja2-mode))
;; fish
(add-to-list 'auto-mode-alist '("\\.fish$" . conf-mode))


;;;; Custom functions

;; Kill the characters from the cursor to the beginning of line
;;(global-set-key [M-delete] 'backward-kill-line)
(defun backward-kill-line (arg)
  "Kill chars backward until start of line."
  (interactive "p")
  (kill-line 0))

;; Insert date into buffer at point
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %Y-%m-%d - %l:%M %p")))

;; Smart C-a from prelude
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; Join next line or join all lines in region
(defun smart-join-line ()
  "Join the current line with the line beneath it or all region lines."
  (interactive)
  (if (use-region-p)
      (save-excursion
	(let ((start-line (line-number-at-pos (region-beginning)))
	      (current-line (line-number-at-pos (region-end))))
	  (goto-char (region-end))
	  (while (> current-line start-line)
	    (join-line)
	    (setq current-line (line-number-at-pos)))))
    (delete-indentation 1)))

;; Add new line next
(defun newline-next-and-indent ()
  "Append a new line below, move the point to the innted point of the line."
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

;; Add new line prev
(defun newline-prev-and-indent ()
  "Insert a new line above, move the point to the innted point of the line."
  (interactive)
  (let ((oldpos (point)))
    (beginning-of-line)
    (newline-and-indent)
    (previous-line)
    (indent-for-tab-command)))


;; Jump to symbol definition (uses ido)
;; taken from prelude
(defun goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))
      (recenter)))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names (substring-no-properties name))
          (add-to-list 'name-and-pos (cons (substring-no-properties name) position))))))))


;;;;; key bindings

;; open init.el
(global-set-key (kbd "<f8>")
                (lambda()(interactive)(find-file "~/.emacs.d/init.el")))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
    'interactive)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Basic movement key bindings
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "C-{") 'backward-char)
(global-set-key (kbd "C-}") 'forward-char)
(global-set-key (kbd "M-{") 'backward-word)
(global-set-key (kbd "M-}") 'forward-word)

;; Register shortcuts (global-set-key (kbd "C-ñ m") 'point-to-register)
(global-set-key (kbd "C-ñ j") 'jump-to-register)

;; Basic deleting key bindings
(global-set-key (kbd "C-S-k") 'kill-whole-line)

;; Buffers related key bindings
(global-set-key (kbd "C-M-ñ") 'ido-switch-buffer)

(global-set-key (kbd "C-ñ C-j") 'next-buffer)
(global-set-key (kbd "C-ñ j") 'next-buffer)

(global-set-key (kbd "C-ñ C-k") 'previous-buffer)
(global-set-key (kbd "C-ñ k") 'previous-buffer)

;; hippie-expand
(global-set-key (kbd "M-SPC") (make-hippie-expand-function
                               '(try-expand-dabbrev-visible
                                 try-expand-dabbrev
                                 try-expand-dabbrev-all-buffers) t))
(global-set-key (kbd "C-M-SPC") (make-hippie-expand-function
                               '(try-expand-line
				 try-expand-line-all-buffers
				 try-complete-file-name-partially
				 try-complete-file-name) t))

;; html-mode
(defun html-mode-keys ()
  "Modify keymaps used by `html-mode'."
  (local-set-key (kbd "C-c -") 'sgml-close-tag))
(add-hook 'jinja2-mode-hook 'html-mode-keys)


;;;; Local packages bindings

;;;; Custom functions bindings

;; Smart C-a
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)
;; Smart line join
(global-set-key (kbd "C-S-j") 'smart-join-line)
;; Newline next/prev and indent
(global-set-key (kbd "<C-return>") 'newline-next-and-indent)
(global-set-key (kbd "<C-S-return>") 'newline-prev-and-indent)
;; Go to symbol
(global-set-key (kbd "M-j") 'goto-symbol)


(use-package json-mode
  :ensure t
  :mode (("\\.json$" . json-mode)
         ("\\.eslintrc$" . json-mode)))


(use-package helm
  :ensure t)


(use-package helm-swoop
  :ensure t
  :init
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)
  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)
  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-vertically)
  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color nil)
  ;; ;; Go to the opposite side of line from the end or beginning of line
  (setq helm-swoop-move-to-line-cycle t)
  ;; Optional face for line numbers
  ;; Face name is `helm-swoop-line-number-face`
  (setq helm-swoop-use-line-number-face nil)
  ;; If you prefer fuzzy matching
  (setq helm-swoop-use-fuzzy-match t)
  :config
  ;; Change the keybinds to whatever you like :)
  (global-set-key (kbd "M-i") 'helm-swoop)
  (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
  (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
  (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; When doing evil-search, hand the word over to helm-swoop
  ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

  ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
  (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

  ;; Move up and down like isearch
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line))


;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
