;; Themes
(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory
                     (convert-standard-filename "themes")))
(load-theme 'gmonokai t)
(setq custom-safe-themes
      (quote
       ("27c0599626f0a132bbdc06c55e8cd20693d2ca4f07e386a81dad86d57b9e3c64"
        default)))


;;;; Settings

(defvar my-paths '("/usr/local/opt/coreutils/libexec/gnubin"
		   "/usr/local/bin"
		   "/usr/bin"
		   "/bin"
		   "/usr/sbin"
		   "/sbin"
		   "/Users/gonz/Bin"
		   "/Users/gonz/node_modules/.bin"))
(defvar my-emacs-bin-directory
  (convert-standard-filename (concat user-emacs-directory "bin/")))

;; Manually set PATH env var and exec-path
(setenv "PATH" (mapconcat 'identity my-paths ":") )
(setq exec-path (append my-paths (list "." exec-directory)))

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

;; Highlight matching parentheses, or whole expr if not visible.
(show-paren-mode 1)
(setq show-paren-style 'mixed)

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
(setq org-default-notes-file "~/notes.org")
(define-key global-map "\C-cc" 'org-capture)

;; help-at-pt
(setq help-at-pt-timer-delay 0.3)
(help-at-pt-cancel-timer)
(help-at-pt-set-timer)
(setq help-at-pt-display-when-idle '(flymake-overlay))

;; server
(require 'server)
;;Start emacs server in running GUI and not already running
(setq server-socket-dir "/tmp/emacs-shared")
(if (display-graphic-p)
    (unless (server-running-p)
      (server-start)))

;; uniquify
(require 'uniquify)
;; include part of the file directory name at the beginning of the buffer name
(setq uniquify-buffer-name-style 'forward)

;; flymake
;; Automatically enable flymake-mode upon opening any file for which
;; syntax check is possible
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Custom flymake syntax checkers
(when (load "flymake" t)
  ;; Disable flymake for html files
  (delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)

  ;; python syntax check
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
  		       'flymake-create-temp-inplace))
  	   (local-file (file-relative-name
  			temp-file
  			(file-name-directory buffer-file-name)))
  	   (pycheck-bin (concat my-emacs-bin-directory
  				"pycheckers")))
      (list pycheck-bin (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
  	       '("\\.py\\'" flymake-pyflakes-init))

  ;; javascript jslint
  (defun flymake-jslint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
  		       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name)))
  	   (jscheck-bin (concat my-emacs-bin-directory
  				"jschecker")))
      (list jscheck-bin (list local-file))))

  (setq flymake-err-line-patterns
  	(cons '("^\\(.*\\)(\\([[:digit:]]+\\)):\\(.*\\)$"
  		1 2 nil 3)
  	      flymake-err-line-patterns))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.js\\'" flymake-jslint-init))
)

;; Setup local info dir
(eval-after-load "info"
  '(progn (info-initialize)
	  (push (concat user-emacs-directory
			(convert-standard-filename "info/"))
		Info-directory-list)))

;; Fix python info-lookup-symbol
(require 'info-look)
(info-lookup-add-help
 :mode 'python-mode
 :regexp "[[:alnum:]_]+"
 :doc-spec
 '(("(python)Index" nil "")))


;;;; Remote Packages

(defvar my-packages '(fsharp-mode
		      markdown-mode
		      rainbow-mode
		      ace-jump-mode
		      expand-region
		      magit
		      ido-ubiquitous
		      haml-mode
		      sass-mode
		      yaml-mode
		      smex
                      wgrep
                      wgrep-ag
		      fill-column-indicator))

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Check if packages in my-packages are installed; if not, install.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 my-packages)


;;;; Local Packages

(let ((default-directory
        (concat user-emacs-directory
                (convert-standard-filename "modules"))))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))
(require 'syntax-subword)
(require 'php-mode)
(require 'ag)
(require 'switch-window)
(require 'powerline)


;;;; Non-builtin packages settings

;; wgrep-ag
(require 'wgrep-ag)

;; ag
(setq ag-highlight-search 1)

;; switch-window
(setq switch-window-shortcut-style 'qwerty)
(setq switch-window-qwerty-shortcuts
      '("a" "s" "d" "f" "j" "k" "l" "ñ" "w" "e" "i" "o"))

;; smex
(smex-initialize)

;; powerline
(powerline-default-theme)

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

;; fci-mode (Fill column indicator)
(setq fci-rule-column 80
      fci-rule-color "#595959"
      fci-rule-width 1
      fci-rule-use-dashes t
      fci-dash-pattern 0.4)
(add-hook 'python-mode-hook 'fci-mode)

;; syntax-subword
(global-syntax-subword-mode)


;;;; Automodes

;; sass-mode
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
;; Markdown
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

;; Basic movement key bindings
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "C-{") 'backward-char)
(global-set-key (kbd "C-}") 'forward-char)
(global-set-key (kbd "M-{") 'backward-word)
(global-set-key (kbd "M-}") 'forward-word)

;; Basic deleting key bindings
(global-set-key (kbd "C-S-k") 'kill-whole-line)

;; Buffers related key bindings
(global-set-key (kbd "C-M-ñ") 'switch-window)
(global-set-key (kbd "C-ñ C-ñ") 'ido-switch-buffer)
(global-set-key (kbd "C-ñ C-}") 'next-buffer)
(global-set-key (kbd "C-ñ C-{") 'previous-buffer)

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

;;;; Local packages bindings

;; magit
(global-set-key (kbd "C-.") 'magit-status)

;; smex
(global-set-key (kbd "M-x") 'smex)


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
