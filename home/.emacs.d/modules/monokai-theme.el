;;; monokai-theme.el --- REQUIRES EMACS 24: Monokai Color Theme for Emacs.

;; Copyright (C) 2013 Gonzalo Saavedra.

;; Author: Gonzalo Saavedra <talduken@gmail.com>
;; Version: 0.0.11

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3 of the License.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.

;; Forked from Lorenzo's 0.0.10 version.
;; Original copyright notice:

;; Copyright (C) 2012 Lorenzo Villani.
;;
;; Author: Lorenzo Villani <lorenzo@villani.me>
;; URL: https://github.com/lvillani/el-monokai-theme

(unless (>= 24 emacs-major-version)
  (error "monokai-theme requires Emacs 24 or later."))

(deftheme monokai
  "Monokai color theme")

(let ((monokai-blue-light "#89BDFF")
      (monokai-gray "#595959")
      (monokai-gray-darker "#383830")
      (monokai-gray-darkest "#141411")
      (monokai-gray-lightest "#595959")
      (monokai-gray-light "#E6E6E6")
      (monokai-green "#A6E22A")
      (monokai-green-light "#A6E22E")
      (monokai-grey-dark "#272822")
      (monokai-magenta "#F92672")
      (monokai-purple "#AE81FF")
      (monokai-purple-light "#FD5FF1")
      (monokai-yellow "#E6DB74")
      (monokai-yellow-dark "#75715E")
      (monokai-yellow-light "#F8F8F2"))
  (custom-theme-set-faces
   'monokai
   ;; Frame
   `(default ((t (:foreground ,monokai-yellow-light :background ,monokai-grey-dark))))
   `(cursor ((t (:foreground ,monokai-magenta))))
   `(hl-line ((t (:background ,monokai-gray-darkest))))
   `(minibuffer-prompt ((t (:foreground ,monokai-yellow-dark))))
   `(modeline ((t (:background ,monokai-gray-lightest :foreground ,monokai-gray-light))))
   `(region ((t (:background ,monokai-gray-darker))))
   `(show-paren-match-face ((t (:background ,monokai-gray-lightest))))
   ;; Main
   `(font-lock-builtin-face ((t (:foreground ,monokai-green))))
   `(font-lock-comment-face ((t (:foreground ,monokai-yellow-dark))))
   `(font-lock-constant-face ((t (:foreground ,monokai-purple))))
   `(font-lock-doc-string-face ((t (:foreground ,monokai-yellow))))
   `(font-lock-function-name-face ((t (:foreground ,monokai-green))))
   `(font-lock-keyword-face ((t (:foreground ,monokai-magenta))))
   `(font-lock-string-face ((t (:foreground ,monokai-yellow))))
   `(font-lock-type-face ((t (:foreground ,monokai-blue-light))))
   `(font-lock-variable-name-face ((t (:foreground ,monokai-magenta))))
   `(font-lock-warning-face ((t (:bold t :foreground ,monokai-purple-light))))
   ;; CUA
   `(cua-rectangle ((t (:background ,monokai-gray-darkest))))
   ;; IDO
   `(ido-first-match ((t (:foreground ,monokai-purple))))
   `(ido-only-match ((t (:foreground ,monokai-green))))
   `(ido-subdir ((t (:foreground ,monokai-blue-light))))
   ;; Whitespace
   `(whitespace-space ((t (:foreground ,monokai-gray))))
   ;; Magit
   `(magit-diff-add ((t (:foreground ,monokai-green :background ,monokai-grey-dark))))
   `(magit-diff-del ((t (:foreground ,"red3" :background ,monokai-grey-dark))))
   `(magit-item-highlight ((t (:background ,monokai-gray-darkest))))
   `(magit-diff-file-header ((t (:foreground ,monokai-yellow :background ,monokai-grey-dark))))
   `(magit-diff-hunk-header ((t (:foreground ,monokai-blue-light :background ,monokai-grey-dark))))
   ;; Yasnippet
   `(yas/field-highlight-face ((t (:background ,monokai-gray-darker))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name)))
  (when (not window-system)
    (custom-set-faces '(default ((t (:background "#1c1c1c")))))))

(provide-theme 'monokai)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; monokai-theme.el ends here
