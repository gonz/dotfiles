;;; gmonokai-theme.el --- REQUIRES EMACS 24: Gmonokai Color Theme for Emacs.

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

;; Forked from Lorenzo's monokai 0.0.10 version.
;; Original copyright notice:
;; Copyright (C) 2012 Lorenzo Villani.
;; Author: Lorenzo Villani <lorenzo@villani.me>
;; URL: https://github.com/lvillani/el-monokai-theme

;;(unless (>= 24 emacs-major-version)
;;  (error "gmonokai-theme requires Emacs 24 or later."))

(deftheme gmonokai
  "Gmonokai color theme")

(let ((monokai-black         "#000000")
      (monokai-almost-black  "#141412")
      (monokai-gray-darkest  "#1b1c18")
      (monokai-gray-dark     "#20211c")
      (monokai-gray-darker   "#383830")
      (monokai-gray          "#454545")
      (monokai-gray-lightest "#595959")
      (monokai-gray-light    "#E6E6E6")
      (monokai-blue-light    "#89BDFF")
      (monokai-green         "#A6E22A")
      (monokai-red-light     "#fc6060")
      (monokai-red           "#fc1c1c")
      (monokai-magenta       "#F92672")
      (monokai-purple-light  "#FD5FF1")
      (monokai-purple        "#AE81FF")
      (monokai-yellow-light  "#F8F8F2")
      (monokai-yellow        "#E6DB74")
      (monokai-yellow-dark   "#75715E"))
  (custom-theme-set-faces
   'gmonokai

   ;; Frame
   `(default
     ((t (:foreground ,monokai-yellow-light :background ,monokai-gray-darkest))))
   `(cursor
     ((t (:foreground ,monokai-magenta))))
   `(hl-line
     ((t (:background ,monokai-almost-black))))
   `(minibuffer-prompt
     ((t (:foreground ,monokai-yellow-dark))))
   `(modeline
     ((t (:background ,monokai-gray-lightest :foreground ,monokai-gray-light))))
   `(region
     ((t (:background ,monokai-gray-darker))))
   `(show-paren-match-face
     ((t (:background ,monokai-gray))))

   ;; Main
   `(font-lock-builtin-face
     ((t (:foreground ,monokai-green))))
   `(font-lock-comment-face
     ((t (:foreground ,monokai-yellow-dark))))
   `(font-lock-constant-face
     ((t (:foreground ,monokai-purple))))
   `(font-lock-doc-string-face
     ((t (:foreground ,monokai-yellow))))
   `(font-lock-function-name-face
     ((t (:foreground ,monokai-green))))
   `(font-lock-keyword-face
     ((t (:foreground ,monokai-magenta))))
   `(font-lock-string-face
     ((t (:foreground ,monokai-yellow))))
   `(font-lock-type-face
     ((t (:foreground ,monokai-blue-light))))
   `(font-lock-variable-name-face
     ((t (:foreground ,monokai-magenta))))
   `(font-lock-warning-face
     ((t (:bold t :foreground ,monokai-purple-light))))

   ;; CUA
   `(cua-rectangle
     ((t (:background ,monokai-gray-darker))))

   ;; IDO
   `(ido-first-match
     ((t (:foreground ,monokai-purple))))
   `(ido-only-match
     ((t (:foreground ,monokai-green))))
   `(ido-subdir
     ((t (:foreground ,monokai-blue-light))))

   ;; Whitespace
   `(whitespace-space
     ((t (:foreground ,monokai-gray-lightest))))

   ;; Magit
   `(magit-item-highlight
     ((t (:background ,monokai-almost-black))))
   `(magit-section-title
     ((t (:foreground ,monokai-purple :background ,monokai-gray-darkest))))
   `(magit-branch
     ((t (:foreground ,monokai-red :background ,monokai-gray-darkest))))
   `(magit-diff-file-header
     ((t (:foreground ,monokai-yellow :background ,monokai-gray-darkest))))
   `(magit-diff-add
     ((t (:foreground ,monokai-green :background ,monokai-gray-darkest))))
   `(magit-diff-del
     ((t (:foreground ,monokai-red-light :background ,monokai-gray-darkest))))
   `(magit-diff-hunk-header
     ((t (:foreground ,monokai-blue-light :background ,monokai-gray-darkest))))

   ;; Flymake
   `(flymake-errline
     ((t (:underline (:color ,monokai-red :style wave)))))

   ;; Yasnippet
   `(yas/field-highlight-face
     ((t (:background ,monokai-gray-darker))))

   ;; Hl tags
   `(hl-tags-face
     ((t (:background ,monokai-black))))
   ))


;;;###autoload
;; (when load-file-name
;;   (add-to-list 'custom-theme-load-path
;;                (file-name-as-directory (file-name-directory load-file-name)))
;;   (when (not window-system)
;;     (custom-set-faces '(default ((t (:background "#1c1c1c")))))))

(provide-theme 'gmonokai)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; gmonokai-theme.el ends here
