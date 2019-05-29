;;; acme-theme.el --- An Acme-esque Emacs theme -*- lexical-binding:t; -*-

;; Copyright (C) 2019 Noodles!

;; Author: Noodles! <nnoodle@chiru.no>
;; Version: 0.2.0
;; Keywords: themes
;; URL: https://github.com/nnoodle/emacs-acme-theme

;; This file is not part of GNU Emacs.

;; MIT License

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; An Emacs theme that tries to be similar to the Acme (and Plan 9) motif.

;;

;;; TODO:
;; - incorperate acme-style modified buffer indicators
;; - figure out whether or not link highlighting is a good thing

;;; Code:

(deftheme acme
  "A theme similar to the one used in the Acme editor.")

(defgroup acme-theme nil
  "The Acme theme"
  :group 'faces
  :prefix "acme-theme-")

(defcustom acme-theme-more-syntax-hl nil
  "Non-nil enables just a bit more syntax highlighting.

\"Syntax highlighting is juvenile. When I was a child, I was taught
arithmetic using colored rods
\(http://en.wikipedia.org/wiki/Cuisenaire_rods). I grew up and today I
use monochromatic numerals.\"

- Rob \"monochromatic is not problematic\" Pike
https://groups.google.com/d/msg/golang-nuts/hJHCAaiL0so/kG3BHV6QFfIJ"
  :type 'boolean
  :group 'acme-theme)

(defcustom acme-theme-gray-rainbow-delimiters nil
  "Non-nil makes `rainbow-delimiters' gray."
  :type 'boolean
  :group 'rainbow-delimiters
  :group 'acme-theme)

(defcustom acme-theme-white-backgrounds t
  "Non-nil hooks `acme-theme-white-background' to certain modes."
  :type 'boolean
  :group 'acme-theme)

(defcustom acme-theme-change-defaults t
  "Non-nil changes some defaults to be more like Acme."
  :type 'boolean
  :group 'acme-theme)

(defvar acme-theme-colors-alist
  '((black       . "#000000") ; foreground
    (gray-dark   . "#888888")
    (gray        . "#cccccc")
    (gray-pale   . "#eeeeee")
    (white       . "#ffffff") ; empty column
    (frost-deep  . "#8888cc") ; corner
    (frost-dark  . "#55aaaa") ; tagbar highlight
    (frost       . "#9eeeee")
    (frost-pale  . "#eaffff") ; tagbar
    (yellow-dark . "#99994c") ; scrollbar
    (yellow      . "#eeee9e") ; main highlight
    (yellow-semi . "#ffffcc")
    (yellow-pale . "#ffffea") ; background
    (red-deep    . "#aa0000") ; mouse-2 highlight
    (red-dark    . "#bb5d5d")
    (red         . "#ffaaaa")
    (red-pale    . "#ffeaea")
    (green-deep  . "#006600") ; mouse-3 highlight
    (green-dark  . "#448844")
    (green       . "#88cc88")
    (green-pale  . "#eaffea")
    (blue-deep   . "#000099") ; tagbox
    (blue-dark   . "#0088cc")
    (blue        . "#00aaff")
    (blue-pale   . "#c0eaff"))
  "Colors from Acme & Plan9.")

(defmacro acme-theme-with-color-variables (&rest body)
  "Bind all colors defined in `acme-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons) (list (car cons) (cdr cons)))
                   acme-theme-colors-alist))
     ,@body))

(defun acme-theme-alternative (face &optional alt)
  "Return FACE if `acme-theme-more-syntax-hl' is t, otherwise return default or ALT."
  (if acme-theme-more-syntax-hl face
    (or alt '((t nil)))))

(defun acme-theme-rainbow-delimiters (color)
  "Return rainbow-delimiters face with COLOR or gray."
  `((t (:inherit rainbow-delimiters-base-face :foreground
                 ,(acme-theme-with-color-variables
                   (if acme-theme-gray-rainbow-delimiters
                       gray-dark color))))))

(defun acme-theme-white-background (&optional bg)
  "Use a white background if BG is nil, otherwise use BG."
  (interactive)
  (when (or (and acme-theme-white-backgrounds (memq 'acme custom-enabled-themes))
            (called-interactively-p))
    (acme-theme-with-color-variables
      (let ((bg (or bg white)))
        (face-remap-add-relative 'default :background bg)
        (face-remap-add-relative 'highlight :background gray-pale)
        (face-remap-add-relative 'region :background gray)
        (face-remap-add-relative 'fringe :foreground gray-dark :background bg)
        (face-remap-add-relative 'scroll-bar :foreground bg :background gray-dark)))))

(acme-theme-with-color-variables
  ;; basic faces additional to faces.el
  (defface acme-theme-note
    `((t (:underline (:color ,green :style wave))))
    "Face for linter notes."
    :group 'acme-theme)

  (defface acme-theme-warning
    `((t (:underline (:color ,blue :style wave))))
    "Face for linter warnings."
    :group 'acme-theme)

  (defface acme-theme-error
    `((t (:underline (:color ,red :style wave))))
    "Face for linter errors."
    :group 'acme-theme)

  (custom-theme-set-faces
   'acme
   ;; basic
   `(default ((t (:foreground ,black :background ,yellow-pale))))
   `(success ((t (:foreground ,green))))
   `(warning ((t (:foreground ,red-dark))))
   `(error ((t (:foreground ,red-deep))))
   `(homoglyph ,(acme-theme-alternative `((t :foreground ,frost))))
   `(escape-glyph ,(acme-theme-alternative `((t :foreground ,frost))))
   `(link ((t (:foreground ,black :underline t))))
   `(link-visited ((t (:inherit link :slant italic))))
   `(highlight ((t (:background ,yellow-semi))))
   `(region ((t (:background ,yellow))))
   `(cursor ((t (:background ,black))))
   `(fringe ((t (:foreground ,yellow-dark :background ,yellow-pale))))
   `(scroll-bar ((t (:foreground ,yellow-pale :background ,yellow-dark))))
   `(hl-line ((t (:inherit highlight))))
   `(vertical-border ((t (:background ,black))))
   `(secondary-selection ((t (:background ,red-deep :foreground ,white))))
   `(lazy-highlight ((t (:background ,green-dark :foreground ,white))))
   `(query-replace ((t (:inherit region))))
   `(shadow ,(acme-theme-alternative `((t (:foreground ,gray-dark)))))
   `(minibuffer-prompt ((t nil)))
   `(tooltip ((t (:foreground ,black :background ,green-pale))))

   ;; window-divider
   `(window-divider ((t (:inherit default))))
   `(window-divider-first-pixel ((t (:inherit window-divider))))
   `(window-divider-last-pixel ((t (:inherit window-divider))))

   ;; line numbers
   `(line-number ((t (:inherit fringe))))
   `(line-number-current-line ((t (:inherit scroll-bar))))

   ;; mode-line
   `(mode-line ((t (:foreground ,black :background ,frost-pale
                                :box (:color ,frost-deep)))))
   `(mode-line-inactive ((t (:inherit mode-line))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-buffer-id ((t nil)))
   `(mode-line-highlight ((t nil)))
   `(compilation-mode-line-exit ((t (:weight semi-bold :inherit compilation-info))))
   `(compilation-mode-line-fail ((t (:weight semi-bold :inherit compilation-error))))

   ;; header-line
   `(header-line ((t (:inherit mode-line))))
   `(header-line-highlight ((t (:inherit mode-line))))

   ;; syntax highlighting :^)
   `(font-lock-builtin-face ((t nil)))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-comment-face ,(acme-theme-alternative `((t (:foreground ,gray-dark)))))
   `(font-lock-constant-face ((t nil)))
   `(font-lock-doc-face ((t (:inherit font-lock-string-face))))
   `(font-lock-function-name-face ((t nil)))
   `(font-lock-keyword-face ((t nil)))
   `(font-lock-negation-char-face ((t nil)))
   `(font-lock-preprocessor-face ((t nil)))
   `(font-lock-regexp-grouping-backslash ((t nil)))
   `(font-lock-regexp-grouping-construct ((t nil)))
   `(font-lock-string-face ,(acme-theme-alternative `((t (:foreground ,green-dark)))))
   `(font-lock-type-face ((t nil)))
   `(font-lock-variable-name-face ((t nil)))
   `(font-lock-warning-face ((t nil)))

   ;; isearch
   `(isearch ((t (:inherit region))))
   `(isearch-fail ((t (:background ,red))))

   ;; ido
   `(ido-only-match ((t (:weight bold))))
   `(ido-subdir ((t nil)))
   `(ido-virtual ((t (:slant italic))))

   ;; ivy/counsel
   `(ivy-confirm-face ((t (:inherit minibuffer-prompt :foreground ,green))))
   `(ivy-current-match ((t (:inherit region))))
   `(ivy-grep-info ((t nil)))
   `(ivy-highlight-face ((t (:inherit font-lock-builtin-face))))
   `(ivy-match-required-face ((t (:inherit minibuffer-prompt :foreground ,red-dark))))
   `(ivy-minibuffer-match-face-1 ((t (:inherit highlight))))
   `(ivy-minibuffer-match-face-2 ((t (:inherit ivy-minibuffer-match-face-1 :background ,yellow :weight semi-bold))))
   `(ivy-minibuffer-match-face-3 ((t (:inherit ivy-minibuffer-match-face-2))))
   `(ivy-minibuffer-match-face-4 ((t (:inherit ivy-minibuffer-match-face-3))))
   `(ivy-minibuffer-match-highlight ((t (:inherit region))))
   `(ivy-modified-buffer ((t nil)))
   `(ivy-remote ((t nil)))
   `(ivy-virtual ((t (:inherit font-lock-builtin-face :slant italic))))
   `(counsel-key-binding ((t nil)))

   ;; swiper
   `(swiper-line-face ((t (:inherit highlight))))
   `(swiper-match-face-1 ((t (:inherit ivy-minibuffer-match-face-1))))
   `(swiper-match-face-2 ((t (:inherit ivy-minibuffer-match-face-2))))
   `(swiper-match-face-3 ((t (:inherit ivy-minibuffer-match-face-3))))
   `(swiper-match-face-4 ((t (:inherit ivy-minibuffer-match-face-4))))

   ;; company-mode
   `(company-preview ((t (:foreground ,gray-dark))))
   `(company-preview-common ((t (:foreground ,gray-dark))))
   `(company-scrollbar-bg ((t (:background ,green))))
   `(company-scrollbar-fg ((t (:background ,green-dark))))
   `(company-tooltip ((t (:foreground ,black :background ,green-pale))))
   `(company-tooltip-annotation ((t (:foreground ,blue-deep))))
   `(company-tooltip-common ((t (:weight bold))))
   `(company-tooltip-common-selection ((t (:inherit company-tooltip-common))))
   `(company-tooltip-mouse ((t (:inherit company-tooltip-selection))))
   `(company-tooltip-selection ((t (:background ,green-dark :foreground ,green-pale))))

   ;; show-paren
   `(show-paren-match ((t (:underline ,(if acme-theme-gray-rainbow-delimiters black t)))))
   `(show-paren-mismatch ((t (:underline (:color ,red-deep :style wave)))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ,(acme-theme-rainbow-delimiters black))
   `(rainbow-delimiters-depth-2-face ,(acme-theme-rainbow-delimiters gray-dark))
   `(rainbow-delimiters-depth-3-face ,(acme-theme-rainbow-delimiters blue-deep))
   `(rainbow-delimiters-depth-4-face ,(acme-theme-rainbow-delimiters blue-dark))
   `(rainbow-delimiters-depth-5-face ,(acme-theme-rainbow-delimiters green-deep))
   `(rainbow-delimiters-depth-6-face ,(acme-theme-rainbow-delimiters green-dark))
   `(rainbow-delimiters-depth-7-face ,(acme-theme-rainbow-delimiters red-deep))
   `(rainbow-delimiters-depth-8-face ,(acme-theme-rainbow-delimiters red-dark))
   `(rainbow-delimiters-unmatched-face ((t (:inherit (rainbow-delimiters-base-face show-paren-mismatch)))))

   ;; whitespace
   `(trailing-whitespace ((t (:background ,red-pale))))
   `(whitespace-newline ((t (:inherit whitespace-tab))))
   `(whitespace-space ((t (:inherit whitespace-tab))))
   `(whitespace-tab ((t (:foreground ,gray))))

   ;; dired
   `(dired-perm-write ((t nil)))

   ;; proced
   `(proced-sort-header ((t (:inherit mode-line :weight bold))))

   ;; info-mode
   `(info-header-node ((t (:slant italic))))
   `(info-index-match ((t (:inherit region))))
   `(info-menu-star ((t nil)))
   `(info-node ((t (:slant italic))))

   ;; eww
   `(eww-form-checkbox ((t (:inherit eww-form-submit))))
   `(eww-form-file ((t (:inherit eww-form-submit))))
   `(eww-form-select ((t (:inherit eww-form-submit))))
   `(eww-form-submit ((t (:foreground ,black :background ,frost-pale :box (:line-width 1 :style released-button)))))
   `(eww-form-text ((t (:inherit eww-form-submit))))
   `(eww-form-textarea ((t (:inherit eww-form-text))))
   `(eww-invalid-certificate ((t nil)))
   `(eww-valid-certificate ((t nil)))

   ;; erc
   `(erc-action-face ((t (:slant italic))))
   `(erc-current-nick-face ((t (:underline t))))
   `(erc-fool-face ((t (:inherit shadow :slant italic))))
   `(erc-header-line ((t (:inherit mode-line))))
   `(erc-input-face ((t (:inherit shadow))))
   `(erc-keyword-face ((t (:background ,green-pale :weight bold :underline t))))
   `(erc-my-nick-face ((t nil)))
   `(erc-nick-msg-face ((t (:slant italic))))
   `(erc-notice-face ((t (:weight semi-bold))))
   `(erc-pal-face ((t nil)))
   `(erc-prompt-face ((t (:weight bold))))
   `(erc-timestamp-face ((t (:weight semi-bold))))

   ;; message-mode
   `(message-cited-text ((t nil)))
   `(message-header-cc ((t nil)))
   `(message-header-name ((t (:weight semi-bold))))
   `(message-header-newsgroups ((t nil)))
   `(message-header-other ((t nil)))
   `(message-header-subject ((t nil)))
   `(message-header-to ((t nil)))
   `(message-header-xheader ((t nil)))
   `(message-mml ((t nil)))
   `(message-separator ((t nil)))

   ;; elfeed
   `(elfeed-log-debug-level-face ((t nil)))
   `(elfeed-log-error-level-face ((t nil)))
   `(elfeed-log-info-level-face ((t nil)))
   `(elfeed-log-warn-level-face ((t nil)))
   `(elfeed-search-date-face ((t nil)))
   `(elfeed-search-feed-face ((t nil)))
   `(elfeed-search-tag-face ((t nil)))
   `(elfeed-search-title-face ((t nil)))

   ;; notmuch
   `(notmuch-crypto-part-header ((t nil)))
   `(notmuch-crypto-signature-bad-key ((t (:weight semi-bold))))
   `(notmuch-crypto-signature-good ((t (:weight semi-bold))))
   `(notmuch-crypto-signature-good-key ((t (:weight semi-bold))))
   `(notmuch-crypto-signature-unknown ((t (:weight semi-bold))))
   `(notmuch-hello-logo-background ((t nil)))
   `(notmuch-message-summary-face ((t (:inherit mode-line))))
   `(notmuch-tag-added ((t (:underline ,black))))
   `(notmuch-tag-deleted ((t (:strike-through ,black))))
   `(notmuch-tag-face ((t nil)))
   `(notmuch-tag-flagged ((t (:weight bold))))
   `(notmuch-tag-unread ((t (:foreground ,red-deep :weight bold)))) ; too important

   ;; outline
   `(outline-level-1 ((t nil)))
   `(outline-level-2 ((t nil)))
   `(outline-level-3 ((t nil)))
   `(outline-level-4 ((t nil)))
   `(outline-level-5 ((t nil)))
   `(outline-level-6 ((t nil)))
   `(outline-level-7 ((t nil)))
   `(outline-level-8 ((t nil)))

   ;; org
   `(org-block ((t (:inherit org-code))))
   `(org-code ((t (:inherit (shadow fixed-pitch-serif)))))
   `(org-date ((t (:inherit fixed-pitch-serif))))
   `(org-document-info ((t (:inherit org-meta-line))))
   `(org-document-info-keyword ((t (:inherit org-document-info))))
   `(org-document-title ((t nil)))
   `(org-done ((t nil)))
   `(org-ellipsis ((t nil)))
   `(org-footnote ((t nil)))
   `(org-hide ((t (:foreground ,yellow-pale))))
   `(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   `(org-priority ((t (:inherit fixed-pitch))))
   `(org-table ((t (:inherit fixed-pitch))))
   `(org-tag ((t (:inherit fixed-pitch))))
   `(org-todo ((t nil)))

   ;; eshell
   `(eshell-ls-archive ((t nil)))
   `(eshell-ls-backup ((t nil)))
   `(eshell-ls-clutter ((t nil)))
   `(eshell-ls-directory ((t nil)))
   `(eshell-ls-executable ((t nil)))
   `(eshell-ls-missing ((t nil)))
   `(eshell-ls-product ((t nil)))
   `(eshell-ls-special ((t nil)))
   `(eshell-ls-symlink ((t nil)))
   `(eshell-ls-unreadable ((t nil)))
   `(eshell-prompt ((t nil)))

   ;; sh-mode
   `(sh-heredoc ((t (:inherit shadow))))
   `(sh-quoted-exec ((t nil)))

   ;; comint
   `(comint-highlight-input ((t nil)))

   ;; term
   `(term ((t ,(when acme-theme-white-backgrounds `(:background ,white)))))
   `(term-color-black ((t nil)))
   `(term-color-blue ((t nil)))
   `(term-color-cyan ((t nil)))
   `(term-color-green ((t nil)))
   `(term-color-magenta ((t nil)))
   `(term-color-red ((t nil)))
   `(term-color-white ((t nil)))
   `(term-color-yellow ((t nil)))

   ;; magit
   `(magit-diff-context-highlight ((t (:inherit magit-section-highlight))))
   `(magit-section-highlight ((t (:inherit highlight))))

   ;; git-gutter
   `(git-gutter:added ((t (:inherit fringe))))
   `(git-gutter:deleted ((t (:inherit fringe))))
   `(git-gutter:modified ((t (:inherit fringe))))

   ;; flymake
   `(flymake-note ((t (:inherit acme-theme-note))))
   `(flymake-warning ((t (:inherit acme-theme-warning))))
   `(flymake-error ((t (:inherit acme-theme-error))))

   ;; flycheck
   `(flycheck-info ((t (:inherit acme-theme-note))))
   `(flycheck-warning ((t (:inherit acme-theme-warning))))
   `(flycheck-error ((t (:inherit acme-theme-error))))

   ;; sly
   `(sly-mrepl-prompt-face ((t (:weight semi-bold))))
   `(sly-mrepl-output-face ((t nil)))
   `(sly-note-face ((t (:inherit acme-theme-note))))
   `(sly-style-warning-face ((t (:inherit acme-theme-note))))
   `(sly-warning-face ((t (:inherit acme-theme-warning))))
   `(sly-error-face ((t (:inherit acme-theme-error))))
   `(sly-mode-line ((t nil)))
   ))

(acme-theme-with-color-variables
  (custom-theme-set-variables
   'acme
   `(exwm-floating-border-color ,frost)
   `(rainbow-delimiters-max-face-count ,(if acme-theme-gray-rainbow-delimiters 1 8))
   `(ansi-color-names-vector
     ,(if acme-theme-more-syntax-hl
          [,black ,red ,green ,red-pale ,blue-dark ,frost-deep ,frost ,black]
        [,black ,black ,black ,black ,black ,black ,black ,black]))
   `(tooltip-frame-parameters
     (quote ((name . "tooltip")
             (internal-border-width . 2)
             (border-width . 2)
             (no-special-glyphs . t)
             (border-color . ,green))))
   `(pdf-view-midnight-colors
     (cons ,gray-pale ,black))
   `(pdf-annot-default-annotation-properties
     (quote ((t (label . ,user-full-name))
             (text (icon . "Note")
                   (color . ,black))
             (highlight (color . ,yellow))
             (squiggly (color . ,red-deep))
             (strike-out(color . ,black))
             (underline (color . ,red-deep))))))

  (when acme-theme-change-defaults
    ;; this is awkward to undo.
    ;; (setenv "PS1" "% ")
    ;; (setenv "PS2" "\t")
    (custom-theme-set-variables
     'acme
     ;; acme-like behavior
     ;; '(blink-cursor-mode 0)
     ;; '(cursor-type (quote bar))
     ;; '(scroll-bar-mode (quote left))
     '(delete-active-region 1)
     ;; on plan9port, the prompt appears as `% ', while on 9front,
     ;; this appears something like `term% '
     '(eshell-prompt-function
       (lambda () "% "))
     '(eshell-prompt-regexp "^% ")
     '(mouse-autoselect-window t)
     '(list-directory-brief-switches "-aCF")
     '(window-divider-default-right-width 2)
     '(window-divider-default-bottom-width 2)
     '(x-gtk-use-system-tooltips nil))))

(acme-theme-with-color-variables
  (when acme-theme-white-backgrounds
    ;; similar to empty space in acme
    (add-hook 'lisp-interaction-mode-hook #'acme-theme-white-background)
    ;; like right-click context menus
    (add-hook 'completion-list-mode-hook (lambda () (acme-theme-white-background green-pale)))
    (add-hook 'magit-popup-mode-hook (lambda () (acme-theme-white-background green-pale)))
    ;; kinda like the sam(1)
    (add-hook 'evil-command-window-mode-hook (lambda () (acme-theme-white-background frost-pale)))
    ;; like 9term(1)
    (add-hook 'shell-mode-hook #'acme-theme-white-background)
    (add-hook 'term-mode-hook #'acme-theme-white-background)
    ;; (add-hook 'eshell-mode-hook #'acme-theme-white-background) ; like win(1)
    ;; like page(1)
    (add-hook 'image-mode-hook (lambda () (acme-theme-white-background gray-dark)))
    (add-hook 'doc-view-mode-hook (lambda () (acme-theme-white-background gray-dark)))
    (add-hook 'pdf-view-mode-hook (lambda () (acme-theme-white-background gray-dark)))
    ;; like plot(1) / chart(1)
    (add-hook 'chart-mode-hook #'acme-theme-white-background)
    ;; like stats(1) (but without the fancy graphs)
    (add-hook 'proced-mode-hook #'acme-theme-white-background)
    ;; like gping(1)
    (add-hook 'net-utils-mode-hook #'acme-theme-white-background)
    ;; like mothra(1)
    (add-hook 'eww-mode-hook #'acme-theme-white-background)
    ;; looks like faces(1)
    (add-hook 'calendar-mode-hook (lambda () (acme-theme-white-background frost-pale)))
    (add-hook 'display-time-world-mode-hook (lambda () (acme-theme-white-background frost-pale)))
    ))

;;; patch `tooltip-show' to allow customizing border-color
(define-advice tooltip-show (:override (text &optional use-echo-area) dont-ignore)
  "Show a tooltip window displaying TEXT.

Text larger than `x-max-tooltip-size' is clipped.

If the alist in `tooltip-frame-parameters' includes `left' and `top'
parameters, they determine the x and y position where the tooltip
is displayed.  Otherwise, the tooltip pops at offsets specified by
`tooltip-x-offset' and `tooltip-y-offset' from the current mouse
position.

Optional second arg USE-ECHO-AREA non-nil means to show tooltip
in echo area."
  (if use-echo-area
      (tooltip-show-help-non-mode text)
    (condition-case error
        (let ((params (copy-sequence tooltip-frame-parameters))
              (fg (face-attribute 'tooltip :foreground))
              (bg (face-attribute 'tooltip :background)))
          (when (stringp fg)
            (setf (alist-get 'foreground-color params) (or (alist-get 'foreground-color params) fg))
            (setf (alist-get 'border-color params) (or (alist-get 'border-color params) fg)))
          (when (stringp bg)
            (setf (alist-get 'background-color params) (or (alist-get 'background-color params) bg)))
          (x-show-tip (propertize text 'face 'tooltip)
                      (selected-frame)
                      params
                      tooltip-hide-delay
                      tooltip-x-offset
                      tooltip-y-offset))
      (error
       (message "Error while displaying tooltip: %s" error)
       (sit-for 1)
       (message "%s" text)))))

(run-hooks 'acme-theme-hook)
(provide-theme 'acme)
;;; acme-theme.el ends here
