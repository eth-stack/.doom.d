;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;

;;;###autoload
(defun dqv/edit-zsh-configuration ()
  (interactive)
  (find-file "~/.zshrc"))

;;;###autoload
(defun dqv/use-eslint-from-node-modules ()
  "Set local eslint if available."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

;;;###autoload
(defun dqv/goto-match-paren (arg)
  "Go to the matching if on (){}[], similar to vi style of % ."
  (interactive "p")
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "NghiaTD"
      user-mail-address "toduynghia@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;; (setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic nil) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


(defun dqv/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t
        )
  (visual-fill-column-mode 1)
  )

(use-package! visual-fill
  :hook (org-mode . dqv/org-mode-visual-fill))

(delete-selection-mode t)

(setq
 projectile-project-search-path '("~/Documents/Work/metaloka" "~/Documents/Work/Personal" "~/Documents/Work/Bytesoft" "~/Documents/Work/bytenext")
 doom-font (font-spec :family "Ubuntu Mono derivative Powerline" :size 18 :weight 'light)
 doom-unicode-font (font-spec :family "Ubuntu Mono derivative Powerline" :weight 'light)
 doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
 js-indent-level 2
 typescript-indent-level 2
 json-reformat:indent-width 2

 ;; lsp
 lsp-ui-sideline-enable nil
 lsp-ui-doc-enable nil
 lsp-enable-symbol-highlighting nil
 +lsp-prompt-to-install-server 'quiet
 )

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-/") 'evilnc-comment-or-uncomment-lines)

(map!
 :leader "= ="        #'format-all-buffer

 :leader "["        #'hs-hide-block
 :leader "]"        #'hs-show-block
 )

(map!
 "M-k"          #'move-text-up
 "M-j"          #'move-text-down

 :nv "gr"           #'lsp-find-references
 :nv "gi"           #'lsp-find-implementation
 :nv "gy"           #'lsp-find-type-definition
 )

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

(eval-after-load 'web-mode
  '(progn
     (add-hook 'web-mode-hook #'add-node-modules-path)
     (add-hook 'web-mode-hook #'prettier-js-mode)))

(setq format-all-debug t)

(add-hook 'after-init-hook #'global-prettier-mode)
(add-hook 'after-init-hook #'global-tree-sitter-mode)

;; lsp format use prettier
(add-hook! 'after-init-hook
  (progn
    (setq-hook! 'typescript-mode-hook +format-with :nil)
    (add-hook! 'typescript-mode-hook 'prettier-mode)
    (setq-hook! 'rjsx-mode-hook +format-with :nil)
    (add-hook! 'rjsx-mode-hook 'prettier-mode)
    (setq-hook! 'js2-mode-hook +format-with :nil)
    (add-hook! 'js2-mode-hook 'prettier-mode)
    (setq-hook! 'typescript-tsx-mode-hook +format-with :nil)
    (add-hook! 'typescript-tsx-mode-hook 'prettier-mode)
    ))



(use-package! typescript-mode
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "typescript-tsx")
  (add-to-list 'auto-mode-alist (cons (rx ".tsx" string-end) #'typescript-tsx-mode))
  )

(add-hook! typescript-tsx-mode 'lsp!)




(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{3,6\\}"
     (0 (let ((colour (match-string-no-properties 0)))
          (if (or (= (length colour) 4)
                  (= (length colour) 7))
              (put-text-property
               (match-beginning 0)
               (match-end 0)
               'face (list :background (match-string-no-properties 0)
                           :foreground (if (>= (apply '+ (x-color-values
                                                          (match-string-no-properties 0)))
                                               (* (apply '+ (x-color-values "white")) .6))
                                           "black" ;; light bg, dark text
                                         "white" ;; dark bg, light text
                                         )))))
        append))))

(defun hexcolour-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil hexcolour-keywords t))


(add-hook! 'after-init-hook
  (progn
    (add-hook! 'typescript-mode-hook 'hexcolour-add-to-font-lock)
    (add-hook! 'typescript-tsx-mode-hook 'hexcolour-add-to-font-lock)
    (add-hook! 'css-mode-hook 'hexcolour-add-to-font-lock)
    (add-hook! 'elisp-byte-code-mode-hook 'hexcolour-add-to-font-lock)
    (add-hook! 'rjsx-mode-hook 'hexcolour-add-to-font-lock)
    (add-hook! 'js2-mode-hook 'hexcolour-add-to-font-lock)
    ))

(after! treemacs
  (setq
   evil-treemacs-state-cursor 'box
   treemacs-project-follow-cleanup t
   treemacs-width 25
   )
  (treemacs-follow-mode +1)
  )


(custom-set-faces
 `(font-lock-type-face ((t (:foreground ,(doom-color 'dark-cyan)))))
 )
