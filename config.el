;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;

;;;###autoload
(defun my/edit-zsh-configuration ()
  (interactive)
  (find-file "~/.zshrc"))

;;;###autoload
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

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


(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t
        )
  (visual-fill-column-mode 1)
  )

(use-package! visual-fill
  :hook (org-mode . my/org-mode-visual-fill))

(delete-selection-mode t)

(setq
 projectile-project-search-path '("~/Documents/Work/metaloka" "~/Documents/Work/Personal" "~/Documents/Work/Bytesoft" "~/Documents/Work/bytenext")
 doom-font (font-spec :family "Ubuntu Mono" :size 18 :weight 'light)
 doom-unicode-font (font-spec :family "Ubuntu Mono" :weight 'light)
 doom-variable-pitch-font (font-spec :family "Ubuntu" :size 16)
 js-indent-level 2
 typescript-indent-level 2
 json-reformat:indent-width 2

 ;; lsp
 ;; lsp-ui-sideline-enable nil
 lsp-ui-doc-enable nil
 +lsp-prompt-to-install-server 'quiet
 )

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

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
 :leader "= ="        #'prettier-prettify

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

(use-package! lsp-mode
  :commands lsp
  :config
  (setq lsp-idle-delay 0.2
        lsp-enable-file-watchers nil
        lsp-completion-provider t
        )
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)

  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  )

(use-package! lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-headerline-breadcrumb-enable t ;
        lsp-lens-enable t                  ;
        )
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ([remap xref-pop-marker-stack] . lsp-ui-peek-jump-backward)
         )
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)  )

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
(add-hook! js2-mode 'lsp!)



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
 `(web-mode-html-tag-face ((t (:foreground,(doom-color 'magenta)), :weight 'normal)))
 )

(use-package! dotenv-mode
  :mode ("\\.env\\.?.*\\'" . dotenv-mode))


(use-package engine-mode
  :config
  (engine/set-keymap-prefix (kbd "C-c s"))
  (setq browse-url-browser-function 'browse-url-default-browser
        engine/browser-function 'browse-url-default-browser
        ;; browse-url-generic-program "google-chrome"
        )
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "sh")

  (defengine gitlab
    "https://gitlab.com/search?search=%s&group_id=&project_id=&snippets=false&repository_ref=&nav_source=navbar"
    :keybinding "sl")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "o")

  (defengine npm
    "https://www.npmjs.com/search?q=%s"
    :keybinding "n")

  (defengine crates
    "https://crates.io/search?q=%s"
    :keybinding "c")

  (defengine localhost
    "http://localhost:%s"
    :keybinding "l")

  (defengine translate
    "https://translate.google.com/?sl=en&tl=vi&text=%s&op=translate"
    :keybinding "t")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")

  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  (engine-mode 1))

(use-package! flycheck
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-hook 'flycheck-mode-hook 'my/use-eslint-from-node-modules))

(after! leetcode
  (setq leetcode-prefer-language "rust"
        leetcode-prefer-sql "mysql"
        leetcode-save-solutions t
        leetcode-directory "~/github/nghiatd/go-leetcode")
  )

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)


(use-package! tree-sitter
  :config

  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))

  ;; This makes every node a link to a section of code
  (setq tree-sitter-debug-jump-buttons t
        ;; and this highlights the entire sub tree in your code
        tree-sitter-debug-highlight-jump-region t))

(after! prog-mode
  (map! :map prog-mode-map "C-h C-f" #'find-function-at-point)
  (map! :map prog-mode-map
        :localleader
        :desc "Find function at point"
        "g p" #'find-function-at-point))
