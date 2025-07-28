;;; post-init.el --- after init -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package compile-angel
  :ensure t
  :demand t

  :custom
  (compile-angel-verbose nil)

  :config
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/post-early-init.el" compile-angel-excluded-files)
  ;; A local mode that compiles .el files whenever the user saves them.
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)
  ;; A global mode that compiles .el files before they are loaded.
  (compile-angel-on-load-mode))

(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-hide-results t)
  (auto-package-update-delete-old-versions t)
  ;; (auto-package-update-prompt-before-update t)

  :config
  (auto-package-update-maybe))

(use-package evil
  :ensure t
  :commands (evil-mode evil-define-key)
  :hook (after-init . evil-mode)

  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-disable-insert-state-bindings t)

  :custom
  (evil-echo-state nil)
  (evil-undo-system 'undo-redo)
  (evil-want-fine-undo t)
  (evil-ex-visual-char-range t)
  (evil-ex-search-vim-style-regexp t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-respect-visual-line-mode t)
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-move-beyond-eol t)
  (evil-v$-excludes-newline t))

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :init
;;   (setq evil-collection-setup-minibuffer t)
;;   :config
;;   (evil-collection-init))

(use-package evil-escape
  :ensure t
  :init (evil-escape-mode)
  :config
  (setq-default evil-escape-delay 0.3)
  (setq-default evil-escape-key-sequence "kj"))

(use-package evil-surround
  :after evil
  :ensure t
  :commands global-evil-surround-mode
  :hook (after-init . global-evil-surround-mode))

(with-eval-after-load "evil"
  (evil-define-operator my-evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment))

(use-package corfu
  :ensure t
  :commands (corfu-mode global-corfu-mode)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  :config
  (global-corfu-mode))

(use-package cape
  :ensure t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  :ensure t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (setq consult-async-input-debounce 0.02
        consult-async-input-throttle 0.05
        consult-async-refresh-delay 0.02)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

;; (use-package eglot
;;  :ensure nil
;;  :commands (eglot-ensure
;;             eglot-rename
;;             eglot-format-buffer))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package org
  :ensure t
  :commands (org-mode org-version)
  :mode ("\\.org\\'" . org-mode)

  :custom
  (org-directory "~/notes/")
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-fontify-done-headline t)
  (org-fontify-todo-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-startup-truncated t))

(use-package org-roam
  :ensure t
  :init (setq org-roam-v2-ack t)
  :custom (org-roam-directory "~/notes/roam/")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config (org-roam-setup))

(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode))

(use-package paredit
  :ensure t
  :commands paredit-mode
  :hook
  (emacs-lisp-mode . paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "RET") nil))

(use-package enhanced-evil-paredit
  :ensure t
  :commands enhanced-evil-paredit-mode
  :hook
  (paredit-mode . enhanced-evil-paredit-mode))

(use-package which-key
  :ensure nil ; builtin
  :commands which-key-mode
  :hook (after-init . which-key-mode)

  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.1)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

;; dired: Group directories first
(with-eval-after-load 'dired
  (let ((args "--group-directories-first -ahlv"))
    (when (or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
      (if-let* ((gls (executable-find "gls")))
          (setq insert-directory-program gls)
        (setq args nil)))
    (when args
      (setq dired-listing-switches args))))

(use-package doom-themes
  :ensure t

  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)

  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(mapc #'disable-theme custom-enabled-themes) 
(load-theme 'doom-one t)

(set-face-attribute 'default nil
                    :height 110 :weight 'normal :family "JetBrains Mono")

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(savehist-mode 1)
(recentf-mode 1)
(save-place-mode 1)

(setq-default tab-width 4)
(indent-tabs-mode nil)

(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

(setq-default display-line-numbers-type 'relative)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

(electric-pair-mode t)
(add-hook 'after-init-hook #'show-paren-mode)

(defun zen-mode--apply-margins (&optional window)
  "Calculate and apply centered margins to WINDOW."
  (interactive)
  (let* ((win (or window (selected-window)))
         (total-width (window-total-width win))
         (desired-width 126)
         (total-margin-width (- total-width desired-width))
         (margin-width-left (max 0 (floor total-margin-width 2)))
         (margin-width-right (max 0 (- (- total-width desired-width) margin-width-left))))
    (set-window-margins win margin-width-left margin-width-right)))

(define-minor-mode zen-mode
  "A minor mode to make text centered with a reasonable width."
  :lighter " Zen" ; Text to display in the mode line when active
  :keymap nil    ; No specific keymap for this mode, can be added later if needed
  :global nil    ; This is a buffer-local minor mode
  (cond
   (zen-mode
    (zen-mode--apply-margins)
    (add-hook 'window-size-change-functions #'zen-mode--apply-margins nil 'local))
   (t
    (remove-hook 'window-size-change-functions #'zen-mode--apply-margins 'local)
    (set-window-margins (selected-window) 0 0)
    (setq-local zen-mode--original-margins nil))))

(provide 'zen-mode)

(defun enable-zen-mode-on-new-buffer ()
  "Enables a specific minor mode if it's a new buffer."
  (unless (minibufferp)
    (zen-mode 1)))
(add-hook 'after-change-major-mode-hook #'enable-zen-mode-on-new-buffer)

