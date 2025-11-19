;; Prerequisite

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa-devel" . "https://elpa.gnu.org/devel/"))
(package-initialize)

(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns x pgtk haiku))
	  (exec-path-from-shell-initialize)))

;; Appearance

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(set-frame-font "Maple Mono NF CN")
(set-face-attribute 'default nil :height 100)

(global-display-line-numbers-mode t)

(when (display-graphic-p) (global-hl-line-mode 1))

(use-package catppuccin-theme
  :ensure t
  :init (setq catppuccin-flavor 'frappe
	      catppuccin-italic-comments t
	      catppuccin-italic-variables t
	      catppuccin-italic-blockquotes t)
  :config (load-theme 'catppuccin :no-confirm))

(use-package nerd-icons-completion
  :ensure t
  :config (nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after (corfu)
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package treemacs-nerd-icons
  :ensure t
  :after (treemacs)
  :config
  (treemacs-nerd-icons-config))

(use-package marginalia
  :ensure t
  :custom (marginalia-mode 1))

(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'column))

(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode))

(use-package git-gutter-fringe
  :ensure t)

;; Editing

(electric-pair-mode 1)
(indent-tabs-mode -1)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  :init
  (setq corfu-auto t
	corfu-auto-delay 0
	corfu-auto-prefix 0
	corfu-quit-no-match t)
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package vertico
  :ensure t
  :custom
  (vertico-count 20)
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package savehist
  :init (savehist-mode))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-show-hidden-files nil)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-git-mode 'deferred)
  (global-set-key (kbd "M-0") 'treemacs-select-window))

(use-package vterm
  :ensure t)

(use-package magit
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode 1))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p)

  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Language features

(setq tab-width 4
      c-basic-offset 4)

(use-package eglot
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'eglot-ensure))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
	    :rev :newest
	    :branch "main")
  :config
  (add-hook 'prog-mode-hook #'(lambda () (copilot-mode 1)))
  (global-set-key (kbd "C-<tab>") 'copilot-accept-completion))

;; Lisp

(use-package sly
  :ensure t
  :init (setq inferior-lisp-program "ros -Q run"))

(use-package lisp-extra-font-lock
  :vc (:url "https://github.com/calsys456/lisp-extra-font-lock"
	    :rev :newest
	    :branch "main")
  :config (lisp-extra-font-lock-global-mode 1))

(use-package colourful
  :vc (:url "https://github.com/calsys456/colorful"
	    :rev :newest
	    :branch "main"))

;; Project configuration

(use-package projectile
  :ensure t
  :config (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map))

(defun cal/configure-cmake ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (compile "cmake -B build -G Ninja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=/usr")))

(defun cal/build-and-install-cmake ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (compile "cmake --build build && sudo cmake --install build")))

(defun cal/restart-ddm ()
  (interactive)
  (async-shell-command "sudo systemctl restart ddm.service"))

;; Internet

(use-package wanderlust
  :ensure t
  :init (setq wl-smtp-connection-type   'ssl
	      wl-smtp-posting-port      465
	      wl-smtp-authenticate-type "plain"
	      wl-smtp-posting-user      "us@calsys.org"
	      wl-smtp-posting-server    "smtp.titan.email"
	      wl-local-domain           "calsys.org"
	      wl-message-id-domain      "smtp.titan.email"
	      wl-from                   "April & May & June <us@calsys.org>"
	      wl-fcc-force-as-read      t
	      wl-default-spec           "%"))

;; Startup

(toggle-frame-maximized)

;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(catppuccin-theme colourful copilot corfu eat eglot
                      exec-path-from-shell expand-region git-gutter
                      git-gutter-fringe highlight-indent-guides
                      lisp-extra-font-lock lsp-mode magit marginalia
                      nerd-icons-completion nerd-icons-corfu
                      projectile sly treemacs treemacs-nerd-icons
                      vertico vterm wanderlust))
 '(package-vc-selected-packages
   '((colourful :url "https://github.com/calsys456/colorful" :branch
                "main")
     (lisp-extra-font-lock :url
                           "https://github.com/calsys456/lisp-extra-font-lock"
                           :branch "main")
     (copilot :url "https://github.com/copilot-emacs/copilot.el"
              :branch "main"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
