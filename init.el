;; Prerequisite

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa-devel" . "https://elpa.gnu.org/devel/"))
(package-initialize)

(setopt use-package-always-ensure t)

(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns x pgtk haiku))
	  (exec-path-from-shell-initialize)))

;; Appearance

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(set-frame-font "Maple Mono NF CN")
(set-face-attribute 'default nil :height 100)

(global-display-line-numbers-mode t)
(display-time-mode 1)
(display-battery-mode 1)

(when (display-graphic-p) (global-hl-line-mode 1))

(use-package catppuccin-theme
  :if (display-graphic-p)
  :init (setq catppuccin-flavor 'frappe
	      catppuccin-italic-comments t
	      catppuccin-italic-variables t
	      catppuccin-italic-blockquotes t)
  :config (load-theme 'catppuccin :no-confirm))

(use-package nerd-icons-completion
  :config (nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :if (display-graphic-p)
  :after (corfu)
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package treemacs-nerd-icons
  :if (display-graphic-p)
  :after (treemacs)
  :config (treemacs-nerd-icons-config))

(use-package kind-icon
  :if (not (display-graphic-p)))

(use-package marginalia
  :custom (marginalia-mode 1))

(use-package doom-modeline
  :if (display-graphic-p)
  :config (doom-modeline-mode 1))

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'column))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode))

(use-package git-gutter-fringe)

;; Editing

(electric-pair-mode 1)
(setopt indent-tabs-mode nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package corfu
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

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :init
  (corfu-terminal-mode 1)
  (set-face-foreground 'corfu-default "red")
  (set-face-foreground 'corfu-popupinfo "red"))

(use-package vertico
  :custom
  (vertico-count 20)
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package savehist
  :init (savehist-mode))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package treemacs
  :config
  (setq treemacs-show-hidden-files nil)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-git-mode 'deferred)
  (global-set-key (kbd "M-0") 'treemacs-select-window))

(use-package vterm)

(use-package magit)

(use-package which-key
  :config (which-key-mode 1))

(use-package consult
  :bind (("C-c r" . consult-ripgrep)
         ("C-c i" . consult-imenu)
         ("C-c f" . consult-fd)
         ("C-x b" . consult-buffer)
         ("M-y"   . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)))

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

(setopt tab-width 4)

(use-package eglot
  :config
  (add-hook 'prog-mode-hook #'eglot-ensure)
  (add-to-list 'eglot-server-programs
	       `(qml-mode ,(or (executable-find "qmlls")
			       (and (file-exists-p "/usr/lib/qt6/bin/qmlls")
				    "/usr/lib/qt6/bin/qmlls"))))
  (let ((clangd (or (executable-find "clangd")
                    (executable-find "clangd-19")
                    (executable-find "clangd-21"))))
    (add-to-list 'eglot-server-programs
                 `((c++-mode c-mode) ,clangd "-header-insertion=never" "-j=8" "--background-index" "--clang-tidy" "--all-scopes-completion"))))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
	    :rev :newest
	    :branch "main")
  :config
  (add-hook 'prog-mode-hook #'(lambda () (copilot-mode 1)))
  (global-set-key (kbd "M-i") 'copilot-accept-completion)
  (setopt copilot-max-char-warning-disable t
          copilot-indent-offset-warning-disable t))

;; Lisp

(use-package sly
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

;; C++ / Qt

(defun cal/c-mode-hook ()
  (c-set-style "user")
  (setq indent-tabs-mode nil
        c-basic-offset 4))

(add-hook 'c-mode-hook 'cal/c-mode-hook)
(add-hook 'c++-mode-hook 'cal/c-mode-hook)

(use-package cmake-mode
  :init (setopt cmake-tab-width 4))

(use-package qml-mode)

;; XML

(setopt sgml-basic-offset 4
        nxml-child-indent 4)

;; Project configuration

(use-package projectile
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
   '(catppuccin-theme cmake-mode colourful consult copilot corfu
                      corfu-terminal doom-modeline eat eglot
                      exec-path-from-shell expand-region git-gutter
                      git-gutter-fringe highlight-indent-guides
                      kind-icon lisp-extra-font-lock lsp-mode magit
                      marginalia nerd-icons-completion nerd-icons-corfu
                      projectile qml-mode sly treemacs
                      treemacs-nerd-icons vertico vterm wanderlust))
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
