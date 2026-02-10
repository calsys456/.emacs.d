;; -*- lexical-binding: t; -*-

;; Prerequisite

(setopt gc-cons-threshold 100000000
        read-process-output-max (* 1024 1024))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa-devel" . "https://elpa.gnu.org/devel/"))
(package-initialize)

(setopt use-package-always-ensure t
        ;; use-package-compute-statistics t
        )

;; (use-package benchmark-init
;;   :ensure t
;;   :config (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns x pgtk haiku))
          (exec-path-from-shell-initialize)))

(use-package kkp
  :ensure t
  :unless (display-graphic-p)
  :config (global-kkp-mode 1))

(defun tty-p ()
  "Return t if running in TTY (or likely environment)."
  (if (fboundp 'tty-type)
      (equal (funcall 'tty-type) "linux")
    (not (or (display-graphic-p)
             (equal (getenv "COLORTERM") "truecolor")))))


;; Appearance

(tool-bar-mode -1)

(unless (eq (window-system) 'ns)
  (menu-bar-mode -1))

(scroll-bar-mode -1)

(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))
(display-time-mode 1)
(display-battery-mode 1)
(standard-display-unicode-special-glyphs)

(unless (tty-p) (global-hl-line-mode 1))

(use-package ligature
  :ensure t
  :if (display-graphic-p)
  :config
  (ligature-set-ligatures
   t
   '("::" "?." "<#--" ":::" "..<"
     "<!---->" "?:" ".=" "<->" ":?" "<~" "<-->" ":?>" "~>" "->" "<:" "~~" "<-" ":>" "<~>" "-->" ":<"
     "<~~" "<--" "<:<" "~~>" ">->" ">:>" "-~" "<-<" "__" "~-" "|->" "#{" "~@" "<-|" "#[" "~~~~~~~"
     ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
     "-------" "#(" ">--" "#?" "<>" "--<" "#!" "</" "<|||" "#:" "/>" "|||>"
     "#=" "</>" "<||" "#_" "<+" "||>" "#__" "+>" "<|" "#_(" "<+>" "|>" "]#" "<*" "<|>" "#######" "*>"
     "_|_" "<<" "<*>" "[TRACE]" "<<<" ">=" "[DEBUG]" ">>" "<=" "[INFO]" ">>>" "<=<" "[WARN]" "{{" ">=>"
     "[ERROR]" "}}" "==" "[FATAL]" "{|" "===" "[TODO]" "|}" "!=" "[FIXME]" "{{--" "!==" "[NOTE]" "{{!--"
     "=/=" "[HACK]" "--}}" "=!=" "[MARK]" "[|" "|=" "[EROR]" "|]" "<=>" "[WARNING]" "!!" "<==>" "todo))"
     "||" "<==" "fixme))" "??" "==>" "Cl" "???" "=>" "al" "&&" "<=|" "cl" "&&&" "|=>" "el" "=<="
     "il" "=>=" "tl" "/*" "=======" "ul" "/**" ">=<" "xl" "*/" ":=" "ff" "++" "=:" "tt" "+++"
     ":=:" "all" ";;" "=:=" "ell" ";;;" "\\ \' \." "ill" ".." "--" "ull" "..." "---" "ll" ".?" "<!--"))
  (global-ligature-mode 1))

(use-package catppuccin-theme
  :ensure t
  :if (not (tty-p))
  :init (setq catppuccin-flavor 'frappe
              catppuccin-italic-comments t
              catppuccin-italic-variables t
              catppuccin-italic-blockquotes t)
  :config (load-theme 'catppuccin :no-confirm))

(use-package nerd-icons-completion
  :ensure t
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :if (not (tty-p))
  :after (corfu)
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package treemacs-nerd-icons
  :ensure t
  :if (not (tty-p))
  :after (treemacs)
  :config (treemacs-nerd-icons-config))

(use-package kind-icon
  :ensure t
  :if (tty-p))

(use-package marginalia
  :ensure t
  :custom (add-hook 'after-init-hook (lambda () (marginalia-mode 1))))

(use-package doom-modeline
  :ensure t
  :if (not (tty-p))
  :config (doom-modeline-mode 1))

(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character))

(use-package diff-hl
  :ensure t
  :config (global-diff-hl-mode 1))

(use-package page-break-lines
  :if (display-graphic-p)
  :config (global-page-break-lines-mode 1))

(use-package transient-posframe
  :ensure t
  :unless (tty-p)
  :config (transient-posframe-mode 1))

(use-package vertico-posframe
  :ensure t
  :unless (tty-p)
  :after (vertico)
  :config (vertico-posframe-mode 1))

(use-package dashboard
  :ensure t
  :config
  (setopt dashboard-center-content t
          dashboard-vertically-center-content t)
  (dashboard-setup-startup-hook))


;; Editing

(electric-pair-mode 1)
(setopt indent-tabs-mode nil)
(global-subword-mode 1)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (corfu-quit-no-match t)
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package corfu-terminal
  :ensure t
  :if (and (< emacs-major-version 31)
           (not (display-graphic-p)))
  :init
  (corfu-terminal-mode 1)
  (when (tty-p)
    (set-face-foreground 'corfu-default "red")
    (set-face-foreground 'corfu-popupinfo "red")))

(use-package vertico
  :ensure t
  :custom
  (vertico-count 20)
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode 1))

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
  :ensure t
  :bind ("C-c t" . vterm))

(use-package magit
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :config (which-key-mode 1))

(use-package consult
  :ensure t
  :bind (("C-c r" . consult-ripgrep)
         ("C-c i" . consult-imenu)
         ("C-c f" . consult-fd)
         ("C-x b" . consult-buffer)
         ("M-y"   . consult-yank-pop)
         ("C-c C-s" . consult-line)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)))

(use-package fzf-native
  :vc (:url "https://github.com/dangduc/fzf-native"
            :rev :newest
            :branch "main")
  :config (fzf-native-load-dyn))

(use-package fussy
  :ensure t
  :config
  (fussy-setup)
  (fussy-eglot-setup))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p)

  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))


;; Language features

(setopt tab-width 4)

(setopt compilation-scroll-output t)

(use-package eglot
  :defer t
  :config
  (setopt eglot-semantic-tokens-mode t)
  (setq eglot-code-action-indicator ">")
  (add-hook 'prog-mode-hook #'eglot-ensure)
  (add-to-list 'eglot-server-programs
               `(qml-mode ,(or (executable-find "qmlls")
                               (and (file-exists-p "/usr/lib/qt6/bin/qmlls")
                                    "/usr/lib/qt6/bin/qmlls"))))
  (setf (alist-get 'qml-mode eglot-server-programs nil t #'equal)
        (list (or (executable-find "qmlls") "/usr/lib/qt6/bin/qmlls"))
        
        (alist-get '(c++-mode c-mode) eglot-server-programs nil t #'equal)
        (lambda (interactive project)
          (if (member (project-name project) '("treeland" "ddm" "treeland-protocols"))
              (let ((remote-root (format "/root/%s" (project-name project))))
                (list "ssh" cal/deepin-dev-machine-name
                      (string-join (list "cd" remote-root "&&" "clangd" "-j=8"
                                         "--header-insertion=never"
                                         "--background-index"
                                         "--clang-tidy"
                                         "--all-scopes-completion"
                                         "--query-driver=/**/*"
                                         (format "--path-mappings=%s=%s" (expand-file-name (project-root project)) remote-root)
                                         (format "--compile-commands-dir=/root/build/%s" (project-name project)))
                                   " ")))
            
            (list "clangd" "-j=8"
                  "--header-insertion=never"
                  "--background-index"
                  "--clang-tidy"
                  "--all-scopes-completion"
                  "--query-driver=/**/*"))))

  (defun cal/eglot-filter-uri-to-path (path)
    (if (and (not (file-exists-p "/etc/deepin_version"))
             (member (project-name (project-current)) '("treeland" "ddm" "treeland-protocols"))
             (or (string-prefix-p "/usr" path)
                 (string-prefix-p "/root" path)))
        (format "/ssh:%s:%s" cal/deepin-dev-machine-name path)
      path))
  (advice-add 'eglot-uri-to-path :filter-return 'cal/eglot-filter-uri-to-path))

(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

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
  :ensure t
  :defer t
  :init (setq sly-lisp-implementations
              '((sbcl ("sbcl"))
                (ecl ("ecl"))
                (roswell ("ros -Q run")))))

(use-package lisp-extra-font-lock
  :vc (:url "https://github.com/calsys456/lisp-extra-font-lock"
            :rev :newest
            :branch "main")
  :config (lisp-extra-font-lock-global-mode 1))

(use-package colourful
  :vc (:url "https://github.com/calsys456/colorful"
            :rev :newest
            :branch "main")
  :config
  (add-hook 'emacs-lisp-mode-hook 'colourful-mode)
  (add-hook 'lisp-mode-hook 'colourful-mode))

;; C++ / Qt

(defun cal/c-mode-hook ()
  (c-set-style "user")
  (setq indent-tabs-mode nil
        c-basic-offset 4))

(add-hook 'c-mode-hook 'cal/c-mode-hook)
(add-hook 'c++-mode-hook 'cal/c-mode-hook)

(use-package cmake-mode
  :ensure t
  :defer t
  :init (setopt cmake-tab-width 4))

(use-package qml-mode
  :ensure t
  :defer t)

;; XML

(setopt sgml-basic-offset 4
        nxml-child-indent 4)

;; Nix

(use-package nix-mode
  :ensure t
  :defer t
  :mode "\\.nix\\'")

;; Applescript

(use-package apples-mode
  :ensure t
  :defer t
  :mode "\\.applescript\\'"
  :config (add-to-list 'apples-indenters "to"))

;; Org

(use-package org
  :defer t)


;; Project configuration

(defvar cal/deepin-dev-machine-name "deepin-dev"
  "The hostname of the Deepin development machine.")

(defun cal/deepin-configure-cmake ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (if (file-exists-p "/etc/deepin_version")
        (compile "cmake -B build -G Ninja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_CXX_FLAGS='${CMAKE_CXX_FLAGS} -Wall -Wextra -Werror -Wno-stringop-overflow' .")
      (compile (format "ssh %s cmake -B /root/build/%s -G Ninja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_CXX_FLAGS='${CMAKE_CXX_FLAGS} -Wall -Wextra -Werror -Wno-stringop-overflow' /root/%s"
                       cal/deepin-dev-machine-name
                       (project-name (project-current))
                       (project-name (project-current)))))))

(defun cal/deepin-build-cmake ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (if (file-exists-p "/etc/deepin_version")
        (compile "cmake --build build")
      (compile (format "ssh %s cmake --build /root/build/%s"
                       cal/deepin-dev-machine-name
                       (project-name (project-current)))))))

(defun cal/deepin-build-and-install-cmake ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (if (file-exists-p "/etc/deepin_version")
        (compile "cmake --build build && sudo cmake --install build")
      (compile (format "ssh %s 'cmake --build /root/build/%s && sudo cmake --install /root/build/%s'"
                       cal/deepin-dev-machine-name
                       (project-name (project-current))
                       (project-name (project-current)))))))

(defun cal/deepin-restart-ddm ()
  (interactive)
  (let ((default-directory (project-root (project-current)))
        (cmd "loginctl | awk '{if ($1+0 == $1 && /seat0/) { print $1 | \"xargs sudo loginctl terminate-session\" }}' \
systemctl stop ddm \
pkill ddm || true; pkill treeland || true \
systemctl set-environment QT_LOGGING_RULES='treeland.*.*=true' \
systemctl daemon-reload \
sudo systemctl restart ddm.service"))
    (if (file-exists-p "/etc/deepin_version")
        (compile cmd)
      (compile (format "ssh %s '%s'" cal/deepin-dev-machine-name cmd)))))


;; Internet

(use-package wanderlust
  :ensure t
  :defer t
  :init (setq elmo-imap4-default-user   "us@calsys.org"
              elmo-imap4-default-server "imap.titan.email"
              elmo-imap4-default-authenticate-type 'clear
              elmo-imap4-default-stream-type 'ssl
              elmo-imap4-default-port   993
              wl-smtp-connection-type   'ssl
              wl-smtp-posting-port      465
              wl-smtp-authenticate-type "plain"
              wl-smtp-posting-user      "us@calsys.org"
              wl-smtp-posting-server    "smtp.titan.email"
              wl-local-domain           "calsys.org"
              wl-message-id-domain      "smtp.titan.email"
              wl-from                   "April & May & June <us@calsys.org>"
              wl-fcc-force-as-read      t
              wl-default-spec           "%"
              wl-summary-width          nil)
  :config
  (when (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
  (when (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook)))


;; Startup

(when (display-graphic-p)
  (set-frame-font "Maple Mono NF CN")
  (set-face-attribute 'default nil :height (if (eq system-type 'darwin) 120 100)))

(unless (display-graphic-p)
  (xterm-mouse-mode 1))

(setf (alist-get 'alpha default-frame-alist) 90)
(set-frame-parameter nil 'alpha 90)
(toggle-frame-maximized)

(when (and (not (display-graphic-p))
           (fboundp 'kkp--this-terminal-supports-kkp-p)
           (funcall 'kkp--this-terminal-supports-kkp-p))
  (send-string-to-terminal
   (format "]21;%s"
           (apply #'concat
                  (cl-loop for i from 1 to 7
                           for color in (list (face-background 'default)
                                              (face-background 'highlight)
                                              (face-background 'mode-line)
                                              (face-background 'mode-line-inactive)
                                              (face-background 'diff-hl-change)
                                              (face-background 'diff-hl-delete)
                                              (face-background 'diff-hl-insert))
                           collect (format "transparent_background_color%d=%s@-1;" i color))))))


;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(aio apples-mode applescript-mode benchmark-init catppuccin-theme
         cmake-mode colourful consult copilot corfu-terminal dashboard
         diff-hl doom-modeline editorconfig-mode eglot esup
         exec-path-from-shell expand-region fussy fzf-native
         git-gutter-fringe highlight-indent-guides kind-icon kkp
         ligature lisp-extra-font-lock lsp-mode lsp-treemacs lsp-ui
         magit marginalia mcp mu4e nerd-icons-completion
         nerd-icons-corfu nix-mode page-break-lines polymode
         projectile qml-mode rainbow-delimiters request shell-maker
         sly transient-posframe treemacs-nerd-icons vertico-posframe
         vterm wanderlust))
 '(package-vc-selected-packages
   '((fzf-native :url "https://github.com/dangduc/fzf-native" :branch
                 "main")
     (colourful :url "https://github.com/calsys456/colorful" :branch
                "main")
     (lisp-extra-font-lock :url
                           "https://github.com/calsys456/lisp-extra-font-lock"
                           :branch "main")
     (copilot :url "https://github.com/copilot-emacs/copilot.el"
              :branch "main")))
 '(safe-local-variable-values '((cmake-tab-width . 4))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
