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

(defun tty-p ()
  "Return t if running in TTY (or likely environment)."
  (if (fboundp 'tty-type)
      (equal (funcall 'tty-type) "linux")
    (not (or (display-graphic-p)
             (equal (getenv "COLORTERM") "truecolor")))))


;; Appearance

(unless (eq (window-system) 'ns)
  (menu-bar-mode -1))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)
(display-battery-mode 1)
(standard-display-unicode-special-glyphs)

(unless (tty-p) (global-hl-line-mode 1))

(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

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
     ("=" (rx (+ (or "=" "!" "/" ">" "<" ":"))))
     "-------" "#(" ">--" "#?" "<>" "--<" "#!" "</" "<|||" "#:" "/>" "|||>"
     "#=" "</>" "<||" "#_" "<+" "||>" "#__" "+>" "<|" "#_(" "<+>" "|>" "]#" "<*" "<|>" "#######" "*>"
     "_|_" "<<" "<*>" "[TRACE]" "<<<" ">=" "[DEBUG]" ">>" "<=" "[INFO]" ">>>" "<=<" "[WARN]" "{{" ">=>"
     "[ERROR]" "}}"  "[FATAL]" "{|"  "[TODO]" "|}" "!=" "[FIXME]" "{{--" "!==" "[NOTE]" "{{!--"
      "[HACK]" "--}}"  "[MARK]" "[|" "|=" "[EROR]" "|]" "<=>" "[WARNING]" "!!" "<==>" "todo))"
     "||" "<==" "fixme))" "??"  "Cl" "???"  "al" "&&" "<=|" "cl" "&&&" "|=>" "el" 
     "il"  "tl" "/*"  "ul" "/**" ">=<" "xl" "*/" ":=" "ff" "++"  "tt" "+++"
     ":=:" "all" ";;"  "ell" ";;;" ";;;;" "\\ \' \." "ill" ".." "--" "ull" "..." "---" "ll" ".?" "<!--"))
  (global-ligature-mode 1))

(use-package catppuccin-theme
  :ensure t
  :if (not (tty-p))
  :init (setq catppuccin-flavor 'frappe
              catppuccin-italic-comments t
              catppuccin-italic-variables t
              catppuccin-italic-blockquotes t)
  :config (load-theme 'catppuccin :no-confirm))

(use-package marginalia
  :ensure t
  :init (add-hook 'after-init-hook (lambda () (marginalia-mode 1))))

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

(use-package dashboard
  :ensure t
  :config
  (setopt dashboard-center-content t
          dashboard-vertically-center-content t)
  (dashboard-setup-startup-hook))

;; Icons

(use-package kind-icon
  :ensure t
  :if (tty-p))

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

;; Posframe

(use-package transient-posframe
  :ensure t
  :unless (tty-p)
  :config (transient-posframe-mode 1))

(use-package vertico-posframe
  :ensure t
  :unless (tty-p)
  :after (vertico)
  :config (vertico-posframe-mode 1))

;; Sideline

(use-package sideline
  :ensure t
  :hook (prog-mode . sideline-mode)
  :custom
  (sideline-backends-right '(sideline-lsp sideline-flymake sideline-blame))
  (sideline-delay 0.5))

(use-package sideline-lsp
  :ensure t
  :after (lsp-mode sideline)
  :custom (sideline-lsp-code-actions-prefix "> "))

(use-package sideline-blame
  :ensure t
  :after (sideline))

(use-package sideline-flymake
  :ensure t
  :after (sideline)
  :hook (flymake-mode . sideline-mode)
  :custom (sideline-flymake-display-mode 'line))


;; Editing

(electric-pair-mode 1)
(setopt indent-tabs-mode nil)
(global-subword-mode 1)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package cape
  :ensure t)

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
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
  :config
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

  (defvar cal/vterm-parent-frame nil)
  (defvar cal/vterm-buffer nil)
  (defvar cal/vterm-posframe nil)
  (defun cal/vterm-toggle (&optional arg)
    (interactive "P")
    (cl-flet ((poop (buf-or-name &rest args)
                (setq cal/vterm-buffer buf-or-name
                      cal/vterm-posframe
                      (posframe-show
                       buf-or-name
                       :poshandler (lambda (info)
                                     (setq cal/vterm-parent-frame (plist-get info :parent-frame))
                                     (cons (/ (- (plist-get info :parent-frame-width) (plist-get info :posframe-width)) 2)
                                           (/ (- (plist-get info :parent-frame-height) (plist-get info :posframe-height)) 2)))
                       :min-width (min 80 (frame-width))
                       :min-height (min 24 (frame-height))
                       :width (floor (* (frame-width) 0.8))
                       :height (floor (* (frame-height) 0.9))
                       :max-width 200
                       :max-height 60
                       :border-width 2
                       :border-color (face-foreground 'default)
                       :cursor t
                       :accept-focus t))
                (select-frame-set-input-focus cal/vterm-posframe)))
      (if (and cal/vterm-posframe
               (frame-live-p cal/vterm-posframe))
          (if (frame-visible-p cal/vterm-posframe)
              (progn (make-frame-invisible cal/vterm-posframe)
                     (select-frame-set-input-focus cal/vterm-parent-frame))
            (progn (set-frame-position
                    cal/vterm-posframe
                    (/ (- (frame-pixel-width) (frame-pixel-width cal/vterm-posframe)) 2)
                    (/ (- (frame-pixel-height) (frame-pixel-height cal/vterm-posframe)) 2))
                   (make-frame-visible cal/vterm-posframe)
                   (select-frame-set-input-focus cal/vterm-posframe)))
        (if (buffer-live-p cal/vterm-buffer)
            (poop cal/vterm-buffer)
          (vterm--internal #'poop arg)))))
  :bind ("C-c t" . cal/vterm-toggle))

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

(use-package consult-lsp
  :ensure t
  :after (lsp-mode consult)
  :bind
  ("C-c /" . consult-lsp-symbols))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (add-to-list 'completion-styles 'flex)

  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))


;; Language features

(setopt tab-width 4)

(setopt compilation-scroll-output t)

(use-package lsp-mode
  :vc (:url "~/Software/lsp-mode/"
       :branch "master"
       :rev :newest)
  :ensure t
  :diminish
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p
                                 'emacs-lisp-mode 'lisp-mode
                                 'makefile-mode 'snippet-mode
                                 'ron-mode)
                          (lsp-deferred))))
         ((markdown-mode yaml-mode yaml-ts-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-completion-no-cache t)
  (lsp-semantic-tokens-enable t)
  (lsp-enable-on-type-formatting nil)
  (lsp-clients-clangd-args '("-j=8"
                             "--all-scopes-completion"
                             "--background-index"
                             "--clang-tidy"
                             "--header-insertion=never"
                             "--query-driver=/**/*"))
  :config
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-buster)
  (defun cal/lsp-filter-clangd-command (command)
    (if-let* ((proj (project-current))
              (proj-name (project-name proj))
              (proj-root (project-root proj)))
        (if (member proj-name '("treeland" "ddm" "treeland-protocols"))
            (let ((remote-root (file-name-concat cal/deepin-dev-remote-project-dir proj-name))
                  (remote-build (file-name-concat cal/deepin-dev-remote-build-dir proj-name)))
              (list "ssh" cal/deepin-dev-machine-name
                    (string-join `("cd" ,remote-root "&&" "clangd"
                                   ,@lsp-clients-clangd-args
                                   ,(format "--compile-commands-dir=%s" remote-build))
                                 " ")))
          command)
      command))
  (advice-add 'lsp-clients--clangd-command :filter-return 'cal/lsp-filter-clangd-command)
  (defun cal/lsp-filter-uri-to-path (path)
    (or (when-let* ((proj (project-current))
                    (proj-name (project-name proj)))
          (when (and (not (file-exists-p "/etc/deepin_version"))
                     (member proj-name '("treeland" "ddm" "treeland-protocols")))
            (let ((remote-root (file-name-concat cal/deepin-dev-remote-project-dir proj-name))
                  (local-root (directory-file-name (expand-file-name (project-root proj)))))
              (if (string-prefix-p remote-root path)
                  (string-replace remote-root local-root path)
                (format "/ssh:%s:%s" cal/deepin-dev-machine-name path)))))
        path))
  (defun cal/lsp-filter-path-to-uri (path)
    (let ((path (car path)))
      (or (when-let* ((proj (project-current))
                      (proj-name (project-name proj)))
            (when (and (not (file-exists-p "/etc/deepin_version"))
                       (member proj-name '("treeland" "ddm" "treeland-protocols")))
              (let ((local-root (directory-file-name (expand-file-name (project-root (project-current)))))
                    (remote-root (file-name-concat cal/deepin-dev-remote-project-dir proj-name)))
                (when (string-prefix-p local-root path)
                  (list (string-replace local-root remote-root path))))))
          (list path))))
  (advice-add 'lsp--uri-to-path :filter-return 'cal/lsp-filter-uri-to-path)
  (advice-add 'lsp--path-to-uri :filter-args 'cal/lsp-filter-path-to-uri))

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-position 'at-point))

(use-package format-all
  :ensure t
  :diminish
  :bind (("C-c =" . format-all-region-or-buffer)))

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
            :branch "main")
  :config (lisp-extra-font-lock-global-mode 1))

(use-package lisp-semantic-hl
  :vc (:url "https://github.com/calsys456/lisp-semantic-hl.el"
            :branch "main")
  :hook ((emacs-lisp-mode lisp-mode) . lisp-semantic-hl-mode))

(use-package package-lint
  :ensure t
  :defer t)

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

(defvar cal/deepin-dev-remote-project-dir "/root/dev"
  "Location of projects on the deepin development machine.")

(defvar cal/deepin-dev-remote-build-dir "/root/build"
  "Location of build directories on the deepin development machine.")

(defun cal/deepin-dev-handle-compile-filename (filename)
  (let* ((proj (project-current))
         (proj-name (project-name proj)))
    (if (and (member proj-name '("treeland" "ddm" "treeland-protocols"))
             (not (file-exists-p "/etc/deepin_version")))
        (let ((remote-root (file-name-concat cal/deepin-dev-remote-project-dir proj-name)))
          (if (string-prefix-p remote-root filename)
              (string-replace remote-root (directory-file-name (project-root proj)) filename)
            (format "/ssh:%s:%s" cal/deepin-dev-machine-name filename)))
      filename)))

(setq compilation-parse-errors-filename-function
      'cal/deepin-dev-handle-compile-filename)

(defun cal/deepin-configure-cmake ()
  (interactive)
  (let* ((default-directory (project-root (project-current))))
    (if (file-exists-p "/etc/deepin_version")
        (compile "cmake -B build -G Ninja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_CXX_FLAGS='${CMAKE_CXX_FLAGS} -Wall -Wextra -Werror -Wno-stringop-overflow' .")
      (compile (format "ssh %s cmake -B %s -G Ninja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_CXX_FLAGS='${CMAKE_CXX_FLAGS} -Wall -Wextra -Werror -Wno-stringop-overflow' %s"
                       cal/deepin-dev-machine-name
                       (file-name-concat cal/deepin-dev-remote-build-dir (project-name (project-current)))
                       (file-name-concat cal/deepin-dev-remote-project-dir (project-name (project-current))))))))

(defun cal/deepin-build-cmake ()
  (interactive)
  (let* ((default-directory (project-root (project-current))))
    (if (file-exists-p "/etc/deepin_version")
        (compile "cmake --build build")
      (compile (format "ssh %s cmake --build %s"
                       cal/deepin-dev-machine-name
                       (file-name-concat cal/deepin-dev-remote-build-dir (project-name (project-current))))))))

(defun cal/deepin-build-and-install-cmake ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (if (file-exists-p "/etc/deepin_version")
        (compile "cmake --build build && sudo cmake --install build")
      (let ((build-dir (file-name-concat cal/deepin-dev-remote-build-dir (project-name (project-current)))))
        (compile (format "ssh %s 'cmake --build %s && sudo cmake --install %s'"
                         cal/deepin-dev-machine-name
                         build-dir
                         build-dir))))))

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
;; (toggle-frame-maximized)

(use-package kkp
  :ensure t
  :unless (display-graphic-p)
  :config (global-kkp-mode 1))

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


;; Funny time

(defun cal/cl-loop-finding-handler ()
  "April's powerless finding handler that poorly imitating
Jonathan Amsterdam's powerful iteration facility"
  (let* ((expr (pop cl--loop-args))
         (kind (cl-case (pop cl--loop-args)
                 ((maximize maximizing) 'max)
                 ((minimize minimizing) 'min)
                 (such-that 'such-that)
                 (t (error "Invalid finding clause"))))
         (test-expr (pop cl--loop-args))
         (value (cl--loop-handle-accum nil)))
    (if (eq kind 'such-that)
        (let ((test-result (gensym "--cl-var--finder-test-result-")))
          (push `((,test-result nil))
                cl--loop-bindings)
          (push `(progn (setq ,test-result ,(if (and (listp test-expr) (eq (car test-expr) 'function))
                                                `(funcall ,test-expr ,expr)
                                              test-expr))
                        (when ,test-result
                          (setq ,value ,expr))
                        (null ,test-result))
                cl--loop-body))
      (let ((peak-score (gensym "--cl-var--finder-peak-score-"))
            (this-score (gensym "--cl-var--finder-this-score-")))
        (push `((,peak-score nil) (,this-score nil))
              cl--loop-bindings)
        (push `(progn (setq ,this-score ,(if (and (listp test-expr) (eq (car test-expr) 'function))
                                             `(funcall ,test-expr ,expr)
                                           test-expr))
                      (when (or (null ,peak-score)
                                (,(if (eql kind 'max) '> '<) ,this-score ,peak-score))
                        (setq ,peak-score ,this-score
                              ,value ,expr))
                      t)
              cl--loop-body)))))

(setf (get 'find    'cl-loop-handler) #'cal/cl-loop-finding-handler)
(setf (get 'finding 'cl-loop-handler) #'cal/cl-loop-finding-handler)

;; (cl-loop for i from -10 by 0.3
;;          finding i such-that #'cl-plusp) ; => 0.2000000000000009

;; (cl-loop for (key value) on '(:a 1 :c 3 :b 2) by #'cddr
;;          finding key maximizing value) ; => :c


;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((fzf-native :url "https://github.com/dangduc/fzf-native" :branch
                 "main")
     (lisp-extra-font-lock :url
                           "https://github.com/calsys456/lisp-extra-font-lock"
                           :branch "main")
     (copilot :url "https://github.com/copilot-emacs/copilot.el"
              :branch "main")))
 '(safe-local-variable-values
   '((eval and buffer-file-name
           (not (eq major-mode 'package-recipe-mode))
           (or (require 'package-recipe-mode nil t)
               (let ((load-path (cons "../package-build" load-path)))
                 (require 'package-recipe-mode nil t)))
           (package-recipe-mode))
     (cmake-tab-width . 4))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
