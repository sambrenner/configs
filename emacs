;; -*- lisp -*-

;; package manager stuff
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-refresh-contents)

;; use-package setup
(eval-when-compile
  (add-to-list 'load-path "/home/sam/.emacs.d/elpa/use-package-20200322.2110/use-package.el")
  (require 'use-package))

;; load paths
(add-to-list 'load-path "~/.emacs.d/nonelpa/")

;; startup
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; font
(set-face-attribute 'default nil
                    :family "FiraCode"
                    :height 100
                    :weight 'light
                    :width 'normal)

;; general global things
(if window-system
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

(menu-bar-mode -1)
(line-number-mode -1)
(display-time-mode 1)
(delete-selection-mode 1)
(show-paren-mode 1)

(setq-default line-spacing 2)
(setq-default tramp-default-method "ssh")
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq next-line-add-newlines t)

(add-hook 'after-init-hook #'global-emojify-mode)
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(defalias 'yes-or-no-p 'y-or-n-p)

;; code formatting
(setq-default js-indent-level 2
              sgml-basic-offset 2
              nxml-child-indent 2
              indent-tabs-mode nil
              tab-width 2)

(defvaralias 'c-basic-offset 'tab-width)

;; custom shortcuts
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-c C-s") 'helm-do-ag)

;; path from shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; mac clipboard
(when (memq window-system '(mac ns))
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;; shell
(use-package multi-term
  :custom
  (system-uses-terminfo nil)
  (multi-term-program "/bin/zsh")
  :ensure t
  :init
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))
  (add-hook 'term-mode-hook (lambda () (linum-mode -1))))

;; dumb jump
(use-package dumb-jump
  :ensure t
  :bind ("C-M-d" . dumb-jump-go))

;; winum
(use-package winum
  :ensure t)

;; scss-mode
(use-package scss-mode
  :mode "\\.styl\\'"
  :config
  (css-indent-offset 2)
  :ensure t)

;; company mode
(use-package company
  :custom
  (company-idle-delay 0.1)
  :hook
  (after-init . global-company-mode)
  :bind (:map company-mode-map
              ([remap indent-for-tab-command] . company-indent-or-complete-common))
  :ensure t)

;; flycheck
(use-package flycheck
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)

  ;; flycheck -> eslint_d
  (if (executable-find "eslint_d")
      (setq flycheck-javascript-eslint-executable "eslint_d")
    (warn "emacs cannot find eslint_d. maybe you need to sudo npm install -g eslint_d"))

  ;; flycheck -> rust
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

  :ensure t)

;; web-mode
(use-package web-mode
  :custom
  (web-mode-script-padding 2)
  (web-mode-style-padding 2)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing t)
  (web-mode-attr-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-quoting nil)
  (web-mode-content-types-alist '(("jsx"  . "\\.js[x]?\\'")))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

  :init
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode ))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode ))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode ))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode ))
  (add-to-list 'auto-mode-alist '("\\.tag\\'" . web-mode ))
  (add-to-list 'auto-mode-alist '("\\.riot\\'" . web-mode ))
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode ))
  (defadvice web-mode-highlight-part (around tweat-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  :ensure t
)

;; tide
(defun init-tide-mode ()
  "Use hl-identifier-mode only on js or ts buffers."
  (when (and (stringp buffer-file-name)
             (string-match "\\.[tj]sx?\\'" buffer-file-name))
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)))

(use-package tide
  :hook (web-mode . init-tide-mode)
  :custom
  (tide-tsserver-executable "/home/sam/.nvm/versions/node/v12.16.1/bin/tsserver")
  :ensure t)

;; helm
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-d" . helm-browse-project))
  :ensure t)

;; helm company
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

;; helm ag
(use-package helm-ag
  :bind (("M-r" . helm-do-ag-project-root))
  :custom
  (helm-ag-use-agignore t)
  :ensure t)

;; magit
(use-package magit
  :custom
  (magit-auto-revert-mode nil)
  (magit-last-seen-setup-instructions "1.4.0")
  :bind (("C-x g" . magit-status))
  :init
  (setenv "EDITOR" "emacsclient")
  :ensure t)

;; whitespace
(use-package whitespace
  :ensure t)

;; linum
(use-package linum
  :init
  (global-linum-mode 1)
  :custom
  (linum-format "%4d  ")
  :ensure t)

;; doom
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-spacegrey t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; doom modeline
(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode)
      :custom
      (doom-modeline-major-mode-icon nil)
      (doom-modeline-project-detection 'project)
      (doom-modeline-project-detection 'ffip))

;; custom fns
(defun insert-datetime nil
  "Insert the ISO-8601 formatted datetime."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S%z")))

(defun create-scratch-buffer nil
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun eval-js nil
  "Run the provided js."
  (interactive)
  (shell-command
   (concat
    "node -e '"
    (read-string "enter js: ")
    "'")))

(defun eval-log-js nil
  "Wrap the provided js in console.log() and run it."
  (interactive)
  (shell-command
   (concat
    "node -e 'console.log("
    (read-string "enter js: ")
    ")'")))

;; autogenerated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(add-to-list (quote web-mode-indentation-params) t)
 '(helm-completion-style (quote emacs))
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(safe-local-variable-values (quote ((eval setq web-mode-set-engine "ctemplate"))))
 '(system-uses-terminfo nil t)
 '(visible-bell nil))

;; custom vars
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-visual-bell ((t (:background "#ff6c6b")))))
