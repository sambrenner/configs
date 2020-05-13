;; -*- lisp -*-

;; loadpaths
(add-to-list 'load-path "~/.emacs.d/nonelpa/")

;; startup
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; general global things
(if window-system
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

(menu-bar-mode -1)
(line-number-mode -1)
(display-time-mode 1)
(delete-selection-mode 1)

(setq-default line-spacing 2)

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(defalias 'yes-or-no-p 'y-or-n-p)

(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-c C-s") 'helm-do-ag)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq next-line-add-newlines t)

(setq tramp-default-method "ssh")

(add-hook 'after-init-hook #'global-emojify-mode)

(custom-set-variables
 '(global-emojify-mode t)
 '(helm-completion-style (quote emacs))
 '(markdown-command
   "/usr/bin/pandoc -c /home/sam/.emacs.d/github-pandoc.css --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")
 '(package-selected-packages
   (quote
    (tide emojify stylus-mode pbcopy doom-modeline doom-themes dumb-jump winum julia-mode rjsx-mode flycheck-rust rust-mode window-numbering pdf-tools company-tern yaml-mode web-mode web-beautify transpose-frame smart-mode-line scss-mode powerline php-auto-yasnippets multi-term markdown-toc magit js2-mode helm-company helm-ag glsl-mode buffer-move apache-mode)))
 '(safe-local-variable-values (quote ((eval setq web-mode-set-engine "ctemplate"))))
 '(visible-bell nil))

;; custom vars
(custom-set-faces
 '(doom-visual-bell ((t (:background "#ff6c6b")))))

;; package manager stuff
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-refresh-contents)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
		 nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
		   (package-install package)
		 package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
	(package-refresh-contents))

(ensure-package-installed 'magit 'tide 'php-mode 'company 'company-tern 'markdown-mode 'markdown-toc 'apache-mode 'helm 'helm-company 'web-mode 'yasnippet 'php-auto-yasnippets 'glsl-mode 'scss-mode 'multi-term 'smart-mode-line 'js2-mode 'helm-ag 'web-beautify 'yaml-mode 'transpose-frame 'buffer-move 'tern 'exec-path-from-shell 'amd-mode 'eslintd-fix 'flycheck 'gulp-task-runner 'indium 'js2-mode 'js2-refactor 'projectile 'xref-js2 'dumb-jump 'doom-themes 'winum 'julia-mode 'use-package 'emojify)

;; path from shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; shell
(require 'multi-term)
(setq system-uses-terminfo nil
      multi-term-program "/bin/zsh")
(add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))
(add-hook 'term-mode-hook (lambda () (linum-mode -1)))

;; dumb jump
(dumb-jump-mode)
(global-set-key (kbd "C-M-d") 'dumb-jump-go)

;; winum
(require 'winum)
(winum-mode)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; nxml-mode
(setq nxml-child-indent 2)

;; glsl-mode
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;; scss-mode
(add-to-list 'auto-mode-alist '("\\.styl\\'" . scss-mode))
(setq css-indent-offset 2)

;; company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)

;; julia-mode
(require 'julia-mode)
(setq julia-indent-offset 2)

;;; emacs-js.el --- JS-mode setup
;; Copyright (C) 2016  Nicolas Petton
;; Author: Nicolas Petton <nicolas@petton.fr>

(require 'js2-mode)
(require 'js2-refactor)
(require 'amd-mode)
(require 'eslintd-fix)
(require 'tern)
(require 'gulp-task-runner)
(require 'company-tern)
(require 'helm-company)
(require 'flycheck)
(require 'xref-js2)
(require 'yasnippet)
(require 'indium)

(if (executable-find "eslint_d")
    (setq flycheck-javascript-eslint-executable "eslint_d")
  (warn "emacs-js: You might want to install eslint_d: sudo npm install -g eslint_d."))

(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(setq js2-basic-offset 2)
(setq js-indent-level 2)
(setq sgml-basic-offset 2)

;; Also ignore some other files
(dolist (file '("require.js" "highcharts.js" "highcharts.src.js" "bootstrap.js" "Gruntfile.js" "moment.js" "moment-with-locales.js"))
  (add-to-list 'xref-js2-ignored-files file))

;; tern will override js2r keybindings...
(define-key tern-mode-keymap (kbd "C-c C-r") nil)

;; ... and xref.
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

(js2r-add-keybindings-with-prefix "C-c C-r")
(setq js2r-always-insert-parens-around-arrow-function-params t)
(setq js2r-prefer-let-over-var t)

(define-key js-mode-map (kbd "M-.") nil)
(define-key js-mode-map (kbd "C-c C-j") nil)

(define-key amd-mode-map (kbd "C-c C-a") #'amd-initialize-makey-group)
(setq amd-use-relative-file-name t)

;; eslint parser executable can be overridden in some projects but marked as
;; risky, so silence that.
(put 'flycheck-javascript-eslint-executable 'risky-local-variable nil)

(defun kill-tern-process ()
  "Kill the tern process if any.
The process will be restarted.  This is useful if tern becomes
unreachable."
  (interactive)
  (delete-process "Tern"))

(eval-after-load 'company '(add-to-list 'company-backends 'company-tern))

;; paredit-like commands for JS
(define-key js-mode-map (kbd "<C-right>") #'js2r-forward-slurp)
(define-key js-mode-map (kbd "<C-left>") #'js2r-forward-barf)
(define-key js-mode-map (kbd "C-k") #'js2r-kill)

;;; Convenience functions
(add-to-list 'yas-snippet-dirs
             (expand-file-name "snippets"
                               (file-name-directory
                                (or load-file-name buffer-file-name)))
             t)
(yas-reload-all)
;;; emacs-js.el ends here

(defun my/activate-tide-mode ()
  "Use hl-identifier-mode only on js or ts buffers."
  (when (and (stringp buffer-file-name)
             (string-match "\\.[tj]sx?\\'" buffer-file-name))
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)))

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; flycheck-rust
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; web-mode
(require 'web-mode)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)

(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

(add-hook 'web-mode-hook (lambda()
                           (tern-mode)
                           (company-mode)))

(use-package tide
   :hook (web-mode . my/activate-tide-mode)
   :ensure t)

(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode ))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode ))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode ))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode ))
(add-to-list 'auto-mode-alist '("\\.tag\\'" . web-mode ))
(add-to-list 'auto-mode-alist '("\\.riot\\'" . web-mode ))

(setq web-mode-content-types-alist '(("jsx"  . "\\.js[x]?\\'")))

;; web-mode jsx
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode ))
(defadvice web-mode-highlight-part (around tweat-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; mouse
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )

;; helm
(require 'helm)
(require 'helm-config)
(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "M-r") 'helm-do-ag-project-root)

;; helm company
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

;; helm ag
(setq helm-ag-use-agignore t)

;; mac clipboard
;; (defun copy-from-osx ()
;;  (shell-command-to-string "pbpaste"))

;; (defun paste-to-osx (text &optional push)
;;  (let ((process-connection-type nil))
;;    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;;      (process-send-string proc text)
;;      (process-send-eof proc))))

;; (setq interprogram-cut-function 'paste-to-osx)
;; (setq interprogram-paste-function 'copy-from-osx)

(setq x-select-enable-clipboard t)

;; magit
(setenv "EDITOR" "emacsclient")
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-x g") 'magit-status)

;; apache mode
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;; whitespace
(require 'whitespace)
(setq-default indent-tabs-mode nil
      tab-width 2
      js-indent-level 2)

(defvaralias 'c-basic-offset 'tab-width)

;; python
(add-hook 'python-mode 'run-python)

;; php-mode
(require 'php-mode)
(setq php-mode-force-pear t)
(add-hook 'php-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode t)
	     (setq tab-width 4)
	     (setq c-basic-offset 4)

	     ;; allow one tab after <?php
	     (c-set-offset 'topmost-intro 4)
	     (c-set-offset 'cpp-macro -4)))

;; turn on paren match highlighting
(show-paren-mode 1)

;; linum
(global-linum-mode 1)
(setq linum-format "%4d  ")

;; doom

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  ;;(setq doom-themes-padded-modeline t)

  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-spacegrey t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; doom modeline

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))

(setq doom-modeline-major-mode-icon nil)

;; built-in `project' on 26+
(setq doom-modeline-project-detection 'project)
;; or `find-in-project' if it's installed
(setq doom-modeline-project-detection 'ffip)

;; font
(set-face-attribute 'default nil
                    :family "FiraCode"
                    :height 100
                    :weight 'light
                    :width 'normal)

;; custom fns
(defun insert-datetime nil
  "Insert the ISO-8601 formatted datetime"
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S%z")))

(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun eval-js nil
  "run the provided js"
  (interactive)
  (shell-command
   (concat
    "node -e '"
    (read-string "enter js: ")
    "'")))

(defun eval-log-js nil
  "wraps the provided js in console.log() and runs it"
  (interactive)
  (shell-command
   (concat
    "node -e 'console.log("
    (read-string "enter js: ")
    ")'")))
