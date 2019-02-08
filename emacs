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
(display-time-mode 1)
(delete-selection-mode 1)

;; (set-frame-font "-FBI -Input Mono-light-normal-normal-*-13-*-*-*-m-0-iso10646-1")

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(defalias 'yes-or-no-p 'y-or-n-p)

(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-c C-s") 'helm-do-ag)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq next-line-add-newlines t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2f57ee6507f30d3228cdddadd0150e7b2fd85dd7c818c2d6485888c7249c37e8" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "a802c77b818597cc90e10d56e5b66945c57776f036482a033866f5f506257bca" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "8288b9b453cdd2398339a9fd0cec94105bc5ca79b86695bd7bf0381b1fbe8147" default)))
 '(markdown-command
   "/usr/bin/pandoc -c /home/sam/.emacs.d/github-pandoc.css --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")
 '(package-selected-packages
   (quote
    (pdf-tools xclip company-tern yaml-mode web-mode web-beautify transpose-frame smyx-theme smarty-mode smart-mode-line scss-mode powerline php-auto-yasnippets multi-term markdown-toc magit js2-mode helm-company helm-ag glsl-mode buffer-move apache-mode actionscript-mode)))
 '(safe-local-variable-values (quote ((eval setq web-mode-set-engine "ctemplate")))))

;; custom vars

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(ensure-package-installed 'magit 'smyx-theme 'smarty-mode 'php-mode 'company 'company-tern 'markdown-mode 'markdown-toc 'apache-mode 'helm 'helm-company 'web-mode 'yasnippet 'php-auto-yasnippets 'glsl-mode 'actionscript-mode 'scss-mode 'multi-term 'smart-mode-line 'js2-mode 'helm-ag 'web-beautify 'yaml-mode 'transpose-frame 'buffer-move 'tern 'exec-path-from-shell 'amd-mode 'eslintd-fix 'flycheck 'gulp-task-runner 'indium 'js2-mode 'js2-refactor 'projectile 'xref-js2 'xclip)

;; path from shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; shell
(require 'multi-term)
(setq system-uses-terminfo nil
      multi-term-program "/bin/zsh")
(add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))
(add-hook 'term-mode-hook (lambda () (linum-mode -1)))

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; nxml-mode
(setq nxml-child-indent 4)

;; glsl-mode
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;; xclip
(require 'xclip)
(xclip-mode 1)

;;; emacs-js.el --- JS-mode setup
;; Copyright (C) 2016  Nicolas Petton
;; Author: Nicolas Petton <nicolas@petton.fr>

(require 'js2-mode)
(require 'js2-refactor)
(require 'amd-mode)
(require 'eslintd-fix)
(require 'tern)
(require 'gulp-task-runner)
(require 'company)
(require 'company-tern)
(require 'helm-company)
(require 'flycheck)
(require 'xref-js2)
(require 'yasnippet)
(require 'indium)

(if (executable-find "eslint_d")
    (setq flycheck-javascript-eslint-executable "eslint_d")
  (warn "emacs-js: You might want to install eslint_d: sudo npm install -g eslint_d."))

(add-hook 'js-mode-hook #'setup-js-buffer)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(defun setup-js-buffer ()
  (unless (eq major-mode 'json-mode)
    (company-mode 1)
    (tern-mode 1)
    ;; When the buffer is not visiting a file, eslint systematically fails
    (if buffer-file-name
        (flycheck-mode 1)
      (flycheck-mode -1))
    (js2-minor-mode 1)
    (js2-refactor-mode 1)
    (js2-imenu-extras-mode)
    (indium-interaction-mode 1)
    (amd-mode 1))

  ;; add eslintd-fix support
  (eslintd-fix-mode)

  ;; add xref-js2 support
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)

  (setq js2-basic-offset 4)
  (setq js2-strict-inconsistent-return-warning nil)
  (setq js2-indent-switch-body t)
  (setq js-switch-indent-offset 4)

  (setq js2-global-externs '("define" "require" "app"))
  (setq js2-include-node-externs t)
  (setq js2-pretty-multiline-declarations nil)

  (yas-minor-mode +1)

  (set (make-local-variable 'company-dabbrev-ignore-case) nil)
  (set (make-local-variable 'company-dabbrev-downcase) nil))

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
(define-key js-mode-map (kbd "M-S") #'js-smart-split)

;;; Convenience functions
(add-to-list 'yas-snippet-dirs
             (expand-file-name "snippets"
                               (file-name-directory
                                (or load-file-name buffer-file-name)))
             t)
(yas-reload-all)
;;; emacs-js.el ends here

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; web-mode
(require 'web-mode)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-pairing t)
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-script-padding 4)
  (setq web-mode-style-padding 4)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.txt\\'" . web-mode ))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode ))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode ))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode ))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode ))
(add-to-list 'auto-mode-alist '("\\.tag\\'" . web-mode ))
(add-to-list 'auto-mode-alist '("\\.riot\\'" . web-mode ))
(setq web-mode-engines-alist
      '(("smarty" . "\\.txt\\'")
        ("ctemplate" . "\\.html\\'")
        ("erb" . "\\.erb\\'")
        ("riot" . "\\.tag\\'")
        ("riot" . "\\.riot\\'"))
      )

;; web-mode jsx
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode ))
(defadvice web-mode-highlight-part (around tweat-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; smart-mode-line
(sml/setup)
(sml/apply-theme 'respectful)

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


;; helm ag
(setq helm-ag-use-agignore t)

;; company
;; (require 'company)
;; (global-company-mode 1)
;; (setq company-dabbrev-downcase nil)

;; helm - company - yasnippet
;; (require 'helm-company)

;; (defvar company-mode/enable-yas t
;;   "Enable yasnippet for all backends.")

;; (defun company-mode/backend-with-yas (backend)
;;   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
;;       backend
;;     (append (if (consp backend) backend (list backend))
;; 	    '(:with company-yasnippet))))

;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; ;; helm-company choose from company completions with C-:
;; (eval-after-load 'company
;;   '(progn
;;      (define-key company-mode-map (kbd "C-:") 'helm-company)
;;      (define-key company-active-map (kbd "C-:") 'helm-company)))

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
      tab-width 4
      js-indent-level 4)

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

;; tramp
(setq tramp-default-method "ssh")

;; linum
(global-linum-mode 1)
(setq linum-format "%4d  ")

;; smyx wtf
;; https://stackoverflow.com/questions/11127109/emacs-24-package-system-initializatiton-problems/11140619#11140619
(defun init-smyx()
  (load-theme 'smyx)
  )
(add-hook 'after-init-hook 'init-smyx)
