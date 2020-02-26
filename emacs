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

(set-frame-font "-FBI -Input Mono Narrow-extralight-normal-normal-*-15-*-*-*-m-0-iso10646-1")

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(defalias 'yes-or-no-p 'y-or-n-p)

(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-c C-s") 'helm-do-ag)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq next-line-add-newlines t)

(setq tramp-default-method "ssh")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("07e3a1323eb29844e0de052b05e21e03ae2f55695c11f5d68d61fb5fed722dd2" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "fa3bdd59ea708164e7821574822ab82a3c51e262d419df941f26d64d015c90ee" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "bcc332a1d8564fdf44609c5d09ab2ab90973aeb6174e0b7596252aaf560d9e57" "5aed376a90e91110dbef9ee3789566b40609a8b82b5572ea63b895eacc9a0884" "dce5658477ccb6f42c0302a70affb258d09de99a72cef566a39d00bb829a473b" "fdbb12feab54454ca98ea6b6a5dc2d9fd6da9c7bc8ce1c3a7b9b59a62426dc7e" "e6babe7db0237266022eef0bca842f58a097e9688aeeb295da7593b49d12cc39" "b20214dddc2ecc81f1d711821f11f5587effc69c3b9f026291371474a28d2958" "77c6fe9013d1af3a8acc73b44b24b77fef3e370cfa7c0b97ba20edf3b430a0a3" "229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" "cb96a06ed8f47b07c014e8637bd0fd0e6c555364171504680ac41930cfe5e11e" "72fda75af7caddec17ba9b49d2f99703c20a5f5f5c4dcec641d34a0b83569e88" "88a3c267ce2132defd46f2a4761925983dcbc35b1c3cfff1dded164ce169fed4" "74a42b2b5dde1057e66bcf4c241789213e0ed5b77a2ee41c982fdc8c2abe9d98" "ca849ae0c889eb918785cdc75452b1e11a00848a5128a95a23872e0119ccc8f4" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "1c8171893a9a0ce55cb7706766e57707787962e43330d7b0b6b0754ed5283cda" "1ed5c8b7478d505a358f578c00b58b430dde379b856fbcb60ed8d345fc95594e" "615123f602c56139c8170c153208406bf467804785007cdc11ba73d18c3a248b" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "d261bb8f66be37752791a67f03dd24361592ce141b32d83bcbe63ec1c738b087" "0809c08440b51a39c77ec5529f89af83ab256a9d48107b088d40098ce322c7d8" "2f57ee6507f30d3228cdddadd0150e7b2fd85dd7c818c2d6485888c7249c37e8" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "a802c77b818597cc90e10d56e5b66945c57776f036482a033866f5f506257bca" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "8288b9b453cdd2398339a9fd0cec94105bc5ca79b86695bd7bf0381b1fbe8147" default))
 '(doom-nord-brighter-modeline t)
 '(doom-nord-padded-modeline t)
 '(doom-one-brighter-modeline t)
 '(doom-smyck-brighter-modeline t)
 '(doom-themes-padded-modeline t)
 '(markdown-command
   "/usr/bin/pandoc -c /home/sam/.emacs.d/github-pandoc.css --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")
 '(package-selected-packages
   '(doom-modeline doom-themes dumb-jump winum julia-mode rjsx-mode flycheck-rust rust-mode window-numbering pdf-tools company-tern yaml-mode web-mode web-beautify transpose-frame smyx-theme smart-mode-line scss-mode powerline php-auto-yasnippets multi-term markdown-toc magit js2-mode helm-company helm-ag glsl-mode buffer-move apache-mode))
 '(safe-local-variable-values '((eval setq web-mode-set-engine "ctemplate")))
 '(visible-bell nil))

;; custom vars

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#3c4c55" :foreground "#c5d4dd" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight thin :height 150 :width extra-condensed :foundry "nil" :family "Input Mono Narrow"))))
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

(ensure-package-installed 'magit 'smyx-theme 'php-mode 'company 'company-tern 'markdown-mode 'markdown-toc 'apache-mode 'helm 'helm-company 'web-mode 'yasnippet 'php-auto-yasnippets 'glsl-mode 'scss-mode 'multi-term 'smart-mode-line 'js2-mode 'helm-ag 'web-beautify 'yaml-mode 'transpose-frame 'buffer-move 'tern 'exec-path-from-shell 'amd-mode 'eslintd-fix 'flycheck 'gulp-task-runner 'indium 'js2-mode 'js2-refactor 'projectile 'xref-js2 'dumb-jump 'doom-themes)

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
(setq nxml-child-indent 4)

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

(add-hook 'js-mode-hook #'setup-js-buffer)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(defun setup-js-buffer ()
  (unless (eq major-mode 'json-mode)
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
  ;; (eslintd-fix-mode)

  ;; add xref-js2 support
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)

  (setq js2-basic-offset 2)
  (setq js2-strict-inconsistent-return-warning nil)
  (setq js2-indent-switch-body t)
  (setq js-switch-indent-offset 2)

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

;; flycheck-rust
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; web-mode
(require 'web-mode)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-attr-indent-offset 4)
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-script-padding 4)
  (setq web-mode-style-padding 4)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode ))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode ))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode ))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode ))
(add-to-list 'auto-mode-alist '("\\.tag\\'" . web-mode ))
(add-to-list 'auto-mode-alist '("\\.riot\\'" . web-mode ))

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
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

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

;; smyx wtf
;; https://stackoverflow.com/questions/11127109/emacs-24-package-system-initializatiton-problems/11140619#11140619
;;(defun init-smyx()
;;  (load-theme 'smyx)
;;  )
;; (add-hook 'after-init-hook 'init-smyx)

;; doom

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nova t)

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
