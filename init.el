(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("org"         . "http://orgmode.org/elpa/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")
        ("melpa"       . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))


;; core packages
;; ==============
(setq core-packages
      '(
        use-package
        ))

(defun ensure-packages (packages)
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package))))

(ensure-packages core-packages)
(require 'use-package)


;; globals
;; ========
(use-package server
  :if window-system
  :init
      (server-start))

;; hide welcome message
(setq inhibit-startup-message t)
;; no initial scratch message
(setq initial-scratch-message nil)

;; switch window to fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; hide toolbar
(if window-system
    (tool-bar-mode -1)
  )

;; twilight theme
(use-package twilight-theme
  :ensure t)
;; (load-theme 'twilight t)
(use-package darktooth-theme
  :ensure t)
(load-theme 'darktooth t)
(setq-default color-theme-is-global t)

;; Soft-wrap lines
(global-visual-line-mode t)

;; line-num-mode
(global-linum-mode t)
;; Linum format to avoid graphics glitches in fringe
(setq-default linum-format " %4d ")

;; show column number
(setq-default column-number-mode t)

;; change cursor from box to bar
(setq-default cursor-type 'bar)

;; simplify whitespace style
(setq-default whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))

;; do not treat _ as word delimiter
(modify-syntax-entry ?_ "w")

;; set default tab char's display width to 4 spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;; make tab key always call a indent command.
(setq-default tab-always-indent t)
;; make tab key call indent command or insert tab character, depending on cursor position
(setq-default tab-always-indent nil)
;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

;; set font and size
(set-default-font "Inconsolata 14")
;; change font-size with ctrl + mouse wheel
(global-set-key (vector (list 'control mouse-wheel-down-event)) 'text-scale-increase)
(global-set-key (vector (list 'control mouse-wheel-up-event))   'text-scale-decrease)

;; smooth scrolling
;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
;; (setq redisplay-dont-pause t
;;   scroll-margin 1
;;   scroll-step 1
;;   scroll-conservatively 10000
;;   scroll-preserve-screen-position 1)

;; overwrite active region
(delete-selection-mode t)

;; Change "yes or no" to "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show matching parentheses with 0 delay
(show-paren-mode 1)
(setq show-paren-delay 0)

;; indent new lines
(global-set-key (kbd "RET") 'newline-and-indent)

;; Character encodings default to utf-8.
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; apply syntax highlighting to all buffers
(global-font-lock-mode t)

;; Enable copy/past-ing from clipboard
(setq x-select-enable-clipboard t)

;; buffers
;; =======
 (defun volatile-kill-buffer ()
   "Kill current buffer unconditionally."
   (interactive)
   (let ((buffer-modified-p nil))
     (kill-buffer (current-buffer))))

 (global-set-key (kbd "C-x k") 'volatile-kill-buffer)

;; auto revert buffer every x seconds
(global-auto-revert-mode t)


;; packages
;; ========
;; ag
;; =========
(use-package ag
  :ensure t)
(use-package helm-ag
  :ensure t)
(defun projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))


;; company
;; ========
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))


;; emms
;; =====
(use-package emms
  :ensure t
  :config
  (progn
    (emms-standard)
    (emms-default-players))
  :bind (("<f5>" . emms-shuffle)
         ("<f6>" . emms-pause)
         ("<f7>" . emms-previous)
         ("<f8>" . emms-next)
         ("<f9>" . emms-volume-lower)
         ("<f10>" . emms-volume-raise)))


;; flycheck
;; =========
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-checker-error-threshold 2000)
  (setq-default flycheck-highlighting-mode 'lines)
  (setq-default flycheck-idle-change-delay 3)
  (setq-default flycheck-display-errors-delay 0))


;; helm
;; =====
(use-package helm
  :ensure t)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;; https://www.reddit.com/r/emacs/comments/2z7nbv/lean_helm_window/
(setq helm-display-header-line nil) ;; t by default
(set-face-attribute 'helm-source-header nil :height 0.1)
(helm-autoresize-mode 1)
(defun helm-toggle-header-line ()
  (if (= (length helm-sources) 1)
      (set-face-attribute 'helm-source-header nil :height 0.1)
    (set-face-attribute 'helm-source-header nil :height 1.0)))
(add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)

;; html
;; ====
(use-package web-mode
  :ensure t
  :mode (("\\.html$" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-enable-current-element-highlight t))

(add-hook 'sgml-mode-hook
          (lambda ()
            ;; Default indentation to 2, but let SGML mode guess, too.
            (set (make-local-variable 'sgml-basic-offset) 4)
            (setq indent-tabs-mode nil)
            (sgml-guess-indent)))
;; Forcing django mode on all html
;; TODO: Better way to do this?
(setq web-mode-engines-alist
      '(("django"    . "\\.html\\'"))
      )


;; magit
;; ======
(use-package magit
  :ensure t)

;; markdown
;; =========
(use-package markdown-mode
  :ensure t)
(add-hook 'markdown-mode-hook
            (lambda ()
              (when buffer-file-name
                (add-hook 'after-save-hook
                          'check-parens
                          nil t))))

;; multi-term
;; ===========
(use-package multi-term
  :ensure t)
(setq multi-term-program "/bin/bash")
(defalias 'term 'multi-term)


;; org
;; ====
(setq-default org-support-shift-select t)


;; projectile
;; ===========
(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)
    (projectile-global-mode)))

(use-package helm-projectile
   :ensure t
   :defer t
   :ensure helm-projectile)


;; python-mode
;; ============
(use-package pony-mode
  :ensure t)
(add-hook 'python-mode-hook 'pony-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent-offset 4)))

;; anaconda
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))
(use-package company-anaconda
  :ensure t
  :config
  (add-to-list 'company-backends 'company-anaconda))
;; TODO: Figure out pyenv
(use-package pyenv-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'pyenv-mode))

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name.
Version must be already installed."
  (pyenv-mode-set (projectile-project-name)))
(add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)


;; pep8
;; @manual: sudo apt-get install pep8 python-autopep8 python-flake8 pylint
(use-package python-pep8
 :ensure t)
(use-package py-autopep8
 :ensure t)
(setq py-autopep8-options '("--ignore=E309,"))

;; powerline
(use-package powerline
 :ensure t)
(powerline-center-theme)

;; R and ESS
;; ===========
(use-package ess
  :ensure t
  :commands R
  :init (progn
          ;; TODO: why doesn't use-package require it for us?
          (require 'ess)

          (setq ess-eval-visibly-p nil
                ess-use-tracebug t
                ;; ess-use-auto-complete t
                ess-help-own-frame 'one
                ess-ask-for-ess-directory nil)
          (setq-default ess-dialect "R")
          ))
          ;; (ess-toggle-underscore t)))


;; rainbow-mode
;; =============
(use-package rainbow-mode
  :ensure t
  :defer t
  :init
  (setq rainbow-html-colors-major-mode-list '(css-mode
                                              html-mode
                                              less-css-mode
                                              nxml-mode
                                              php-mode
                                              sass-mode
                                              scss-mode
                                              web-mode
                                              xml-mode))
  (dolist (mode rainbow-html-colors-major-mode-list)
    (add-hook (intern (format "%s-hook" mode)) 'rainbow-mode)))


;; rainbow-delimiters
;; ===================
(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    )
  )


;; shortcuts
;; ==========
(set-register ?t (cons 'file "~/Sync/Notes/todo.txt"))


;; tabbar
;; ======
(use-package tabbar
  :ensure t)
(tabbar-mode t)

;; define all tabs to be one of 3 possible groups: “Emacs Buffer”, “Dired”, “User Buffer”.
(defun tabbar-buffer-groups ()
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs Buffer"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    (t
     "User Buffer"
     )
    )))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
(global-set-key (vector (list 'control `tab)) 'tabbar-forward-tab)
(global-set-key (kbd "C-S-p") 'tabbar-backward-group)
(global-set-key (kbd "C-S-n") 'tabbar-forward-group)
(global-set-key (kbd "C-<") 'tabbar-backward)
(global-set-key (kbd "C->") 'tabbar-forward)

;; https://zhangda.wordpress.com/2012/09/21/tabbar-mode-rocks-with-customization/
(setq tabbar-background-color "#959A79") ;; the color of the tabbar background
(custom-set-faces
 '(tabbar-default ((t (:inherit variable-pitch :background "#959A79" :foreground "black" :weight bold))))
 '(tabbar-button ((t (:inherit tabbar-default :foreground "dark red"))))
 '(tabbar-button-highlight ((t (:inherit tabbar-default))))
 '(tabbar-highlight ((t (:underline t))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "#95CA59"))))
 '(tabbar-separator ((t (:inherit tabbar-default :background "#95CA59"))))
 '(tabbar-unselected ((t (:inherit tabbar-default)))))

(setq tabbar-cycle-scope (quote tabs))
(setq table-time-before-update 0.1)
(setq tabbar-use-images t)


;; tramp
;; =======
(setq tramp-default-method "ssh")


;; undo-tree
;; ==========
(require 'undo-tree)
(global-undo-tree-mode 1)


;; uniquify
;; =========================
;; show unique buffer names
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))


;; visual-regexp
;; =============
;;(use-package visual-regexp
;;   :ensure t)
;;(use-package visual-regexp-steroids
;;   :ensure t)


;; window-purpose
;; ===============
(use-package window-purpose
  :ensure t
  :config
  (purpose-mode 1)
  (setq purpose-user-mode-purposes
      '((term-mode . terminal)
        (shell-mode . terminal)
        (ansi-term-mode . terminal)
        (multi-term . terminal)
        (python-mode . coding)
        (lisp-mode . coding)
        (org-mode . coding)
        (web-mode . coding)))
  (purpose-compile-user-configuration)  )


;; yasnippet
;; =========
(use-package yasnippet
  :ensure t)
(yas-global-mode 1)
(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate t)))

;;;;; =============== EXPLORE ====================
;(require 'discover)
