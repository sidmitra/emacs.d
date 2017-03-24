(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("org"         . "http://orgmode.org/elpa/")
        ("melpa"       . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))


;; core packages
;; ==============
(setq core-packages
      '(use-package))

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


;; appearence
;; ===========
;; hide welcome message
(setq inhibit-startup-message t)

;; no initial scratch message
(setq initial-scratch-message nil)

;; hide toolbar
(if window-system
    (tool-bar-mode -1))
(if window-system
    (menu-bar-mode -1))

;; switch window to fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Character encodings default to utf-8.
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Color theme
(use-package darktooth-theme
  :ensure t)
(load-theme 'darktooth t)

(use-package color-theme-sanityinc-tomorrow
   :ensure t)
;; (load-theme 'sanityinc-tomorrow-day t)
;; (load-theme 'sanityinc-tomorrow-night t)

;; Soft-wrap lines
(global-visual-line-mode t)

;; line-num-mode
(global-linum-mode t)
;; Linum format to avoid graphics glitches in fringe
(setq-default linum-format " %4d ")

;; show column number
(setq-default column-number-mode t)

;; set font and size
(set-frame-font "Hack 14")
;; change font-size with ctrl + mouse wheel
(global-set-key (vector (list 'control mouse-wheel-down-event)) 'text-scale-increase)
(global-set-key (vector (list 'control mouse-wheel-up-event))   'text-scale-decrease)

;; apply syntax highlighting to all buffers
(global-font-lock-mode t)

;; change cursor from box to bar
(setq-default cursor-type 'bar)

;; smooth scrolling
(use-package smooth-scrolling
  :ensure t)
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

;; Show matching parentheses with 0 delay
(show-paren-mode 1)
(setq-default show-paren-delay 0)

;; Change "yes or no" to "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; disable emacs window disappearing on Ctrl-z
(global-unset-key (kbd "C-z"))

;; turn off ad-handle-definition: `tramp-read-passwd' got redefined
(setq ad-redefinition-action 'accept)


;; Editing
;; ========
;; Enable copy/past-ing from clipboard
(setq select-enable-clipboard t)

;; https://www.reddit.com/r/emacs/comments/30g5wo/the_kill_ring_and_the_clipboard/
(setq save-interprogram-paste-before-kill t)

;; overwrite active region
(delete-selection-mode t)

;; indent new lines
(global-set-key (kbd "RET") 'newline-and-indent)

;; duplicate line
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")


;; simplify whitespace style
(setq-default whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))

;; Delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; do not treat _ as word delimiter
;; (modify-syntax-entry ?_ "w")

;; set default tab char's display width to 4 spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;; make tab key always call a indent command.
(setq-default tab-always-indent t)
;; make tab key call indent command or insert tab character, depending on cursor position
(setq-default tab-always-indent nil)
;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

;; move cursor in brackets
(use-package cursor-in-brackets
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'cursor-in-brackets-mode))

;; move-text
;; (use-package move-line
;;   :ensure t
;;   :config
;;   (move-text-default-bindings))
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))


;; backup
;; =======
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/per-save"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(defun force-backup-of-buffer ()
  "Make a special 'per session' backup at the first save of each Emacs session."
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)


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

;; show current file path
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))
(global-set-key [C-f1] 'show-file-name)


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


;; beacon
;; ========
(use-package beacon
  :ensure t
  :config
  (beacon-mode t))


;; company
;; ========
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above nil
        company-tooltip-limit 10
        company-tooltip-minimum 3
        company-tooltip-margin 1
        company-transformers '(company-sort-by-occurrence)
        company-dabbrev-downcase nil)

  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  )


;; Docker
;; ========
(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile$")


;; shell
;; =====
;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))


;; flycheck
;; =========
(use-package let-alist
  :ensure t)
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-checker-error-threshold 2000)
  (setq-default flycheck-highlighting-mode 'lines)
  (setq-default flycheck-idle-change-delay 3)
  (setq-default flycheck-display-errors-delay 0))
;;(setq-default flycheck-flake8-maximum-line-length 120))


;; golang
;; =======
(use-package go-mode
  :ensure t)


;; helm
;; =====
(use-package helm
  :ensure t
  :config
  ;; replace default find file
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  ;; Use helm-M-x instead, shows keybindings for commands
  (global-set-key (kbd "M-x") 'helm-M-x)

  ;; rebind tab to run persistent action
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; make TAB works in terminal
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  ;; list actions using C-z
  ;; (define-key helm-map (kbd "C-z")  'helm-select-action)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  ;; open helm buffer inside current window, not occupy whole other window
  (setq helm-split-window-in-side-p t)
  ;; move to end or beginning of source when reaching top or bottom of source.
  (setq helm-move-to-line-cycle-in-source t)
  ;; search for library in `require' and `declare-function' sexp.
  (setq helm-ff-search-library-in-sexp  t)
  ;; scroll 8 lines other window using M-<next>/M-<prior>
  (setq helm-scroll-amount 8)
  (setq helm-ff-file-name-history-use-recentf t)

  ;; Make helm window lean
  ;; https://www.reddit.com/r/emacs/comments/2z7nbv/lean_helm_window/
  (setq helm-display-header-line nil) ;; t by default
  (set-face-attribute 'helm-source-header nil :height 0.1)
  (helm-autoresize-mode 1)
  (defun helm-toggle-header-line ()
    (if (= (length helm-sources) 1)
        (set-face-attribute 'helm-source-header nil :height 0.1)
      (set-face-attribute 'helm-source-header nil :height 1.0)))
  (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)

  ;; Enable helm
  (helm-mode 1))


;; magit
;; ======
(use-package magit
  :ensure t
  :config
  (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0"))


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

(use-package flymd
  :ensure t)
(defun my-flymd-browser-function (url)
  (let ((browse-url-browser-function 'browse-url-firefox))
    (browse-url url)))
(setq flymd-browser-open-function 'my-flymd-browser-function)


;; modeline
;; ==========
(use-package powerline
  :ensure t
  :config
  ;;(powerline-center-theme)
  ;;(setq-default powerline-default-separator 'curve)
  )
(use-package spaceline
  :ensure t
  :config
  (progn
    (require 'spaceline-config)
    (setq powerline-default-separator 'slant)
    (setq spaceline-workspace-numbers-unicode t)
    (setq spaceline-separator-dir-left '(left . left))
    (setq spaceline-separator-dir-right '(right . right))
    (setq powerline-height 27)
    (spaceline-toggle-window-number-on)
    (spaceline-toggle-buffer-modified-on)
    (spaceline-toggle-major-mode-on)
    (spaceline-toggle-battery-on)
    (spaceline-toggle-hud-on)
    (spaceline-toggle-projectile-root-on)
    (spaceline-emacs-theme)
    ))


;; mode-icons
;; ===========
(use-package mode-icons
  :ensure t
  :config
  (mode-icons-mode))


;; multi-term
;; ===========
(use-package multi-term
  :ensure t)
(setq multi-term-program "/bin/bash")
(defalias 'term 'multi-term)


;; neotree
;; ========
(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-smart-open t)
  )


;; org
;; ====
(setq-default org-support-shift-select t)
(setq-default org-agenda-files '("~/Projects/Notes/todo.org"))
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "yellow" :weight bold))
        ("DONE" . "green")
        ))


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
    (projectile-mode)))

(use-package helm-projectile
  :ensure t
  :defer t
  :ensure helm-projectile)


;; python-mode
;; ============
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent-offset 4)))

;; anaconda
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  ;; Not sure this works yet
  ;; (when (projectile-project-p)
  ;;   (setenv "PYTHONPATH" (projectile-project-root)))
  )
(use-package company-anaconda
  :ensure t
  :config
  (add-to-list 'company-backends 'company-anaconda))

;; sudo apt install python3-setuptools python3-flake8
;; sudo apt install python3-jedi
(setq-default python-shell-interpreter "python3")

(use-package py-isort
  :ensure t
  :init
  (add-hook 'before-save-hook 'py-isort-before-save))


;; Layout
;; ========
;; (use-package 0blayout
;;   :ensure t
;;   :config
;;   (0blayout-mode 1))


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
(add-to-list 'company-backends 'company-ess)


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
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))


;; shell
;; =======
(defun term-bash (buffer-name)
  "Start a terminal and rename buffer."
  (interactive "sbuffer name: ")
  (ansi-term "/bin/bash")
  (rename-buffer buffer-name t))


;; shortcuts
;; ==========
(set-register ?t (cons 'file "~/Projects/Notes/todo.org"))
(set-register ?j (cons 'file "~/Projects/Notes/journal.org"))
(set-register ?p (cons 'file "~/Projects/Notes/pw/permanent.md"))
(set-register ?i (cons 'file "~/.emacs.d/init.el"))



;; tabbar
;; ======
(use-package tabbar
  :ensure t
  :config
  (tabbar-mode t)
  (setq tabbar-cycle-scope (quote tabs))
  (setq table-time-before-update 0.1)
  (setq tabbar-use-images nil)

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

  ;; ignore all buffers starting with *
  (setq tabbar-buffer-list-function
        (lambda ()
          (remove-if
           (lambda(buffer)
             (find (aref (buffer-name buffer) 0) " *"))
           (buffer-list))))

  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
  (global-set-key (vector (list 'control `tab)) 'tabbar-forward-tab)
  (global-set-key (kbd "C-S-p") 'tabbar-backward-group)
  (global-set-key (kbd "C-S-n") 'tabbar-forward-group)
  (global-set-key (kbd "C-<") 'tabbar-backward)
  (global-set-key (kbd "C->") 'tabbar-forward))


;; tramp
;; ======
;; (setq tramp-default-method "ssh")


;; undo-tree
;; ==========
(use-package undo-tree
  :ensure t)
(global-undo-tree-mode 1)


;; uniquify
;; =========
;; show unique buffer names
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))


;; web
;; ====
(use-package web-mode
  :ensure t
  :mode (
         ("\\.css$" . web-mode)
         ("\\.html$" . web-mode)
         ("\\.js$" . web-mode)
         ("\\.json$" . web-mode)
         ("\\.jsx$" . web-mode)
         ("\\.scss$" . web-mode)
         ("\\.less$" . web-mode))
  :config
  (setq-default indent-tabs-mode nil) ;; no TABS
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-auto-expanding t)

  (add-to-list 'web-mode-comment-formats '("jsx" . "// "))
  (add-hook 'web-mode-hook
            (lambda ()
              (add-to-list 'company-dabbrev-code-modes 'web-mode)))
  (add-hook 'web-mode-hook #'(lambda () (modify-syntax-entry ?_ "w"))))

(use-package company-web
  :ensure t)

;; emmet
(use-package emmet-mode
  :ensure t
  :mode (("\\.html$" . web-mode)))

;; Javascript
;; use eslint with web-mode for jsx files
;; See http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html#emacs_configuration_for_eslint_and_jsx
;; npm install -g eslint babel-eslint eslint-plugin-react
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; js-auto-beautify
(use-package js-auto-beautify
  :ensure t
  :config
  (add-hook 'web-mode 'js-auto-beautify-mode)
  (add-hook 'web-mode (lambda ()
                        (web-mode-set-content-type "jsx"))))



;; yaml-mode
;; =========
(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml$" . yaml-mode))
  :config
  )


;; skewer-mode
;; TODO: Move httpd-root to config per project
;; ============
(use-package simple-httpd
  :ensure t
  :config
  (add-hook 'html-mode-hook 'skewer-html-mode))


;; yasnippet
;; =========
(use-package yasnippet
  :ensure t)
(yas-global-mode 1)
(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate-functions t)))
(add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")


;; My Projects
;; =========
;; Forcing django mode on all html
;; TODO: Better way to do this?
(setq-default web-mode-engines-alist
              '(("django"    . "\\.html\\'")))
(setq-default web-mode-content-types-alist
              '(("jsx" . "\\.js[x]?\\'")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet yaml-mode use-package undo-tree twilight-bright-theme tabbar spaceline soothe-theme solarized-theme smooth-scrolling simple-httpd rainbow-mode rainbow-delimiters py-isort neotree multi-term molokai-theme mode-icons minimap markdown-mode magit js-auto-beautify helm-projectile helm-ag go-mode flymd flycheck exec-path-from-shell ess espresso-theme eslintd-fix eslint-fix emmet-mode dockerfile-mode darktooth-theme cursor-in-brackets company-web company-anaconda color-theme-solarized color-theme-sanityinc-tomorrow color-theme-github beacon anti-zenburn-theme ample-theme ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
