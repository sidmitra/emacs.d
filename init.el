;; Initialize packages
;; If you remove (package-initialize), it will be automatically added here by package.el
;; Hence keeping related config here.
(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("org"         . "http://orgmode.org/elpa/")
        ("melpa"       . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Load my custom configuration
(org-babel-load-file "~/.emacs.d/configuration.org")

;; Automatically added
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments
   (quote
    ("--ignore-dir" "node_modules" "--ignore-dir" "elpa" "--ignore-dir" "lib" "--ignore-dir" "build")))
 '(custom-safe-themes
   (quote
    ("e2aa315770b0041d2b1034a6913e5df2029fc869347b407d5f3d1311e347ef7b" default)))
 '(package-selected-packages
   (quote
    (yasnippet-snippets pipenv cov git-blamed bm vue-mode jinja2-mode json-mode py-autopep8 adaptive-wrap deft company-statistics pyenv-mode dotenv-mode calfw calfw-cal calfw-gcal calfw-ical calfw-org yapfify minimap yasnippet yaml-mode emmet-mode eslint-fix company-web web-mode undo-tree tabbar rainbow-delimiters rainbow-mode py-isort company-anaconda anaconda-mode helm-projectile projectile neotree multi-term mode-icons spaceline powerline flymd markdown-mode magit go-mode flycheck exec-path-from-shell dockerfile-mode company beacon helm-ag ag origami multiple-cursors move-text cursor-in-brackets smooth-scrolling dracula-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
