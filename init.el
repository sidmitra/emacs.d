;; Initialize packages
;; If you remove (package-initialize), it will be automatically added here by package.el
;; Hence keeping related config here.
(setq package-check-signature nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives
      '(("melpa"       . "https://melpa.org/packages/")
        ("gnu"         . "https://elpa.gnu.org/packages/")
        ("org"         . "https://orgmode.org/elpa/")
        ;; ("gelpa"       . "https://gelpa.gdritter.com/")
        ("marmalade"   . "https://marmalade-repo.org/packages/")))
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
   '("--ignore-dir" "node_modules" "--ignore-dir" "elpa" "--ignore-dir" "lib" "--ignore-dir" "build"))
 '(custom-safe-themes
   '("0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" "0713580a6845e8075113a70275b3421333cfe7079e48228c52300606fa5ce73b" "428754d8f3ed6449c1078ed5b4335f4949dc2ad54ed9de43c56ea9b803375c23" "018c8326bced5102b4c1b84e1739ba3c7602019c645875459f5e6dfc6b9d9437" "fefab1b6d3366a959c78b4ed154018d48f4ec439ce652f4748ef22945ca7c2d5" "728eda145ad16686d4bbb8e50d540563573592013b10c3e2defc493f390f7d83" "071f5702a5445970105be9456a48423a87b8b9cfa4b1f76d15699b29123fb7d8" "36d50e2f9cdb35be79361f5087ac44ce0fa187b6c2cc2056e0370d870a2f5cf9" "e2aa315770b0041d2b1034a6913e5df2029fc869347b407d5f3d1311e347ef7b" default))
 '(package-selected-packages
   '(magit-delta flycheck-posframe org-roam volatile-highlights helm-bufler bufler org-superstar company-posframe helm-posframe org-journal helm-spotify-plus dired-sidebar ibuffer-sidebar vc-msg eyebrowse literate-coffee-mode tree-mode json-navigator jupyter git-gutter helm-fuzzy helm-fuzzier org-sticky-header slack pyvenv dumb-jump edwina centaur-tabs solaire-mode sane-term diff-hl hackernews keyfreq focus ivy virtualenv deadgrep company-lsp lsp-ui elm-yasnippets flycheck-elm elm-mode flutter lsp-dart dart-mode tide paradox heroku esonify flucui-themes wgrep-helm cider clojure-mode string-inflection comment-tags shackle nlinum which-key python-pytest filladapt toml-mode blacken expand-region hl-todo yasnippet-snippets pipenv git-blamed bm vue-mode jinja2-mode json-mode py-autopep8 adaptive-wrap deft company-statistics pyenv-mode dotenv-mode calfw calfw-cal calfw-gcal calfw-ical calfw-org yapfify minimap yasnippet yaml-mode emmet-mode eslint-fix company-web web-mode undo-tree tabbar rainbow-delimiters rainbow-mode py-isort company-anaconda anaconda-mode helm-projectile projectile neotree multi-term mode-icons spaceline powerline flymd markdown-mode magit go-mode flycheck exec-path-from-shell dockerfile-mode company beacon helm-ag ag origami multiple-cursors move-text cursor-in-brackets smooth-scrolling dracula-theme use-package))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
