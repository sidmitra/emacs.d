;; Initialize packages
;; If you remove (package-initialize), it will be automatically added here by package.el
;; Hence keeping related config here.
(setq package-check-signature nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives
      '(("melpa"       . "https://melpa.org/packages/")
        ("gnu"         . "https://elpa.gnu.org/packages/")
        ("org"         . "https://orgmode.org/elpa/")
        ("marmalade"   . "https://marmalade-repo.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))


;; straight.el package manager
;; did not work inside the org file
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; Load my custom configuration
(org-babel-load-file "~/.emacs.d/README.org")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(eradio org-journal deft org-roam doct org-pretty-tags org-super-agenda org-superstar dockerfile-mode flymd flycheck-rust cargo rust-mode emmet-mode prettier-js company-web web-mode dotenv-mode toml-mode yaml-mode flycheck-posframe flycheck-prospector pyvenv lsp-mode diff-hl git-timemachine magit-delta browse-at-remote vc-msg forge magit format-all yasnippet-snippets yasnippet better-shell vterm keyfreq undo-tree company-statistics ag ctrlf company-prescient selectrum-prescient prescient selectrum neotree dumb-jump projectile bm which-key-posframe which-key comment-dwim-2 expand-region move-text whole-line-or-region multiple-cursors editorconfig browse-kill-ring mode-icons minions doom-modeline nlinum-hl nlinum eyebrowse rainbow-mode rainbow-delimiters highlight-indent-guides color-identifiers-mode unicode-fonts all-the-icons doom-themes exec-path-from-shell paradox use-package))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
