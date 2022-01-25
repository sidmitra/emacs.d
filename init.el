;; Initialize packages
;; If you remove (package-initialize), it will be automatically added here by package.el
;; Hence keeping related config here.
(setq package-check-signature nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives
      '(("org"         . "https://orgmode.org/elpa/")
        ("melpa"       . "https://melpa.org/packages/")
        ("nongnu"         . "https://elpa.nongnu.org/nongnu/")
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

;; Replace use-package with straight
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-protocol 'ssh)
(straight-use-package 'use-package)

;; Load org before we run rest of the config through org-babel
(use-package org)

;; Load my custom configuration
(org-babel-load-file "~/.emacs.d/README.org")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "d268b67e0935b9ebc427cad88ded41e875abfcc27abd409726a92e55459e0d01" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3" "a82ab9f1308b4e10684815b08c9cac6b07d5ccb12491f44a942d845b406b0296" "1ca05bdae217adeb636e9bc5e84c8f1d045be2c8004fafd5337d141d9b67a96f" "aca70b555c57572be1b4e4cec57bc0445dcb24920b12fb1fea5f6baa7f2cad02" default))
 '(package-selected-packages
   '(eradio org-journal deft org-roam doct org-pretty-tags org-super-agenda org-superstar dockerfile-mode flymd flycheck-rust cargo rust-mode emmet-mode prettier-js company-web web-mode dotenv-mode toml-mode yaml-mode flycheck-posframe flycheck-prospector pyvenv lsp-mode diff-hl git-timemachine magit-delta browse-at-remote vc-msg forge magit format-all yasnippet-snippets yasnippet better-shell vterm keyfreq undo-tree company-statistics ag ctrlf company-prescient selectrum-prescient prescient selectrum neotree dumb-jump projectile bm which-key-posframe which-key comment-dwim-2 expand-region move-text whole-line-or-region multiple-cursors editorconfig browse-kill-ring mode-icons minions doom-modeline nlinum-hl nlinum eyebrowse rainbow-mode rainbow-delimiters highlight-indent-guides color-identifiers-mode unicode-fonts all-the-icons doom-themes exec-path-from-shell paradox use-package))
 '(paradox-github-token t)
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
