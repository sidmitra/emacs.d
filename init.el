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
