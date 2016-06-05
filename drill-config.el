;;; emacs
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;; org
(add-to-list 'load-path (expand-file-name "~/opt/org-mode/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)
(require 'org-id)
(setq org-id-method (quote uuidgen))
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
(setq org-startup-indented t)
(setq org-catch-invisible-edits 'error)
(setq org-use-speed-commands t)

;;; org-mem
(load-file "~/code/elisp/my-org-drill/org-mem.el")
