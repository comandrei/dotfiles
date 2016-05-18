;;; emacs-config --- Custom stuff
;;; Code:
;;; Commentary:
(setq package-enable-at-startup nil)
(package-initialize)
(setq auto-mode-alist (cons '("emacs" . lisp-mode) auto-mode-alist))
(ido-mode 1)
(show-paren-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(setq-default show-trailing-whitespace t)
;; Prerequisite: Emacs >= 24
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

(setq-default indent-tabs-mode nil)

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

;; make more packages available with the package installer
(setq to-install
      '(exec-path-from-shell less-css-mode markdown-mode zenburn-theme jedi smartparens yaml-mode flycheck magit virtualenvwrapper))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sls?\\'" . yaml-mode))

(mapc 'install-if-needed to-install)
;; (require 'yasnippet)
(require 'smartparens)
(show-smartparens-global-mode +1)
(load-theme 'zenburn t)
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/snippets/")

(setq js-indent-level 4)

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "~/.virtualenvs/")

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)
            (jedi:ac-setup)
            (local-set-key "\C-cd" 'jedi:show-doc)
            (local-set-key (kbd "M-SPC") 'jedi:complete)
            (local-set-key (kbd "M-.") 'jedi:goto-definition)))

(defun flycheck-python-set-executables ()
  (let ((exec-path (python-shell-calculate-exec-path)))
    (setq flycheck-python-pylint-executable (executable-find "pylint")
          flycheck-python-pylintrc "~/.pylintrc"
          flycheck-python-flake8-executable (executable-find "flake8")))
  ;; Force Flycheck mode on
  (flycheck-mode))

(defun flycheck-python-setup ()
  (add-hook 'hack-local-variables-hook #'flycheck-python-set-executables
            nil 'local))

(add-hook 'python-mode-hook (lambda ()
                              (hack-local-variables)
                              (when (boundp 'project-venv-name)
                                (venv-workon project-venv-name))))
(add-hook 'python-mode-hook #'flycheck-python-setup)

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer)
                 (not (buffer-modified-p buffer)))
        (set-buffer buffer)
        (revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshed open files"))
(global-set-key [(control shift f5)] 'revert-all-buffers)

(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((project-venv-name . "panda")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
