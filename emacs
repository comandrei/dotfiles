(setq auto-mode-alist (cons '("emacs" . lisp-mode) auto-mode-alist))
(ido-mode 1)
(show-paren-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)

;; Prerequisite: Emacs >= 24
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)


(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

;; make more packages available with the package installer
(setq to-install
      '(zenburn-theme flymake flymake-cursor magit yasnippet))

(mapc 'install-if-needed to-install)
(require 'yasnippet)
(require 'zenburn-theme)
(require 'flymake)
(require 'flymake-cursor)

(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets/")

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-inplace))
       (local-file (file-relative-name
            temp-file
            (file-name-directory buffer-file-name))))
      (list "flake8"  (list local-file))))
   (add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pyflakes-init)))
(load-library "flymake-cursor")

(add-hook 'python-mode-hook
      (lambda ()
        (unless (eq buffer-file-name nil) (flymake-mode))
        (local-set-key [f2] 'flymake-goto-prev-error)
        (local-set-key [f3] 'flymake-goto-next-error)
        ))

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
