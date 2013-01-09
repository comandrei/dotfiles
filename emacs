(ido-mode 1)
(show-paren-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)

(require 'yasnippet) ;; not yasnippet-bundle

(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets/")
(add-to-list 'load-path "~/.emacs.d/site-packages")

(require 'zenburn-theme)
(require 'flymake)
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
