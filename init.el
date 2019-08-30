(require 'package)

;; Check for SSL connections
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; INSTALLING MELPA HERE
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(package-selected-packages
   (quote
    (elpy company-quickhelp company-lsp exec-path-from-shell lsp-rust cargo flycheck flycheck-rust lsp-mode lsp-ui rust-mode toml-mode company company-jedi magit color-theme-sanityinc-tomorrow counsel swiper ivy))))

(require 'use-package)


;; Use M-x describe-face to change the face under your cursor
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#121212" :foreground "#fcf9ea" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(font-lock-builtin-face ((t (:foreground "#b041ad"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#badfdb" :slant italic))))
 '(font-lock-comment-face ((t (:foreground "#badfdb" :slant italic))))
 '(font-lock-function-name-face ((t (:foreground "#ff8a5c"))))
 '(font-lock-keyword-face ((t (:foreground "#49beb7"))))
 '(font-lock-string-face ((t (:foreground "#d1eecc")))))

;; Generic Configuration
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; globals
(global-linum-mode t) ;; enable line numbers globally

;; Ivy configuration
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-f") 'counsel-file-jump)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable) ;; C-h v
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
;; Rebind file name completion to a fuzzy regex builder
(setq ivy-re-builders-alist
      '((read-file-name-internal . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))


;; LANGUAGE MODE SETUPS
;; generic
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package company
  :hook (prog-mode . company-mode)
  :config (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1)
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 0.1)
  (setq company-show-numbers t))

;; Bind numbers when company active to select
;; rebind space for completion
;; but prefer number in autocomplete over selection
(defun ora-company-number ()
  "Forward to `company-complete-number'.

Unless the number is potentially part of the candidate.
In that case, insert the number."
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" company-prefix k)))
    (if (or (cl-find-if (lambda (s) (string-match re s))
                        company-candidates)
            (> (string-to-number k)
               (length company-candidates)))
        (self-insert-command 1)
      (company-complete-number
       (if (equal k "0")
           10
         (string-to-number k))))))

(with-eval-after-load 'company
  (let ((map company-active-map))
    ;; Remap Meta next and prev to ctrl next and prev
    (define-key map (kbd "M-n") nil)
    (define-key map (kbd "M-p") nil)
    (define-key map (kbd "C-n") #'company-select-next)
    (define-key map (kbd "C-p") #'company-select-previous)
    ;; remap number selection to the ora kind
    (mapc (lambda (x) (define-key map (format "%d" x) 'ora-company-number))
	  (number-sequence 0 9))
    (define-key map " " (lambda ()
			  (interactive)
			  (company-abort)
			  (self-insert-command 1)))
    (define-key map (kbd "<return>") nil)
    (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
  ))

(provide 'ora-company)

;; Use LSP
(use-package lsp-mode
  :commands lsp
  :config (require 'lsp-clients))
;; For now we set lsp-enable-snippit to false because I don't find yasnippet necessary
;; We may re-explore this in the future.
(setq lsp-enable-snippet nil)

(use-package lsp-ui)

;; Python
(setq python-shell-interpreter "/usr/local/bin/python")
(defun my/python-mode-hook ()
  ;; company-jedi isn't showing documentation
  ;; (add-to-list 'company-backends 'company-jedi)
  (use-package elpy
    :ensure t
    :init
    (elpy-enable))
  (local-set-key (kbd "C-c f") 'elpy-goto-definition)
  (local-set-key (kbd "C-c F") 'elpy-goto-definition-other-window)
  )
(add-hook 'python-mode-hook 'my/python-mode-hook)

;; Rust

;; (use-package toml-mode)

;; (use-package rust-mode
;;   :hook (rust-mode . lsp))

;; ;; Add keybindings for interacting with Cargo
;; (use-package cargo
;;   :hook (rust-mode . cargo-minor-mode))

;; (use-package flycheck-rust
;;   :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
