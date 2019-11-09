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
 '(electric-pair-mode t)
 '(package-selected-packages
   (quote
    (origami pyvenv python-black projectile use-package irony modern-cpp-font-lock counsel-etags spaceline rainbow-delimiters fzf racer highlight-symbol undo-tree buffer-move avy workgroups2 zoom elpy company-quickhelp company-lsp exec-path-from-shell lsp-rust cargo flycheck flycheck-rust lsp-mode lsp-ui rust-mode toml-mode company company-jedi magit color-theme-sanityinc-tomorrow counsel swiper ivy))))

;; Always have use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
;; If startup is slow we can comment this out
(setq use-package-always-ensure t)

(defun ensure-package-installed (packages)
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
	 nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
	   (package-install package)
	 package)))
   packages))

(or (file-exists-p package-user-dir)
        (package-refresh-contents))

(ensure-package-installed package-selected-packages)

;; Use M-x describe-face to change the face under your cursor
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "color-234" :foreground "#ccc9ba" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(font-lock-builtin-face ((t (:foreground "#b041ad"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#badfdb" :slant italic))))
 '(font-lock-comment-face ((t (:foreground "#badfdb" :slant italic))))
 '(font-lock-function-name-face ((t (:foreground "#ff8a5c"))))
 '(font-lock-keyword-face ((t (:foreground "#a82037"))))
 '(font-lock-string-face ((t (:foreground "#d1eecc"))))
 '(hl-line ((t (:inherit highlight :background "color-237")))))

;; Generic Configuration
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; globals
(global-linum-mode t) ;; enable line numbers globally
(visual-line-mode)
(global-hl-line-mode)

;; Auto refresh on git branch switches
(global-auto-revert-mode t)

;; spacemacs powerline
(spaceline-spacemacs-theme)

;; Automatic cleanup of old buffers at midnight
(midnight-mode t)
(setq clean-buffer-list-delay-general 1)

;; Ivy configuration
(use-package ivy)
(use-package counsel)
(use-package swiper)
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

;; Move around windows with shift arrow
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Balance buffer size
(setq window-combination-resize t)

;; No tabs
(setq-default indent-tabs-mode nil)

;; avy gotos. Quick jumps
(use-package avy
  :config
  (global-unset-key (kbd "C-j"))
  (global-set-key (kbd "C-j") 'avy-goto-char-timer)
  (global-set-key (kbd "M-g g") 'avy-goto-line))

;; add undo tree
(use-package undo-tree
  :config (global-undo-tree-mode))

;; easy buffer moves
(use-package buffer-move
  :config
  (global-set-key (kbd "<C-S-up>")     'buf-move-up)
  (global-set-key (kbd "<C-S-down>")   'buf-move-down)
  (global-set-key (kbd "<C-S-left>")   'buf-move-left)
  (global-set-key (kbd "<C-S-right>")  'buf-move-right))

;; A bunch copied from here
;; https://gist.github.com/nilsdeppe/7645c096d93b005458d97d6874a91ea9

;; Auto-wrap at 80 characters
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 80)
(turn-on-auto-fill)
;; Disable auto-fill-mode in programming mode
(add-hook 'prog-mode-hook (lambda () (auto-fill-mode -1)))

;; Disable the menu bar since we don't use it, especially not in the
;; terminal
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))

;; Don't ring the bell
(setq ring-bell-function 'ignore)

;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.
(setq x-stretch-cursor t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Origami - Does code folding, ie hide the body of an
;; if/else/for/function so that you can fit more code on your screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package origami
  :ensure t
  :commands (origami-mode)
  :bind (:map origami-mode-map
              ("C-c o :" . origami-recursively-toggle-node)
              ("C-c o a" . origami-toggle-all-nodes)
              ("C-c o t" . origami-toggle-node)
              ("C-c o o" . origami-show-only-node)
              ("C-c o u" . origami-undo)
              ("C-c o U" . origami-redo)
              ("C-c o C-r" . origami-reset)
              )
  :config
  (setq origami-show-fold-header t)
  ;; The python parser currently doesn't fold if/for/etc. blocks, which is
  ;; something we want. However, the basic indentation parser does support
  ;; this with one caveat: you must toggle the node when your cursor is on
  ;; the line of the if/for/etc. statement you want to collapse. You cannot
  ;; fold the statement by toggling in the body of the if/for/etc.
  (add-to-list 'origami-parser-alist '(python-mode . origami-indent-parser))
  :init
  (add-hook 'prog-mode-hook 'origami-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beacon-mode: flash the cursor when switching buffers or scrolling
;;              the goal is to make it easy to find the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beacon
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function beacon-mode "beacon.el"))
  :config
  (beacon-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key: when you pause on a keyboard shortcut it provides
;;            suggestions in a popup buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function which-key-mode "which-key.el"))
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RealGud - https://github.com/realgud/realgud
;; A rewrite of GUD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package realgud
  :ensure t
  :init
  (setenv "TERM" "dumb")
  :config
  (setq realgud:pdb-command-name "python -m pdb"))

;; Enable System Copy with Emacs Copy
(setq x-select-enable-clipboard t)

;; Symbol Navigation
(use-package highlight-symbol
  :ensure t
  :config
  (set-face-attribute 'highlight-symbol-face nil
                      :background "default"
                      :foreground "#FA009A")
  (setq highlight-symbol-idle-delay 0.1)
  (setq highlight-symbol-on-navigation-p t)
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
  (add-hook 'prog-mode-hook
            (lambda ()
              (local-set-key (kbd "M-n") 'highlight-symbol-next)
              (local-set-key (kbd "M-p") 'highlight-symbol-prev)))
  )

;; fzf
(global-set-key (kbd "C-f") 'fzf-git)

;; better backspace
(defun backward-delete-whitespace-to-column ()
  "delete back to the previous column of whitespace, or as much whitespace as possible,
or just one char if that's not possible"
  (interactive)
  (if indent-tabs-mode
      (call-interactively 'backward-delete-char-untabify)
    (let ((movement (% (current-column) tab-width))
          (p (point)))
      (when (= movement 0) (setq movement tab-width))
      (save-match-data
        (if (string-match "\\w*\\(\\s-+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char-untabify (- (match-end 1) (match-beginning 1)))
        (call-interactively 'backward-delete-char-untabify))))))

(defun my-smart-backspace ()
  (interactive)
  (if (string-match "^[[:space:]]*$" (thing-at-point 'line))
      (delete-indentation)
    (backward-delete-char-untabify 1)))

;; (global-set-key (kbd "M-DEL") 'my-smart-backspace)


;; LANGUAGE MODE SETUPS
;; generic
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config (setq flycheck-check-syntax-automatically '(save mode-enable))
  ;; the default value was '(save idle-change new-line mode-enabled)
  )

(use-package company
  :hook (prog-mode . company-mode)
  :config (setq company-tooltip-align-annotations t)
  (setq company-dabbrev-downcase 0)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.5)
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 0.75)
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
  :hook (prog-mode . lsp)
  :config (require 'lsp-clients)
  (setq lsp-auto-guess-root t)
  ;; For now we set lsp-enable-snippit to false because I don't find yasnippet necessary
  ;; We may re-explore this in the future.
  (setq lsp-enable-snippet nil)
  )

(use-package lsp-ui)

;; Color parens by depth
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Auto pair parens
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; Python
(setq python-shell-interpreter "/usr/local/bin/python")
(use-package python-black
  :demand t
  :after python)
(defun my/python-mode-hook ()
  ;; company-jedi isn't showing documentation
  ;; (add-to-list 'company-backends 'company-jedi)
  (use-package elpy
    :ensure t
    :init
    (elpy-enable))
  (local-set-key (kbd "C-c f") 'elpy-goto-definition)
  (local-set-key (kbd "M-.") 'elpy-goto-definition)
  (local-set-key (kbd "C-c F") 'elpy-goto-definition-other-window)
  ;; (setq python-black-on-save-mode t) ;; Don't activate just yet
  )
(add-hook 'python-mode-hook 'my/python-mode-hook)

;; Rust
(use-package toml-mode)
(use-package rust-mode
  :hook (rust-mode . lsp))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Auto format rust
(add-hook 'rust-mode-hook
          (lambda ()
            (global-set-key (kbd "C-c TAB") #'rust-format-buffer)))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'rust-mode-hook
          (lambda ()
            (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)))

(require 'racer)
(with-eval-after-load 'racer
  (let ((map racer-mode-map))
    (define-key map (kbd "C-c f") 'racer-find-definition)
    (define-key map (kbd "C-c F") 'racer-find-definition-other-window)))

;; C++
(setq-default c-basic-offset 4)
(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode t)
