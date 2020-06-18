;; required for MELPA setup
(require 'package)

;; MELPA setup
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (slime)))
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Slime setup
;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))


;; MY EMACS USAGE CONFIGURATIONS

;; Line numbers
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode t))

;; Theme
;(load "~/emacs-themes/fternoon-theme-source-code.el")
;(load-theme 'afternoon t)

(load-theme 'tango-dark)
(set-background-color "#300028")
;; The default one is too bright.
;; (For swiper-line-face inherited from highlight)
(set-face-attribute 'highlight (selected-frame) :background "#807000")

;; Highlight current line
(require 'hl-line)
(set-face-background 'hl-line "#402040")
(set-face-foreground 'highlight nil)
(global-hl-line-mode 1)

;; verilog setup
(setq verilog-auto-newline nil)

;; Julia setup
(add-to-list 'load-path "/Users/canaknesil/seperate-programs/julia-emacs")
(require 'julia-mode)

;; Auto-fill mode by default to text buffers
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; On the fly spell check for text mode (requires ispell program, e.g. GNU Aspell)
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
		   (flyspell-buffer)
		   (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Font size
(set-face-attribute 'default (selected-frame) :height 140)

;; Hide tool bar
(tool-bar-mode -1)
;; Hide menu bar
(menu-bar-mode 0)
;; Hide scroll bar
(when (display-graphic-p) ;; scroll-bar-mode var is void in terminal
  (scroll-bar-mode -1))
;; Hide minibuffer scroll bar
(set-window-scroll-bars (minibuffer-window) nil nil)

;; Do not open startup GNU Emacs buffer
(setq inhibit-startup-screen t)

;; Byte compile and load the elisp buffer
(global-set-key (kbd "C-c C-c") 'emacs-lisp-byte-compile-and-load)

;; Company completion
(setq company-idle-delay 0)
(add-hook 'after-init-hook 'global-company-mode)

;; AUCTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Aggressive fill paragraph mode
(afp-setup-recommended-hooks)

;; Scroll animation
(load "~/Documents/repos/scroll-animation.el/scroll-animation.el")

;; Start as maximized window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; org-bullets setup
;;(add-to-list 'load-path "/Users/canaknesil/seperate-programs/org-bullets")
;;(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Highigh uncommitted changes on the fringe
(global-diff-hl-mode)
(when (not (display-graphic-p))
  (diff-hl-margin-mode t))

;; Tread camelcase as separate words
(add-hook 'prog-mode-hook 'subword-mode)

;; Confirm closing
(setq confirm-kill-emacs 'y-or-n-p)

;; Dired ls options
(setq-default dired-listing-switches "-alh")

;; Show matching parenthesis
(show-paren-mode t)
(setq show-paren-delay 0.0)

;; All backups to a specific directory
(setq backup-directory-alist `(("." . "~/emacs-tmp")))

;; Switch to new window after creation
(defun hrs/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun hrs/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun delete-window-and-balance ()
  "Balance windows after invoking C-x 0."
  (interactive)
  (delete-window)
  (balance-windows))

(global-set-key (kbd "C-x 2") 'hrs/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'hrs/split-window-right-and-switch)
(global-set-key (kbd "C-x 0") 'delete-window-and-balance)

;; Which-key (display possible keys while typing a command
(which-key-mode)

;; helpful (better help buffers)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

;; ivy, swiper, and counsel setup
(ivy-mode 1)
;; Recommended config for new users
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
;; Key bindings
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

;; Flycheck (on-the-fly syntax check)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; Using pylint for Python syntax checking.
;; Configuration is in ~/.pylintrc

