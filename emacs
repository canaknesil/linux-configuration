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

;; Highlight current line
(require 'hl-line)
(set-face-background 'hl-line "#000020")
(set-face-foreground 'highlight nil)
(global-hl-line-mode 1)

;; verilog setup
(setq verilog-auto-newline nil)

;; Julia setup
(add-to-list 'load-path "/Users/canaknesil/seperate-programs/julia-emacs")
(require 'julia-mode)

;; Auto-fill mode by default to text buffers
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; On the fly spell check for text mode
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

