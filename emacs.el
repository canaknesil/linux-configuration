;; Emacs Configuration (second file)

;; See emacs-readme.org for more information


;;
;; PACKAGE AND USE-PACKAGE
;;

(require 'package)

;; Melpa
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; Install packages if not already available.
(require 'use-package-ensure)
(setq use-package-always-ensure t)


;;
;; EDITING
;;

;; TODO Rainbow delimiters

;; Loading this packages causes the following error:
;; Lisp nesting exceeds 'max-lisp-eval-depth'

;; (use-package rainbow-delimiters
;;   :hook
;;   (prog-mode rainbow-delimiters-mode))


;; highlight-parentheses only works with (), not [] and others.
;;(use-package highlight-parentheses)


;; TODO Highlight indentation

;; highlight-indent-guides doesn't work with empty lines.
;; highlight-indentation has experimental support for empty lines.

;; (use-package highlight-indent-guides
;;   :defer
;;   :init
;;   (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;;   :config
;;   (customize-set-variable 'highlight-indent-guides-method 'character))

;; This is not good, only counts spaces.
;;(add-hook 'prog-mode-hook #'highlight-indentation-mode)
;; Experimental support for blank lines.
;;(setq highlight-indentation-black-lines t)

;; indent-bars package doesn't work properly.
;; (use-package indent-bars
;;   :defer
;;   :hook (prog-mode . indent-bars-mode))


;; Avy
(use-package avy
	     ;; don't defer
	     :config
	     (avy-setup-default) ;; binds avy-isearch
	     :bind
	     (("C-:" . avy-goto-char)
	      ("C-'" . avy-goto-char-2)
	      ("M-g f" . avy-goto-line)
	      ("M-g w" . avy-goto-word-1)))


;; TODO Auto highlight symbol

;; auto-highlight-symbol highlights without selecting and the color is
;; too bright.

;; (use-package auto-highlight-symbol ...)

;; I want this only when the whole symbol is selected (e.g. by double
;; clicking).


;; Move text
(use-package move-text
	     ;; don't defer
	     :config
	     (move-text-default-bindings))


;; TODO Highlight uncommitted changes

;; Higligh uncommitted changes on the fringe
(use-package diff-hl
	     ;; don't defer
	     :config
	     (global-diff-hl-mode))

;; Fringe does not work on terminal.  display-graphic-p returns always
;; nil when started as daemon.  Find a way to enable fringe mode for
;; X, even using as daemon, while still using margin mode with
;; terminal. Maybe set a hook after making frame that checks whether
;; the frame is X or terminal, and act accordingly.


;; Company completion (core only)
(use-package company
	     :defer
	     :hook
	     (after-init . global-company-mode)
	     :config
	     (setq-default company-idle-delay 0)
	     
	     ;; Custom key bindings
	     ;; Assuming C-g is always bound to company-abort.
	     (keymap-set company-active-map "C-n" "C-g C-n")
	     (keymap-set company-active-map "M-n" #'company-select-next)
	     (keymap-set company-active-map "C-p" "C-g C-p")
	     (keymap-set company-active-map "M-p" #'company-select-previous)
	     (keymap-set company-active-map "RET" "C-g <return>")
	     (keymap-set company-active-map "<return>" "C-g <return>")
	     (keymap-set company-active-map "M-RET" #'company-complete-selection)
	     (keymap-set company-active-map "M-<return>" #'company-complete-selection)
	     )


;; Visual fill column
(use-package visual-fill-column
	     :defer)


;;
;; WINDOWS
;;

;; Golden ratio windows

;; (use-package golden-ratio
;;   ;; don't defer
;;   :config
;;   (golden-ratio-mode 1))

;; Disadvantage: Not working with treemacs.

;; It is still good to have it for on-demand activation.
(use-package golden-ratio
	     :defer)


;; Transpose frame
(use-package transpose-frame
	     :defer)


;;
;; PROJECT
;;

;; Dumb Jump (jump to definition)
(use-package dumb-jump
	     :defer
	     :commands dumb-jump-xref-activate
	     :init
	     (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


;; Treemacs
(use-package treemacs
	     :defer
	     :init
	     (with-eval-after-load 'winum
	       (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
	     :config
	     (progn
	       (setq-default
		treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
		treemacs-deferred-git-apply-delay        0.5
		treemacs-directory-name-transformer      #'identity
		treemacs-display-in-side-window          t
		treemacs-eldoc-display                   'simple
		treemacs-file-event-delay                2000
		treemacs-file-extension-regex            treemacs-last-period-regex-value
		treemacs-file-follow-delay               0.2
		treemacs-file-name-transformer           #'identity
		treemacs-follow-after-init               t
		treemacs-expand-after-init               t
		treemacs-find-workspace-method           'find-for-file-or-pick-first
		treemacs-git-command-pipe                ""
		treemacs-goto-tag-strategy               'refetch-index
		treemacs-header-scroll-indicators        '(nil . "^^^^^^")
		treemacs-hide-dot-git-directory          t
		treemacs-indentation                     2
		treemacs-indentation-string              " "
		treemacs-is-never-other-window           nil
		treemacs-max-git-entries                 5000
		treemacs-missing-project-action          'ask
		treemacs-move-forward-on-expand          nil
		treemacs-no-png-images                   nil
		treemacs-no-delete-other-windows         t
		treemacs-project-follow-cleanup          nil
		treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
		treemacs-position                        'left
		treemacs-read-string-input               'from-child-frame
		treemacs-recenter-distance               0.1
		treemacs-recenter-after-file-follow      nil
		treemacs-recenter-after-tag-follow       nil
		treemacs-recenter-after-project-jump     'always
		treemacs-recenter-after-project-expand   'on-distance
		treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
		treemacs-project-follow-into-home        nil
		treemacs-show-cursor                     nil
		treemacs-show-hidden-files               t
		treemacs-silent-filewatch                nil
		treemacs-silent-refresh                  nil
		treemacs-sorting                         'alphabetic-asc
		treemacs-select-when-already-in-treemacs 'move-back
		treemacs-space-between-root-nodes        t
		treemacs-tag-follow-cleanup              t
		treemacs-tag-follow-delay                1.5
		treemacs-text-scale                      nil
		treemacs-user-mode-line-format           nil
		treemacs-user-header-line-format         nil
		treemacs-wide-toggle-width               70
		treemacs-width                           35 ;; default 35
		treemacs-width-increment                 1
		treemacs-width-is-initially-locked       t
		treemacs-workspace-switch-cleanup        nil)

	       ;; The default width and height of the icons is 22 pixels. If you are
	       ;; using a Hi-DPI display, uncomment this to double the icon size.
	       ;;(treemacs-resize-icons 44)

	       (treemacs-follow-mode t)
	       (treemacs-filewatch-mode t)
	       (treemacs-fringe-indicator-mode 'always)
	       (when treemacs-python-executable
		 (treemacs-git-commit-diff-mode t))

	       (pcase (cons (not (null (executable-find "git")))
			    (not (null treemacs-python-executable)))
		 (`(t . t)
		  (treemacs-git-mode 'deferred))
		 (`(t . _)
		  (treemacs-git-mode 'simple)))

	       (treemacs-hide-gitignored-files-mode nil))
	     :bind
	     (:map global-map
		   ("M-0"       . treemacs-select-window)
		   ("C-x t 1"   . treemacs-delete-other-windows)
		   ("C-x t t"   . treemacs)
		   ("C-x t d"   . treemacs-select-directory)
		   ("C-x t B"   . treemacs-bookmark)
		   ("C-x t C-t" . treemacs-find-file)
		   ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

;; (use-package treemacs-projectile
;;   :after (treemacs projectile)
;;   :ensure t)

(use-package treemacs-icons-dired
	     :hook (dired-mode . treemacs-icons-dired-enable-once)
	     :ensure t)

;; (use-package treemacs-magit
;;   :after (treemacs magit)
;;   :ensure t)

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

;; (use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
;;   :after (treemacs)
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Tabs))


;;
;; Minibuffer
;;

;; Minibuffer completion

;; Builtin alternatives are icomplete mode and fido mode.
;;(icomplete-mode 1)
;;(icomplete-vertical-mode 1)


;; Ivy: a generic completion mechanism for Emacs.
;; Replaces completing-read-function with ivy-completing-read.

(use-package ivy
	     ;; don't defer
	     :config
	     (ivy-mode 1)
	     ;; Recommended config for new users
	     (setq-default ivy-use-virtual-buffers t)
	     (setq-default ivy-count-format "(%d/%d) ")
	     ;; Additional config from me
	     (setq-default ivy-height 20)
	     (setq-default ivy-fixed-height-minibuffer t)
	     (setq-default ivy-re-builders-alist
			   '((t . ivy--regex-ignore-order))) ; ignore order of space separated words.

	     ;; ivy current match face is difficult to read
	     (set-face-foreground 'ivy-current-match "#ffffff")
	     (set-face-background 'ivy-current-match "#4060d0")
	     )


;; Swiper: an Ivy-enhanced alternative to Isearch.

;; Disadvantage: Not interoperable with other search commands. For
;; example, query-replace or occur can be run from isearch minibuffer,
;; while not from swiper.

;; Disadvantage: Doesn't support all isearch features, such as isearch
;; word and symbol search.

(use-package swiper
	     :defer
	     :bind (("C-s" . 'swiper-isearch)
		    ("C-r" . 'swiper-isearch-backward)
		    ("M-s ." . 'swiper-isearch-thing-at-point))
	     )


;; Counsel: a collection of Ivy-enhanced versions of common Emacs
;; commands.

;; Ivy alone seems enough. Amx package is used to show keybindings
;; with M-x.

;; (use-package counsel
;;   :defer
;;   :bind (("M-x" . 'counsel-M-x)
;; 	 ("C-x C-f" . 'counsel-find-file)
;; 	 ("M-y" . 'counsel-yank-pop)
;; 	 ("C-x b" . 'ivy-switch-buffer)
;; 	 ("C-c v" . 'ivy-push-view)
;; 	 ("C-c V" . 'ivy-pop-view)))


;; Show keybindings with M-x
(use-package amx
	     ;; don't defer
	     :config
	     (amx-mode 1))


;;
;; MISCELLANEOUS
;;

;; Helpful (better help buffers)

;; helpful is not good at window choice to display *helpful* buffers.

(use-package helpful
	     :defer
	     ;;(global-set-key (kbd "C-h f") #'helpful-callable)
	     ;;(global-set-key (kbd "C-h v") #'helpful-variable)
	     ;;(global-set-key (kbd "C-h k") #'helpful-key)
	     )


;; ESUP - Emacs Start Up Profiler
(use-package esup
	     :defer
	     :pin melpa)


;;
;; PROGRAMMING LANGUAGES & MODES
;;

;; Julia

;; Old way
;;(add-to-list 'load-path "/Users/canaknesil/seperate-programs/julia-emacs")
;;(require 'julia-mode)

;; New way
(use-package julia-mode
	     :defer)


;; Rust
(use-package rust-mode
	     :defer)


;; TODO LaTeX

;; Built-in alternative: Tex mode
;; Is Tex mode enough?

;; Make it work.

;; IDE for Latex
;; (use-package auctex
;;   :defer
;;   :config
;;   (setq TeX-auto-save t)
;;   (setq TeX-parse-self t)
;;   (setq-default TeX-master nil))

;; (use-package auctex-latexmk
;;   :defer) ;; latexmk support for auctex


;; Org Mode
(use-package org-bullets
	     :defer
	     :commands org-bullets-mode
	     :init
	     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   ;;(R . t)
   ;;(python . t)
   ;;(shell . t)
   ))

(setq-default org-babel-python-command "python -q")


;; Markdown
(use-package markdown-mode
	     :defer)


;; Python
(use-package elpy
	     :defer
	     :config
	     (elpy-enable)
	     ;; Fall back to completion via shell.
	     ;;(setq elpy-get-info-from-shell t)
	     )

;; Run M-x elpy-config
;; From there you can install necessary packages for elpy.
;; Packages automatically installed at RPC virtual environment. 


;; Arduino
(use-package flycheck
	     :defer) ;; arduino-mode gives error if flycheck is not found.
(use-package arduino-mode
	     :defer
	     :after flycheck)
(use-package company-arduino
	     :defer)


;; OpenSCAD
(use-package scad-mode
	     :defer)


;; Powershell
(use-package powershell
	     :defer)


;; Matlab
(use-package matlab-mode
	     :defer)


;; Haskell
(use-package haskell-mode
	     :defer)


;; CMake
(use-package cmake-mode
	     :defer)


;;
;; SHORTCUT COMMANDS
;;


;;
;; EMACS SERVER
;;

(require 'server)
(unless (and (fboundp 'server-running-p) (server-running-p))
  (server-start))


;;
;; DONE
;;

(message "emacs.el done.")

