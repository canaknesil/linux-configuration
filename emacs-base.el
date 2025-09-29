;; Basic Emacs Configuration

;; See emacs-readme.org for more information


;;
;; INITIAL CONFIGURATION
;;

;; HOME and Startup Directories on Windows

;; On Windows, the directory Emacs treats as home defaults to the
;; user-specific application data directory. .emacs is expected to be
;; there. The user-specific application data directory depends on the
;; Windows version. This can be overwritten by defining HOME
;; environment variable.

;; The initial default-directory at Emacs start-up depends on from
;; where it is started. find-file starts from the default directory. I
;; want it to be the home directory unless Emacs is started with
;; visiting a file.

(when (eq system-type 'windows-nt)
  ;; Set home directory correctly independent from weather HOME
  ;; environment variable is defined.
  (setenv "EMACS_HOME" (getenv "Home"))
  (setenv "Home" (getenv "UserProfile"))
  ;; Set the default-directory to home, at the initial buffer.
  (setq default-directory "~/"))


;;
;; EDITING
;;

;; Line numbers
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode t))


;; Theme

;; To load custom theme
;; (load "~/emacs-themes/afternoon-theme-source-code.el")
;; (load-theme 'afternoon t)

(load-theme 'tango-dark)

;; Beware that terminals use a smaller number of colors
;; (list-colors-display). If a non-supported color is entered, it is
;; replaced with the smallest-distanced supported color.

;; The following way of setting custom background is bad. It works
;; with x-display and with daemon but not with terminal display. When
;; running emacs from terminal (emacs -nw)
;; tty-run-terminal-initialization is called after the init file,
;; which sets the background to default via frame-set-background-mode.

;; Custom background (re-evaluate only first 2 lines to overwrite)
;;(setq my-custom-background "#300028")
;;(set-background-color my-custom-background) ;; Only sets current frame.
;; For future frames.
;; (add-hook 'after-make-frame-functions
;; 	  (lambda (frame)
;; 	    (select-frame frame)
;; 	    (set-background-color my-custom-background)))

;; Custom background
;; Try to reduce blue light in the background.
(cond ((daemonp)
       (set-face-background 'default "#181818") ;; daemon in terminal
       (add-hook 'after-make-frame-functions    ;; daemon as window
		 (lambda (frame)
		   (select-frame frame)
		   (when (display-graphic-p)
		     (set-background-color "#181800")))))
      ((not (display-graphic-p)) (set-face-background 'default "#181818")) ;; non-daemon in terminal
      ((display-graphic-p) (set-face-background 'default "#201800")))      ;; non-daemon as window

;; Default text foreground
;; Try to reduce blue light here as well.
;; Default for tango is eeeeec.
(set-face-attribute 'default nil :foreground "#eeeee0")

;; Custom highlight background, default is too bright.
;; swiper-line-face inherits from highlight
(set-face-attribute 'highlight nil :background "#605030")

;; show-paren-mode match background is too bright, not seeing the delimiter.
(set-face-background 'show-paren-match "#980000")
(set-face-foreground 'show-paren-match "#ccccff")

;; isearch/swiper match face too bright
(set-face-background 'isearch "#b04000")

;; Less distracting line numbers (default gray70)
(set-face-attribute 'line-number nil :foreground "#606060")

;; Less distracting mode line (default foreground #2e3436 background #d3d7cf)
(set-face-attribute 'mode-line nil :background "#b0b0a4")


;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#402040")
(set-face-foreground 'highlight nil)


;; Font and its size

;; Font configuration given in Emacs manual
;; (add-to-list 'default-frame-alist (cons 'font font))

(defun configure-font-extra (font)
  ;; Font of key bindings, y-or-n, markdown inline code, etc. inherit
  ;; from fixed-pitch and fixed-pitch-serif. These faces aren't
  ;; affected by the default way of setting fonts recommended in the
  ;; manual. Setting them here.
  (set-face-font 'fixed-pitch font)
  (set-face-font 'fixed-pitch-serif font))

(defun configure-font (font)
  ;; Font configuration recommended in Emacs manual.
  (add-to-list 'default-frame-alist (cons 'font font))

  ;; Extra configuration is required for some cases.  The following
  ;; configuration doesn't work on Windows. Use customize to apply the
  ;; config.
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook
  		(lambda () (configure-font-extra font)))
    (configure-font-extra font)))

;; Usage example (font=Cascadia Code, fontsize=10):
;; (configure-font "Cascadia Code-10")


;; TODO Line wrap for text buffers

;;(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defun turn-on-visual-line-mode ()
  (visual-line-mode 1))

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'prog-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; Visual line mode rebinds the following keys:
;; <remap> <kill-line>              kill-visual-line
;; <remap> <move-beginning-of-line> beginning-of-visual-line
;; <remap> <move-end-of-line>       end-of-visual-line

;; TODO: I want the original bindings of these commands.


;; Unfill paragraph

;; Unfil paragraph, reverse of fill-paragraph.
;; Taken from https://www.emacswiki.org/emacs/UnfillParagraph

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
	;; This would override `fill-column' if it's an integer.
	(emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
(global-set-key (kbd "M-Q") #'unfill-paragraph)


;; Show matching parenthesis
(show-paren-mode t)
(setq-default show-paren-delay 0.0)


;; Matching parentheses

;; Not enabling marentheses matching automatically because I don't
;; like it.

;; (add-hook 'prog-mode-hook (lambda () (electric-pair-local-mode 1)))

;; Another alternative that I previously used is the smartparens
;; package.

;; (use-package smartparens
;;   :config
;;   ;;(require 'smartparens-config) ; Enable default configuration
;;   (add-hook 'prog-mode-hook #'smartparens-mode)
;;   (setq sp-highlight-pair-overlay nil)
;;   (setq sp-highlight-wrap-overlay nil)
;;   (setq sp-highlight-wrap-tag-overlay nil))

;; Implementing a command that inserts two new lines and the closing
;; parenthesis and leaves the point at the indented second
;; line. Neither built-in features or external packages provide this
;; functionality without enabling automatic pair matching.

(setq my-parens-pairs '(("(" . ")")
			("[" . "]")
			("{" . "}")))
(make-variable-buffer-local 'my-parens-pairs)

(defun my-parens-return ()
  (interactive)
  (let ((opening-paren (string (char-before))))
    (if (not opening-paren)
	(message "Char before point is out of range.")
      (let ((paren-pair (assoc opening-paren my-parens-pairs)))
	(if (not paren-pair)
	    (message (format "Opening parenthesis '%s' does not have a match." opening-paren))
	  
	  ;; Everything is ok, perform the job
	  (save-excursion
	    (newline 2)
	    (insert (cdr paren-pair))
	    (indent-for-tab-command))
	  (next-line)
	  (indent-for-tab-command))))))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (define-key prog-mode-map (kbd "C-c RET") #'my-parens-return)
	    (define-key prog-mode-map (kbd "C-c C-<return>") #'my-parens-return)))


;; Display current function's name
(add-hook 'prog-mode-hook 'which-function-mode)


;; Disable beeping

;; Visible bell instead of sound.
;;(setq visible-bell t)

;; Disable bell sound
(setq-default ring-bell-function (lambda ()))


;; On the fly spell check

;; Requires an external spell checker, e.g. aspell.
;; Also requires a dictionary, e.g. aspell-en.

;; On the fly spell check for text 
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
		   ;;(flyspell-buffer) ;; takes too long for large buffers
		   (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))


;; On the fly syntax check and similar

;; Using flymake. It is built-in.

;; Install flake8 python package. Elpy uses it.
;; Flake8 configuration is at linux-configuration/flake8.
;; Copy or link it to ~/.config/flake8
;; The config makes flake8 to ignore several anoying warnings. 


;; Old setup with flycheck.

;;(use-package flycheck
;;  :init (global-flycheck-mode))

;; Using pylint for Python syntax checking.
;; Configuration is in ~/.pylintrc


;; CamelCase as separate words 
(add-hook 'prog-mode-hook 'subword-mode)

;; Note: Opposite of subword-mode is superword-mode.


;; Enable mouse in terminal
(xterm-mouse-mode t)


;; Delete selection when inserting text
(delete-selection-mode 1)


;; Preserve point location when scrolling
(setq-default scroll-preserve-screen-position 1)

;; This doesn't preserve point location within window when pixel
;; scrolling, which is the desired behavior.


;; Scroll margin

;; Leave a number of lines on top or bottom when using C-l.
(setq-default recenter-positions '(middle 2 -2))

;; Automatic scrolling when the point is close to the top or the
;; bottom.
;;(setq-default scroll-margin' 1)

;; Disadvantage: This prevents the point to move from one window to
;; other in follow-mode.

;; Disadvantage: Clicking to the lines at margin triggers
;; autoscrolling, which is distracting.

;; Didadvantage: Mouse wheel scrolling at the end of the buffer
;; triggers auto-scrolling back up.


;; Pixel scrolling
(unless (version< emacs-version "29")
  (pixel-scroll-precision-mode 1))


;; Comment column

;; Make M-; put the comment one space after the current lines text.
(add-hook 'prog-mode-hook
	  (lambda ()
	    (setq comment-column 0)))


;; Hide Scroll bar 

;; Wrong way:
;; because display-graphic-p returns always nil when started as daemon.
;; (when (display-graphic-p) 
;;   (scroll-bar-mode -1))

;; scroll-bar-mode is void when opened in terminal mode. 
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Hide minibuffer scroll bar
(set-window-scroll-bars (minibuffer-window) nil nil)


;; Which-key (display possible keys while typing a command)
(unless (version< emacs-version "30")
  (which-key-mode))


;;
;; WINDOWS
;;

;; Move to, delete, or swap with another window directionally with windmove

;; default key bindings are SHIFT-{left, right, up, down}.
(windmove-default-keybindings)

;; default keybindings are C-x SHIFT-{left, right, up, down}.
(if (version<= "28.1" emacs-version)
    (windmove-delete-default-keybindings))

;; default keybindings are SUPER-{left, right, up, down}.
;; Altering with C-SHIFT-{left, right, up, down}.
(if (version<= "28.1" emacs-version)
    (windmove-swap-states-default-keybindings '(control shift)))


;; Restore window configuration with winner mode
(winner-mode 1)


;; Switch to new window after creation, delete window after killing buffer
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

;; Not needed. C-x 4 0 kill-buffer-and-window does the job.
;; (defun kill-buffer-and-delete-window ()
;;   "Delete current window after killing buffer, if there are more than 1 windows."
;;   (interactive)
;;   (kill-buffer)
;;   (if (> (count-windows) 1)
;;       (delete-window-and-balance)))

(global-set-key (kbd "C-x 2") 'hrs/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'hrs/split-window-right-and-switch)
(global-set-key (kbd "C-x 0") 'delete-window-and-balance)
;;(global-set-key (kbd "C-x j") 'kill-buffer-and-delete-window)


;; Prefer vertical splitting of windows

;; split-height-threshold default is 80.
;; split-width-threshold default is 160.
;; See window-splittable-p for information.
(setq-default split-height-threshold 120)


;; Tab bar (workspaces)
(setq-default tab-bar-show nil)

;; Documentation is insufficient for tab-bar-tab-hints.
;;(setq tab-bar-tab-hints 1)

;; tab-bar-history-mode provides same functionality as
;; winner-mode. Even the key bindings are the same. Using winner-mode.

;;(tab-bar-history-mode 1)


;;
;; FRAMES
;;

;; Start as maximized window
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; Hide tool bar

;; tool-bar-mode may not be defined if Emacs was compiled without GUI support
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))


;; Hide menu bar
(menu-bar-mode 0)


;; Undelete frame
(unless (version< emacs-version "29")
  (undelete-frame-mode 1))


;; Delete local buffers when deleting frames

;; Disagvantage: Deleting buffers when deleting a frame is does not go
;; with the Emacs philosophy that buffers are independent from windows
;; and frames. That's why, doing so breaks some Emacs features. For
;; example, undelete-frame doesn't recreate deleted buffers.

(require 'seq)
(defvar server-clients)

(defun can/delete-buffers-local-to-frame (frame)
  "Delete buffers that were exclusively seen by frame."
  (when (>= (seq-length (frame-list)) 2)
    (let* (;; buffers seen by the frame, except minibuffers,
	   ;; *Messages*, *scrath*.
	   (frame-buffers
	    (seq-filter (lambda (b)
			  (and (not (minibufferp b))
			       (not (string= (buffer-name b) "*Messages*"))
			       (not (string= (buffer-name b) "*scratch*"))))
			(frame-parameter frame 'buffer-list)))

	   ;; buffers opened through the client arguments
	   ;; Client buffers are automatically killed and client is
	   ;; notified at frame deletion.
	   (client-buffers
	    (let ((proc (frame-parameter frame 'client)))
	      (if (and proc (memq proc server-clients))
		  (process-get proc 'buffers)
		nil)))

	   ;; buffers seen exclusively by other frames
	   (other-frames-buffers
	    (cl-reduce #'append
		       (mapcar (lambda (f)
				 (frame-parameter f 'buffer-list))
			       (seq-filter (lambda (f) (not (eq f frame)))
					   (frame-list)))))

	   ;; buffers that won't be deleted
	   (non-local-buffers (append client-buffers other-frames-buffers))

	   ;; buffers that will be deleted
	   (buffers-to-be-deleted
	    (seq-filter (lambda (x) (not (memq x non-local-buffers)))
			frame-buffers)))

      (can/delete-buffers buffers-to-be-deleted))))


(defun can/delete-buffers (buffers-to-be-deleted)
  "Delete buffers in buffers-to-be-deleted. Save beforehand if
   necessary. Prompt when saving."
  (save-some-buffers nil
		     (lambda ()
		       (and (buffer-file-name) ;; file visiting buffer
			    (memq (current-buffer) buffers-to-be-deleted))))

  (let ((killed-buffer-names
	 (seq-filter #'identity
		     (mapcar (lambda (b)
			       (let ((name (buffer-name b))
				     (res (kill-buffer b)))
				 (if res name nil)))
			     buffers-to-be-deleted))))

    (message "Killed buffers `%s'." killed-buffer-names)
    killed-buffer-names))


(defun delete-frame-kill-local-buffers ()
  (interactive)
  (let ((frame (selected-frame)))
    (can/delete-buffers-local-to-frame frame)
    (delete-frame frame)))


;;(add-hook 'delete-frame-functions #'can/delete-buffers-local-to-frame)


;; Delete frame with C-x C-c
(defun can/delete-frame ()
  "Delete frame. If there is only one frame, exit Emacs."
  (interactive)
  (if (>= (seq-length (frame-list)) 2)
      (delete-frame)
    (save-buffers-kill-terminal)))

(global-set-key (kbd "C-x C-c") 'can/delete-frame)


;; Ediff in a single frame
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)


;; TODO Saving sessions

;; Using built-in desktop library

;; Configure desktop library so that Emacs doesn't restore previous
;; session automatically, but the previous session can be restored
;; with a command, in case of a crash or an accidental exit.

;;(desktop-save-mode 1)
;;(setq desktop-restore-frames nil)


;;
;; FILES
;;

;; All backups to a specific directory
(setq-default backup-directory-alist `(("." . "~/emacs-tmp")))


;; Auto-save no message
(setq-default auto-save-no-message t)


;; Recent files

;; ivy-switch-buffer handles this

;; (recentf-mode 1)
;; (setq recentf-max-menu-items 25)
;; (setq recentf-max-saved-items 25)
;; (global-set-key "\C-x\ \M-f" 'recentf-open-files)

;; ;; Save recent file list every 5 min in case of abrupt exit.
;; (defun recentf-save-list-without-message ()
;;   (let ((inhibit-message t))
;;     (recentf-save-list)))

;; (run-at-time nil (* 5 60) 'recentf-save-list-without-message)

;; ;; Exclude internal recentf file.
;; (add-to-list 'recentf-exclude (expand-file-name recentf-save-file))
;; (recentf-cleanup)


;;
;; MISCELLANEOUS
;;

;; Do not open startup GNU Emacs buffer
(setq-default inhibit-startup-screen t)


;; Confirm closing
(setq-default confirm-kill-emacs 'y-or-n-p)


;; Reload config

;; Run .emacs
(defun reload-config ()
  (interactive)
  (load-file "~/.emacs"))


;;
;; PROGRAMMING LANGUAGES & MODES
;;

;; Tree-sitter
(setq treesit-language-source-alist
      '((cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4")
  	(c "https://github.com/tree-sitter/tree-sitter-c" "v0.20.7")))

   
;; Verilog
(setq-default verilog-auto-newline nil)

   
;; Dired

;; Dired ls options
(setq-default dired-listing-switches "-alh")

;; Don't open new dired buffers when traversing
(setq-default dired-kill-when-opening-new-dired-buffer t)

;; Move to trash when deleting
(setq-default delete-by-moving-to-trash t)


;; Vivado XDC

;; Open in Tcl mode
(add-to-list 'auto-mode-alist '("\\.xdc\\'" . tcl-mode))


;; C-like languages

;; Indentation offset for C-like languages.
(setq-default c-basic-offset 3)

;; Indent first function arguments relative to the indentation of the previous line.
(c-set-offset 'arglist-intro '+)

;; Indent closing parenthesis relative to the beginning of the block
(c-set-offset 'arglist-close 0)

;; Indent closing parenthesis of C++ templates relative to the beginning of the block
;; TODO: There is no easy way without defining new indentation functionality

;; Indent case in switch blocks
(c-set-offset 'case-label '+)


;; Compilation buffer

;; Display ANSI colors in *compilation* buffer
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)


;;
;; SHORT-HAND KEY BINDINGS
;;

;; Define key bindings as follows using global-set-key and
;; keymap-global-set, according to the Emacs manual.

;; (global-set-key (kbd "C-z") #'shell)  ;; Emacs 28 or before
;; (keymap-global-set "C-z" #'shell)     ;; Emacs 29 and above

;; (add-hook 'texinfo-mode-hook
;; 	  (lambda ()
;; 	    (keymap-set texinfo-mode-map "C-c p" 'backward-paragraph)  ;; Emacs 29 and above
;; 	    (define-key texinfo-mode-map (kbd "C-c p") 'backward-paragraph)  ;; Emacs 28 and before
;; 	    ))


(defun duplicate-line-and-next-line ()
  (interactive)
  (duplicate-line)
  (next-line))

(global-set-key (kbd "C-c n") #'duplicate-line-and-next-line)


;;
;; Done
;;

(message "emacs-base.el done.")


