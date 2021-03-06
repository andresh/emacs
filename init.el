
;; ------> place ui-based customizations to a separate file
(setq custom-file "~/.emacs.d/lisp/custom.el")
(load custom-file 'noerror)

;; ------> ensure that required packages are installed
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar required-packages
  '(jedi jinja2-mode smartparens expand-region fsharp-mode git-timemachine)
  "Packages which should be installed upon launch")

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; ------> add load paths
(let ((load-base "~/.emacs.d/lisp/"))
  (mapcar (lambda (x)
            (add-to-list 'load-path x))
          (cons load-base (append
                     (directory-files load-base t "[^\.]$")))))

;; ------> library imports and configuration

;; slime
;;(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
;;(setq common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec/")
;;(setq slime-backend "~/.emacs.d/lisp/slime-2013-12-09/swank-loader.lisp")
;;(require 'slime)
;;(slime-setup '(slime-fancy slime-autodoc slime-asdf slime-banner slime-indentation))

;; quack
;; (require 'quack)
;; (setq quack-default-program "scheme")
;; (setq quack-run-scheme-always-prompts-p nil)

;; smartparens
(require 'smartparens)
(smartparens-global-mode 1)
(setq sp-highlight-pair-overlay nil) ;; do not color the pairs
(setq sp-autoescape-string-quote nil) ;; turn off autoescaping of quotes inside strings
;; turn off quote and backquote pairs in lisp
(sp-local-pair 'lisp-mode "'" nil :actions nil)
(sp-local-pair 'lisp-mode "`" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
(sp-local-pair 'slime-repl-mode "'" nil :actions nil)
(sp-local-pair 'slime-repl-mode "`" nil :actions nil)

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/auto-complete.el/ac-dict")
(ac-config-default)
(setq ac-auto-start 4)
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-auto-show-menu 0.8)

;; set autocomplete bindings to control the completion menu
(define-key ac-menu-map "\C-t" 'ac-next)
(define-key ac-menu-map "\C-c" 'ac-previous)
(define-key ac-menu-map "\C-n" nil) ;; remove the default binding
(define-key ac-menu-map "\M-n" nil) ;; remove the default binding

;; jedi (python autocompletion library, uses auto-complete)
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method nil)
(setq jedi:get-in-function-call-delay 0)

;; other
(autoload 'jinja2-mode "jinja2-mode")
(require 'expand-region)
(setq expand-region-fast-keys-enabled nil)
(require 'control-lock)
(control-lock-keys)
(autoload 'forward-to-word "misc" t)


;; ------> custom functions

(defun fsharp-compile-and-run ()
  (interactive)
  (let ((name (buffer-file-name)))
    (cond ((string-match "^\\(.*\\)\\.fsi$" name) (compilation-start (concat "mono " (match-string 1 name) ".exe")))
          ((string-match "^\\(.*\\)/[^/]+\\.fs$" name)
           (let ((default-directory (concat (match-string 1 name) "/bin/Debug/")))
             (compilation-start (concat "mono " default-directory (car (file-expand-wildcards "*.exe")))))))))

(defmacro call-unless (&key default &rest clauses)
  "Call DEFAULT unless one of the CLAUSES returns t"
  `(lambda ()
     (interactive)
     (or ,@clauses (call-interactively ,default))))

(defmacro call-if-major-mode (fn &rest modes)
  "If currently active major mode is listed in MODES then call FN and
return t"
  `(when (member major-mode (list ,@modes))
    (call-interactively ,fn)
    t))

(defun mark-str-or-pairs ()
  (interactive)
  (if (er--point-inside-string-p)
      (call-interactively 'er/mark-inside-quotes)
    (call-interactively 'er/mark-inside-pairs)))

(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun scheme-eval-last-and-run ()
  (interactive)
  (let (w)
    (setq w (get-buffer-window))
    (call-interactively 'scheme-send-last-sexp)
    (call-interactively 'run-scheme)
    (select-window w)))

;; (defun ess-eval-region-or-buffer ()
;;   (interactive)
;;   (let (w)
;;     (setq w (get-buffer-window))
;;     (if (and transient-mark-mode mark-active)
;;         (call-interactively 'ess-eval-region-and-go)
;;       (call-interactively 'ess-eval-buffer-and-go))
;;     (select-window w)))

(defun upcase-previous-word ()
  "like upcase-word but moving over the words backwards"
  (interactive)
  (backward-word)
  (save-excursion
    (upcase-word 1)))

(defun sudo-edit ()
  "Edit currently visited file as root."
  (interactive)
  (when (buffer-file-name)
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; courtesy of emacsredux.com
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (message "h")
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(defun my-jedi-goto-definition ()
  (interactive)
  (set-mark (point))
  (call-interactively 'jedi:goto-definition))

(defun python-send-region-or-buffer ()
  "if region is active then send it, otherwise send the current buffer"
  (interactive)
  (if (and transient-mark-mode mark-active)
      (python-shell-send-region (mark) (point))
    (python-shell-send-buffer)))

(defun my-forward-word ()
  "leaves the cursor at the start of the word, also stops at newlines"
  (interactive)
  (if (eolp)
      (forward-char)
    (goto-char (min (save-excursion (forward-to-word 1) (point))
                    (line-end-position)))))

(defun my-backward-word ()
  "same as the default, except also stops at newlines"
  (interactive)
  (if (bolp)
      (backward-char)
    (goto-char (max (save-excursion (backward-word) (point))
                    (line-beginning-position)))))

(defun backward-kill-word-or-region ()
  "if region is active then kill it, else backward kill word"
  (interactive)
  (if (and transient-mark-mode mark-active)
      (kill-region (mark) (point))
    (if (bolp)
        (backward-delete-char 1)
      (kill-region (save-excursion (my-backward-word) (point))
                   (point)))))

(defun apply-to-line-or-region (f)
  "If region is selected then just calls f, otherwise selects the
current line and then calls f"
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively f)
    (save-excursion
      (mark-line)
      (call-interactively f))))

(defun mark-line ()
  (interactive)
  (back-to-indentation)
  (set-mark (point))
  (end-of-line))

(defun my-open-line ()
  "open and indent a line below, indent the current line"
  (interactive)
  (newline-and-indent)
  (beginning-of-line)
  (backward-char)
  (indent-for-tab-command))

(defun tag-word-or-region (tag)
    "Surround current word or region with a given tag."
    (interactive "sEnter tag (without <>): ")
    (when (> (mark) (point))
      (let (markpos) (setq markpos (mark))
           (set-mark (point))
           (goto-char markpos)))
    (let (pos1 pos2 bds start-tag end-tag)
        (setq start-tag (concat "<" tag ">"))
        (setq end-tag (concat "</" tag ">"))
        (if (and transient-mark-mode mark-active)
            (progn
                (goto-char (region-end))
                (insert end-tag)
                (goto-char (region-beginning))
                (insert start-tag))
            (progn
                (setq bds (bounds-of-thing-at-point 'symbol))
                (goto-char (cdr bds))
                (insert end-tag)
                 (goto-char (car bds))
                 (insert start-tag)))))

(defun open-block ()
  "add the colon for python compound statements in case in python-mode,
then open a new line"
  (interactive)
  (back-to-indentation)
  (if (and (equal (symbol-value 'major-mode) 'python-mode)
           (member (thing-at-point 'word)
                   '("def" "for" "while" "class" "if" "try"
                     "with" "else" "elif" "try" "except" "finally")))
      (progn
        (end-of-line)
        (when (not (looking-back ":" 1)) (insert ":"))
        (newline-and-indent))
    (end-of-line)
    (newline-and-indent)))

(defun my-compile ()
  "Use compile to run python programs"
  (interactive)
  (save-buffer)
  (compile (concat "python " (buffer-name))))

;; ------> global re-mappings
(define-key (current-global-map) [remap backward-kill-word]
  'backward-kill-word-or-region)
(define-key (current-global-map) [remap newline] 'newline-and-indent)
(define-key (current-global-map) [remap backward-word] 'my-backward-word)
(define-key (current-global-map) [remap forward-word] 'my-forward-word)
(define-key (current-global-map) [remap comment-region]
  '(lambda ()
     (interactive)
     (apply-to-line-or-region 'comment-region)))
(define-key (current-global-map) [remap uncomment-region]
  '(lambda ()
     (interactive)
     (apply-to-line-or-region 'uncomment-region)))

;; ------> keybindings for Dvorak layout
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings always override major modes."
  t " my-keys" 'my-keys-minor-mode-map)

;; re-map C-x and C-c
(keyboard-translate ?\C-m ?\C-x)
(keyboard-translate ?\C-p ?\C-c)
(keyboard-translate ?\C-c ?\C-p)

;; isearch mode bindings
(define-key isearch-mode-map "\C-n" 'isearch-repeat-forward)
(define-key isearch-mode-map "\C-h" 'isearch-repeat-backward)
(define-key isearch-mode-map "\C-t" 'isearch-exit)
(define-key isearch-mode-map "\C-d" 'isearch-delete-char)
(define-key isearch-mode-map "\C-b" 'isearch-edit-string)

;; brackets ;;TODO: move to .Xmodmap
(define-key my-keys-minor-mode-map (kbd "C-9") "[")
(define-key my-keys-minor-mode-map (kbd "C-0") "]")
(define-key my-keys-minor-mode-map (kbd "M-9") "{")
(define-key my-keys-minor-mode-map (kbd "M-0") "}")

;; cursor movement/navigation
(define-key my-keys-minor-mode-map "\C-h" 'backward-char)
(define-key my-keys-minor-mode-map "\M-h" 'backward-word)
(define-key my-keys-minor-mode-map "\C-n" 'forward-char)
(define-key my-keys-minor-mode-map "\M-n" 'forward-word)
(define-key my-keys-minor-mode-map "\M-c"
  (call-unless :default 'backward-paragraph
               (call-if-major-mode 'outline-previous-visible-heading 'org-mode)))

(define-key my-keys-minor-mode-map "\C-t"
  (call-unless :default 'next-line
               (call-if-major-mode 'slime-repl-next-input 'slime-repl-mode)
               (call-if-major-mode 'comint-next-input 'comint-mode)))

(define-key my-keys-minor-mode-map "\C-p"
  (call-unless :default 'previous-line
               (call-if-major-mode 'slime-repl-previous-input 'slime-repl-mode)
               (call-if-major-mode 'comint-previous-input 'comint-mode)))

(define-key my-keys-minor-mode-map "\M-t"
  (call-unless :default 'forward-paragraph
               (call-if-major-mode 'outline-next-visible-heading 'org-mode)))

(define-key my-keys-minor-mode-map "\C-a" 'smarter-move-beginning-of-line)
(define-key my-keys-minor-mode-map (kbd "C-x C-n") 'other-window)
(define-key my-keys-minor-mode-map (kbd "C-x C-t") 'switch-to-other-buffer)

;; editing
(define-key my-keys-minor-mode-map (kbd "C--") 'mark-line)
(define-key my-keys-minor-mode-map "\M-b" 'backward-kill-word-or-region)
(define-key my-keys-minor-mode-map "\C-b" 'backward-kill-word-or-region)
(define-key my-keys-minor-mode-map "\C-f" 'er/expand-region)
(define-key my-keys-minor-mode-map "\M-f" 'er/contract-region)
(define-key my-keys-minor-mode-map "\C-o" 'my-open-line)
(define-key my-keys-minor-mode-map "\C-v" 'yank)
(define-key my-keys-minor-mode-map "\M-v" 'yank-pop)
(define-key my-keys-minor-mode-map (kbd "C-x r") 'replace-string)
(define-key my-keys-minor-mode-map "\M-l" 'recenter-top-bottom)
(define-key my-keys-minor-mode-map (kbd "<C-return>") 'open-block)
(define-key my-keys-minor-mode-map (kbd "C-x C-u") 'undo)
(define-key my-keys-minor-mode-map (kbd "C-y") '(lambda ()
                                                  (interactive)
                                                  (apply-to-line-or-region 'kill-ring-save)))
(define-key my-keys-minor-mode-map (kbd "C-w") '(lambda ()
                                                  (interactive)
                                                  (apply-to-line-or-region 'kill-region)))

(define-key my-keys-minor-mode-map (kbd "C-=") 'upcase-previous-word)
;; use keyboard-translate to make C-d decrease indentation in Python mode
(keyboard-translate ?\C-d ?\C-?)
(define-key my-keys-minor-mode-map (kbd "M-d") 'delete-char)
(define-key my-keys-minor-mode-map (kbd "C-j") 'mark-str-or-pairs)

;; commenting
(define-key my-keys-minor-mode-map (kbd "<f5>") 'comment-region)
(define-key my-keys-minor-mode-map (kbd "<f6>") 'uncomment-region)

;; other
(define-key my-keys-minor-mode-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key my-keys-minor-mode-map (kbd "<f8>") 'find-file)
(define-key my-keys-minor-mode-map (kbd "<f7>") 'sudo-edit)

;; ------> mode hooks
(add-hook 'fsharp-mode-hook
          '(lambda ()
             (define-key fsharp-mode-map (kbd "<f12>") 'fsharp-compile-and-run)))

(add-hook 'scheme-mode-hook
          '(lambda ()
             (define-key scheme-mode-map (kbd "<f12>") 'scheme-eval-last-and-run)))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (define-key lisp-mode-map (kbd "<f12>") 'slime-compile-and-load-file)))

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map (kbd "<return>")
               'dired-find-alternate-file)
             (define-key dired-mode-map "t" 'dired-next-line)
             (define-key dired-mode-map "c" 'dired-previous-line)
             (define-key dired-mode-map (kbd "<tab>") 'dired-dotfiles-toggle)))

(setq html-mode-hook
      '(lambda ()
         (jinja2-mode)
         (define-key jinja2-mode-map (kbd "<f9>") 'tag-word-or-region)))

(add-hook 'org-mode-hook '(lambda ()
                               ;; (my-org-minor-mode)
                               (define-key org-mode-map (kbd "C-,") nil)
                               (define-key org-mode-map "\C-cd" 'org-deadline)
                               (define-key org-mode-map "\C-cs" 'org-schedule)
                               (define-key org-mode-map "\C-ca" 'org-agenda)))

(add-hook 'org-agenda-mode-hook '(lambda ()
                            (define-key org-agenda-mode-map (kbd "t") 'org-agenda-next-line)
                            (define-key org-agenda-mode-map (kbd "c") 'org-agenda-previous-line)))

(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map (kbd "<f9>")
               'python-shell-switch-to-shell)
             (define-key python-mode-map (kbd "<f10>")
               'python-send-region-or-buffer)
             (define-key python-mode-map (kbd "<f12>")
               'my-compile)))

(add-hook 'python-mode-hook 'jedi:setup)

;; (add-hook 'scheme-mode-hook
;;           (lambda ()
;;             (define-key scheme-mode-map (kbd "C-c C-c")
;;               'scheme-send-definition-and-go)))

;; settings for R
;; (setq ess-ask-for-ess-directory nil)
;; (setq comint-scroll-to-bottom-on-input t)
;; (setq comint-scroll-to-bottom-on-output t)
;; (setq comint-move-point-for-output t)

;; (add-hook 'ess-mode-hook
;;           '(lambda ()
;;              (define-key ess-mode-map (kbd "<f12>")
;;                'ess-eval-region-or-buffer)))

;; ------> files/folders to open at startup
;; (find-file "~/.emacs.d/init.el")
(find-file "~/projects")
;; (find-file "~/notes")

;; ------> org mode
(setq org-startup-folded nil)
(setq org-directory "~/notes/")
(setq org-agenda-files '("~/notes/"))
(setq org-default-notes-file (concat org-directory "/capture.org")) ;; file for capture notes
(define-key global-map "\C-cp" 'org-capture)
(setq org-refile-targets '((nil :maxlevel . 2)
                                ; all top-level headlines in the
                                ; current buffer are used (first) as a
                                ; refile target
                           (org-agenda-files :maxlevel . 2)))

(setq org-log-done 'time)
(setq org-agenda-start-on-weekday 1)
;; (setq org-default-notes-file (concat org-directory "test.org"))

(setq org-agenda-ndays 7)
(setq org-agenda-show-all-dates t)
;; (setq org-agenda-skip-deadline-if-done t)
;; (setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)


;; (setq org-todo-keywords
      ;; '((sequence "TODO(t)" "SOMEDAY(s)" "MORE(m)" "|" "DONE(d)" "WAIT(w)" "WONT(n)")))
;; (setq org-use-fast-todo-selection t)


;; ------> misc settings
(setq compilation-scroll-output t) ;; scroll to the end of compilation buffer
(setq erc-hide-list '("JOIN" "PART" "QUIT")) ;; hide join/quit messages in IRC

(show-paren-mode t)
;; disable back-ups
;; (setq make-backup-files nil)

;; ;; custom folder for backups
;; (defvar my-auto-save-folder "~/.saves")
;; (setq backup-directory-alist `((".*" . ,my-auto-save-folder)))
;; (setq auto-save-file-name-transforms `((".*" ,my-auto-save-folder t)))
;; ;; set same backup paths for Tramp
;; (setq tramp-backup-directory-alist backup-directory-alist)
;; (setq tramp-auto-save-directory my-auto-save-folder)

;; (setq-default line-spacing 2)

;; display file path in window title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(column-number-mode 1) ;; display column number on mode line
(global-visual-line-mode 1) ;;wrap long lines at word boundaries
(blink-cursor-mode 0) ;;disable cursor blinking
(setq blink-matching-delay 0.1)

;; indentation
(setq-default indent-tabs-mode nil) ; always replace tabs with spaces
(setq-default tab-width 4) ; set tab width to 4 for all buffers

;; C-k kills whole line and newline if at beginning of line
(setq kill-whole-line t)

;; type "y"/"n" instead of "yes"/"no"
(fset 'yes-or-no-p 'y-or-n-p)

;; modify word boundaries:
(modify-syntax-entry ?_ "w") ;;remove underscore

;; kill to os clipboard
(setq x-select-enable-clipboard t)

;; make split buffer vertical
;; (setq split-height-threshold nil)
;; (setq split-width-threshold 0)

;; selection is replaced with typed text
(delete-selection-mode 1)

;;nicer buffer switching
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)
;; (setq ido-create-new-buffer 'always)

(iswitchb-mode t)
;; (add-to-list 'iswitchb-buffer-ignore "^\\*scratch")
;; (add-to-list 'iswitchb-buffer-ignore "^\\*Messages")

(require 'edmacro)
(defun iswitchb-local-keys ()
      (mapc (lambda (K)
	      (let* ((key (car K)) (fun (cdr K)))
    	        (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	    '(("<left>" . iswitchb-next-match)
	      ("<right>"  . iswitchb-prev-match)
	      ("<up>"    . ignore             )
	      ("<down>"  . ignore             ))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;;hide toolbar etc
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;scrolling
;; (set-scroll-bar-mode 'right)
(setq
  scroll-margin 0
  scroll-conservatively 100000
  scroll-preserve-screen-position 1)

;;disable splash screen and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;;maximize window on startup
(defun fullscreen (&optional f)
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

(run-with-idle-timer 0.1 nil 'fullscreen)

;;fullscreen mode
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))

(global-set-key [f11] 'toggle-fullscreen)
