
;; ------> library imports and configuration
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers
;; (setq autopair-blink nil)

(add-to-list 'load-path "~/.emacs.d/lisp/jinja2-mode/")
(autoload 'jinja2-mode "jinja2-mode")

(add-to-list 'load-path "~/.emacs.d/lisp/change-inner.el")
(autoload 'change-inner "change-inner.el")

(add-to-list 'load-path "~/.emacs.d/lisp/expand-region.el/")
(require 'expand-region)

(require 'control-lock)
(control-lock-keys)

(autoload 'forward-to-word "misc" t)

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/lisp/auto-complete.el/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/auto-complete.el//ac-dict")
(ac-config-default)
(setq ac-auto-start 4)
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-auto-show-menu 0.8)

;; set bindings to control the completion menu
(define-key ac-menu-map "\C-t" 'ac-next)
(define-key ac-menu-map "\C-c" 'ac-previous)
(define-key ac-menu-map "\C-n" nil) ;; remove the default binding
(define-key ac-menu-map "\M-n" nil) ;; remove the default binding


;; jedi (python autocompletion library, uses auto-complete)
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-epc")
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-ctable")
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-deferred")
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-jedi")
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method nil)
(setq jedi:get-in-function-call-delay 0)

;; (require 'nose)


;; ------> custom functions
(defun python-send-region-or-buffer ()
  "if region is active then send it, otherwise send the current buffer"
  (interactive)
  (if (and transient-mark-mode mark-active)
      (python-send-region-and-go (mark) (point))
    (python-send-buffer)
    (python-switch-to-python t)))

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

(defun copy-line-or-region ()
  (interactive)
  (apply-to-line-or-region 'kill-ring-save))

(defun kill-line-or-region ()
  (interactive)
  (apply-to-line-or-region 'kill-region))

;; TODO: probably there's a better way to do this....
(defun apply-to-line-or-region (f)
  "If region is selected then just calls f, otherwise selects the
current line and then calls f"
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively f)
    (save-excursion
      (mark-line)
      (call-interactively f)
      (message (concat (symbol-name f) ": current line")))))

(defun comment-line-or-region ()
  (interactive)
  (apply-to-line-or-region 'comment-region))

(defun uncomment-line-or-region ()
  (interactive)
  (apply-to-line-or-region 'uncomment-region))

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
  "add the colon for python compound statements, open a new line"
  (interactive)
  (back-to-indentation)
  (if (member (thing-at-point 'word)
              '("def" "for" "while" "class" "if" "try" "with" "else" "elif"))
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

(setq compilation-scroll-output t)

;; ------> global re-mappings
(define-key (current-global-map) [remap backward-kill-word]
  'backward-kill-word-or-region)
(define-key (current-global-map) [remap newline] 'newline-and-indent)
(define-key (current-global-map) [remap backward-word] 'my-backward-word)
(define-key (current-global-map) [remap forward-word] 'my-forward-word)
(define-key (current-global-map) [remap comment-region]
  'comment-line-or-region)
(define-key (current-global-map) [remap uncomment-region]
  'uncomment-line-or-region)

;; ------> keybindings for Dvorak layout
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(defvar my-comint-minor-mode-map (make-keymap) "my-comint-minor-mode keymap.")

;; re-map C-x and C-c
(keyboard-translate ?\C-m ?\C-x)
(keyboard-translate ?\C-p ?\C-c)
(keyboard-translate ?\C-c ?\C-p)

;; isearch mode bindings
(define-key isearch-mode-map "\C-n" 'isearch-repeat-forward)
(define-key isearch-mode-map "\C-h" 'isearch-repeat-backward)
(define-key isearch-mode-map "\C-t" 'isearch-exit)
(define-key isearch-mode-map "\C-d" 'isearch-delete-char)

;; brackets ;;TODO: move to .Xmodmap
(define-key my-keys-minor-mode-map (kbd "C-9") "[")
(define-key my-keys-minor-mode-map (kbd "C-0") "]")
(define-key my-keys-minor-mode-map (kbd "M-9") "{")
(define-key my-keys-minor-mode-map (kbd "M-0") "}")

;; cursor movement
(define-key my-keys-minor-mode-map "\C-h" 'backward-char)
(define-key my-keys-minor-mode-map "\M-h" 'backward-word)
(define-key my-keys-minor-mode-map "\C-n" 'forward-char)
(define-key my-keys-minor-mode-map "\M-n" 'forward-word)
(define-key my-keys-minor-mode-map "\M-c" 'backward-paragraph)
(define-key my-keys-minor-mode-map "\C-t" 'next-line)
(define-key my-keys-minor-mode-map "\M-t" 'forward-paragraph)

;; editing
(define-key my-keys-minor-mode-map (kbd "C--") 'mark-line)
(define-key my-keys-minor-mode-map "\M-b" 'backward-kill-word-or-region)
(define-key my-keys-minor-mode-map "\C-b" 'backward-kill-word-or-region)
(define-key my-keys-minor-mode-map "\C-f" 'er/expand-region)
(define-key my-keys-minor-mode-map "\M-w" 'change-inner)
(define-key my-keys-minor-mode-map "\M-f" 'er/contract-region)
(define-key my-keys-minor-mode-map "\C-o" 'my-open-line)
(define-key my-keys-minor-mode-map "\C-v" 'yank)
(define-key my-keys-minor-mode-map "\M-v" 'yank-pop)
(define-key my-keys-minor-mode-map (kbd "C-x r") 'replace-string)
(define-key my-keys-minor-mode-map "\M-l" 'recenter-top-bottom)
(define-key my-keys-minor-mode-map (kbd "<C-return>") 'open-block)
(define-key my-keys-minor-mode-map (kbd "C-y") 'copy-line-or-region)
(define-key my-keys-minor-mode-map (kbd "C-w") 'kill-line-or-region)
;; use keyboard-translate to make C-d decrease indentation in Python mode
(keyboard-translate ?\C-d ?\C-?)
(define-key my-keys-minor-mode-map (kbd "M-d") 'backward-kill-char)

;; commenting
(define-key my-keys-minor-mode-map (kbd "<f5>") 'comment-region)
(define-key my-keys-minor-mode-map (kbd "<f6>") 'uncomment-region)

;; other
(define-key my-keys-minor-mode-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key my-keys-minor-mode-map (kbd "<f8>") 'find-file)
;; (define-key my-keys-minor-mode-map (kbd "C-.") 'hippie-expand)

;; in comint, up/down cycles through input history
(define-key my-comint-minor-mode-map "\C-p" 'comint-previous-input)
(define-key my-comint-minor-mode-map "\C-t" 'comint-next-input)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings always override major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(define-minor-mode my-comint-minor-mode
  "A minor mode for custom bindings specific to comint mode."
  nil " my-comint" 'my-comint-minor-mode-map)


;; ------> mode hooks
;; prevent dired from opening up many buffers
(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map (kbd "<return>")
               'dired-find-alternate-file)
             (define-key dired-mode-map "t" 'dired-next-line)
             (define-key dired-mode-map "c" 'dired-previous-line)))

;; html mode: switch to jinja2 mode and bind key for tag insertion
(setq html-mode-hook
      '(lambda ()
         (jinja2-mode)
         (define-key jinja2-mode-map (kbd "<f9>") 'tag-word-or-region)))

;; had to use another minor mode to override my-keys..
(add-hook 'comint-mode-hook '(lambda ()
                               (my-comint-minor-mode)))

;; make autopair handle triple quotes in python
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map (kbd "<f9>")
               'python-switch-to-python)
             (define-key python-mode-map (kbd "<f10>")
               'python-send-region-or-buffer)
             (define-key python-mode-map (kbd "<f12>")
               'my-compile)))

(add-hook 'python-mode-hook 'jedi:setup)


;; ------> files/folders to open at startup
(find-file "~/.emacs.d/init.el")
(find-file "~/projects")


;; ------> misc settings
;; disable back-ups
;; (setq make-backup-files nil)

(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; (show-paren-mode 1) ;;display matching parens
(column-number-mode 1) ;; display column number on mode line
(global-visual-line-mode 1) ;;wrap long lines at word boundaries
(blink-cursor-mode 0) ;;disable cursor blinking
;; (setq blink-matching-paren t)
;; (setq blink-matching-delay 0.5)

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
(iswitchb-mode t)
(add-to-list 'iswitchb-buffer-ignore "^\\*scratch")
(add-to-list 'iswitchb-buffer-ignore "^\\*Messages")

(require 'edmacro)
(defun iswitchb-local-keys ()
      (mapc (lambda (K)
	      (let* ((key (car K)) (fun (cdr K)))
    	        (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	    '(("<right>" . iswitchb-next-match)
	      ("<left>"  . iswitchb-prev-match)
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


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 102 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "dark green" :slant italic))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background light)) (:foreground "blue"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 88) (background light)) (:foreground "darkgoldenrod"))))
 '(region ((((class color) (min-colors 88) (background light)) (:background "light grey")))))
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
