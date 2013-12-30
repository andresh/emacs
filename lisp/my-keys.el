
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

;; (define-key my-keys-minor-mode-map (kbd "C-t")
;;   (call-unless :default 'forward-char
;;                (call-if-major-mode 'backward-char 'lisp-mode 'emacs-lisp-mode)))

(provide 'my-keys)
