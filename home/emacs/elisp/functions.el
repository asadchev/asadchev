
(defun syntax-forward (syntax &optional arg)
  "Move ARG times to start of a set of the same syntax characters."
  (setq arg (or arg 1))
  (let ((not-syntax (concat "^" syntax)))
    (while (and (> arg 0) (not (eobp)))
      (when (= 0 (skip-syntax-forward not-syntax))
	(skip-syntax-forward syntax) 
	(skip-syntax-forward not-syntax))
      (setq arg (- arg 1)))
    (while (and (< arg 0) (not (bobp)))
      (when (= 0 (skip-syntax-backward not-syntax))
	(skip-syntax-backward syntax) 
	(skip-syntax-backward not-syntax))
      (setq arg (1+ arg)))))

(defun syntax-backward (syntax &optional arg)
  "Move ARG times to end of a set of the same syntax characters."
  (syntax-forward syntax (- (or arg 1))))

(defmacro make-my-emacs-command-move-syntax (direction name syntax)
  (list 'defun (intern (format "my-emacs-command-%s-%s"  direction name))
	'(&optional arg)
        (list 'interactive "p")
        (list (intern (format "syntax-%s" direction)) syntax 'arg)))

(make-my-emacs-command-move-syntax forward blank " ")
(make-my-emacs-command-move-syntax forward symbol "_")
(make-my-emacs-command-move-syntax forward open "(")
(make-my-emacs-command-move-syntax forward close ")")
(make-my-emacs-command-move-syntax forward point ".")

(make-my-emacs-command-move-syntax backward blank " ")
(make-my-emacs-command-move-syntax backward symbol "_")
(make-my-emacs-command-move-syntax backward open "(")
(make-my-emacs-command-move-syntax backward close ")")
(make-my-emacs-command-move-syntax backward point ".")

(defun mark-buffer (&optional arg)
  (interactive "p")
  (mark-whole-buffer))

(defun mark-line (&optional arg)
  (interactive "p")
  (move-beginning-of-line 1)
  (call-interactively 'set-mark-command)
  (forward-line arg))

(defun mark-region (&optional arg))
(defalias 'mark-function 'mark-defun)
(defalias 'mark-sentence 'mark-end-of-sentence)


(defun my-emacs-command-kill-region()
  (kill-region (region-beginning) (region-end)))
 
(defun my-emacs-command-indent-region()
  (indent-region (region-beginning) (region-end)))

(defun my-emacs-command-comment-region()
 (comment-region (region-beginning) (region-end)))

(defun my-emacs-command-uncomment-region()
 (uncomment-region (region-beginning) (region-end)))

(defun my-emacs-command-replace-region (&optional arg)
  (interactive "p")
  (call-interactively 'replace-string))

(defun my-emacs-command-regex-region (&optional arg)
  (interactive "p")
  (call-interactively 'replace-regex))

(defun my-emacs-command-downcase-region()
  (interactive "p")
  (downcase-region (region-beginning) (region-end)))

(defun my-emacs-command-upcase-region()
  (interactive "p")
  (upcase-region (region-beginning) (region-end)))
  
(defun my-emacs-command-copy-region()
  (interactive "p")
  (kill-ring-save (region-beginning) (region-end)))

(defun my-emacs-command-dupe-region()
  (interactive "p")
  (kill-ring-save (region-beginning) (region-end))
  (yank))

(defun my-emacs-command-beautify-region()
  (interactive "*r")
  (let ((cmd "indent -kr"))
    (shell-command-on-region (region-beginning) (region-end)
			     cmd (current-buffer) t))
  (indent-region (region-beginning) (region-end)))


(defmacro make-my-emacs-command-region (cmd name)
  (list 'defun (intern (format "my-emacs-command-%s-%s"
			       (eval cmd) (eval name)))
	'(&optional arg)
	(list 'interactive "p")
	(list (intern (format "mark-%s" (eval name))) 'arg)
	(list (intern (format "my-emacs-command-%s-region" (eval cmd))))))

(setq my-emacs-region
      '(buffer paragraph function line sentence word expression))

(setq my-emacs-region-command 
      '(kill
	beautify
	indent
	comment
	uncomment
	copy
	dupe
	replace
	regex
	mark
	downcase
	upcase))

(mapcar (lambda (c)
	  (mapcar (lambda (r) (make-my-emacs-command-region c r))
		  my-emacs-region))
	my-emacs-region-command)


;; (defun my-emacs-command-copy-line (&optional arg)
;;   (interactive "p")
;;   (mark-line arg)
;;   (kill-ring-save (region-beginning) (region-end))
;;   )

;; (defun my-emacs-command-comment-line (&optional arg)
;;   (interactive "p")
;;   (mark-line arg)
;;   (comment-region (region-beginning) (region-end))
;;   )

;; (defun my-emacs-command-uncomment-line (&optional arg)
;;   (interactive "p")
;;   (mark-line arg)
;;   (uncomment-region (region-beginning) (region-end))
;;   )


;; (defun my-emacs-command-comment-previous-line (&optional arg)
;;   (interactive "p")
;;   (my-emacs-command-comment-line (- arg)))


;; (defun my-emacs-command-comment-paragraph (&optional arg)
;;   (interactive "p")
;;   (mark-paragraph arg)
;;   (comment-region (region-beginning) (region-end)))

;; (defun my-emacs-command-uncomment-paragraph (&optional arg)
;;   (interactive "p")
;;   (mark-paragraph arg)
;;   (uncomment-region (region-beginning) (region-end)))

;; (defun my-emacs-command-copy-paragraph (&optional arg)
;;   (interactive "p")
;;   (mark-paragraph arg)
;;   (kill-ring-save (region-beginning) (region-end)))

;; (defun my-emacs-command-comment-previous-paragraph (&optional arg)
;;   (interactive "p")
;;   (backward-paragraph 1)
;;   (my-emacs-command-comment-paragraph (- arg)))

;; (defun my-emacs-command-comment-previous-line (&optional arg)
;;   (interactive "p")
;;   (my-emacs-command-comment-line (- arg)))

;; (defun beautify-paragraph(&optional arg)
;;   (interactive "p")
;;   (mark-paragraph arg)
;;   (beautify-region (region-beginning) (region-end)))

;; (defun beautify-line(&optional arg)
;;   (interactive "p")
;;   (mark-line arg)
;;   (beautify-region (region-beginning) (region-end)))

;; (defalias 'my-emacs-command-beautify-region 'beautify-region)
;; (defalias 'my-emacs-command-beautify-line 'beautify-line)
;; (defalias 'my-emacs-command-beautify-paragraph 'beautify-paragraph)

