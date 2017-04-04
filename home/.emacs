(setq inhibit-splash-screen t)
;; (server-start)

(setq tramp-mode nil)

(setq kill-whole-line t)
(setq c-hungry-delete-key t)
;;(setq c-auto-newline 1)
(setq c-backslash-max-column 80)

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'arglist-intro '+)
            (c-set-offset 'arglist-close 0)
            (modify-syntax-entry ?_ "w")))

(setq-default indent-tabs-mode nil
	      c-basic-offset 2)

(setq ediff-split-window-function 'split-window-horizontally)

(add-to-list 'load-path "~/emacs/elisp")

(setq compilation-skip-threshold 2)
(setq compilation-always-kill t)

;; mwheel may be broken
(require 'mwheel)
(if (not (boundp 'mouse-wheel-mode))
    ;(unload-feature 'mwheel)
    (load "~/emacs/elisp/mwheel"))
(mouse-wheel-mode t)

;; (when (not
;;        (or (window-system)
;; 	   (string-match "^screen" (getenv "TERM"))))
;; ;(setq frame-title-format `(,(user-login-name) "@" ,(system-name) "     " global-mode-string "     %f" ))
;; ;(when (and (not window-system)
;; ;               (string-match "^xterm" (getenv "TERM")))
;; ;(require 'xterm-title)
;; ; (xterm-title-mode 1))
;;   (require 'xterm-title)
;;   (xterm-title-mode 1)
;;   (setq frame-title-format `("emacs")))


(global-set-key "\M-g" 'goto-line)

(setq transient-mark-mode t)
(setq column-number-mode t)
;(partial-completion-mode t)
(put 'downcase-region 'disabled nil)

(add-hook 'fortran-mode-hook 'which-function-mode)

(require 'saveplace)
(setq-default save-place t)

(desktop-save-mode 1)
(savehist-mode t)
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")


(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

(put 'upcase-region 'disabled nil)

(defun my-pystuff ()
  (setq py-indent-offset 4
        indent-tabs-mode t
        py-smart-indentation nil))
(add-hook 'python-mode-hook 'my-pystuff)

(setq gnus-select-method '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)))

(setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.dox$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.c$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cu$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tcc$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.src$" . fortran-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.F90$" . f90-mode) auto-mode-alist))


;(require 'yasnippet)
(add-to-list 'load-path "~/emacs/elisp/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)

(require 'dropdown-list)
(setq yas/prompt-functions '( yas/ido-prompt
                              yas/dropdown-prompt
                              yas/completing-prompt))

;; Develop in ~/emacs.d/mysnippets, but also
;; try out snippets in ~/Downloads/interesting-snippets
(setq yas/root-directory '("~/emacs/snippets" "~/emacs/elisp/yasnippet/snippets"))

;; ;; Map `yas/load-directory' to every element
(mapc 'yas/load-directory yas/root-directory)
;; ;; (yas/initialize)


(require 'linum)
;(global-linum-mode t)
(add-hook 'find-file-hook (lambda ()
 			    (if (not(equal major-mode 'term-mode))
 				(linum-mode nil))))

;(setq linum-format "%d ")
(setq linum-format
      (lambda (line) 
	(propertize (format
		     (let ((w (length (number-to-string
				       (count-lines (point-min) (point-max))))))
		       (concat "%" (number-to-string w) "d ")) line) 'face 'linum)))


(load-file "~/emacs/elisp/cc-cheetah-mode.el")
(load-file "~/emacs/elisp/functions.el")

(load-file "~/emacs/elisp/dvc-mode.el")
(autoload 'dvc-mode "dvc-mode" "DVC mode." t)


; cedet library
;(load-library "cedet-1.0pre6/common/cedet.el")

;; ;(load-library "cedet/common/cedet.el")
;; (add-to-list 'load-path "~/emacs/elisp/cedet/common")
;; (require 'cedet)
;; (require 'semantic-ia)
;; (require 'semantic-gcc)

;; (global-ede-mode t)
;; ;; (global-semanticdb-minor-mode)

;; ;; (semantic-add-system-include "/usr/include/" 'c++-mode)
;; ;; (semantic-add-system-include "/usr/include/c++/4.3/" 'c++-mode)
;; ;; (semantic-add-system-include "/usr/include/" 'c-mode)

;; ;; Enabling Semantic (code-parsing, smart completion) features
;; ;; Select one of the following:

;; ;; * This enables the database and idle reparse engines
;; ;(semantic-load-enable-minimum-features)

;; ;; * This enables some tools useful for coding, such as summary mode
;; ;;   imenu support, and the semantic navigator
;; (semantic-load-enable-code-helpers)

;; ;; * This enables even more coding tools such as intellisense mode
;; ;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;; ;(semantic-load-enable-gaudy-code-helpers)

;; ;; * This enables the use of Exuberent ctags if you have it installed.
;; ;;   If you use C++ templates or boost, you should NOT enable it.
;; ;; (semantic-load-enable-all-exuberent-ctags-support)
;; ;;   Or, use one of these two types of support.
;; ;;   Add support for new languges only via ctags.
;; ;; (semantic-load-enable-primary-exuberent-ctags-support)
;; ;;   Add support for using ctags as a backup parser.
;; ;; (semantic-load-enable-secondary-exuberent-ctags-support)

;; ;; Enable SRecode (Template management) minor-mode.
;; ;(global-srecode-minor-mode 1)

;; ;; ;; ;; gnu global support
;; ;;  (require 'semanticdb-global)
;; ;;  (semanticdb-enable-gnu-global-databases 'c-mode)
;; ;;  (semanticdb-enable-gnu-global-databases 'c++-mode)
 
;; ;; ;; ;; ctags
;; ;;  (require 'semanticdb-ectag)
;; ;;  (semantic-load-enable-all-exuberent-ctags-support)
;; ;; (semantic-load-enable-code-helpers)

;; ;;; ;; ;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file '"/usr/include/boost/config.hpp")

;; ;; ;; ;; (defun my-semantic-add-preprocessor-files (root regex)
;; ;; ;; ;;   (let ((files (remove-if  'file-directory-p (directory-files root t regex)))
;; ;; ;; ;; 	(dirs (remove-if-not 'file-directory-p (directory-files root t))))
;; ;; ;; ;;     (while files 
;; ;; ;; ;;       (add-to-list 'semantic-lex-c-preprocessor-symbol-file (car files))
;; ;; ;; ;;       (setq files (cdr files)))
;; ;; ;; ;;     (setq dirs (delete (concat root "/.") dirs))
;; ;; ;; ;;     (setq dirs (delete (concat root "/..") dirs))
;; ;; ;; ;;     (mapcar '(lambda(d) (my-semantic-add-preprocessor-files d regex)) dirs)
;; ;; ;; ;;     nil))

;; ;; (my-semantic-add-preprocessor-files "/usr/include/boost" "^config\.hpp$")
;; ;; (my-semantic-add-preprocessor-files "/usr/include/boost/config" ".*\.hpp$")


;; (add-to-list 'load-path "~/emacs/elisp/ecb")
;; ;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/ecb")
;; (require 'ecb)
;; ;(add-to-list 'ecb-compilation-predicates (lambda (b) (message (buffer-name b)) t))
;; (defun my-predicate (b) (and (comint-check-proc b)
;; 			     (not (string-match "^\*ielm.*"  (buffer-name b)))))
;; (setq ecb-compilation-predicates '(my-predicate))



;;(defun my-directory-tree (root)
;;  (let ((children (remove-if '(lambda(f) (or (not (file-directory-p f))
;; 					     (string= (concat root "/.") f)
;; 					     (string= (concat root "/..") f)))
;; 			     (directory-files root t))))
;;     (cons root (reduce 'append (mapcar 'my-directory-tree children)))))
    
;; (defun directory-find-files (root regex)
;;   (let ((files (remove-if  'file-directory-p (directory-files root t regex)))
;; 	(dirs (remove-if-not 'file-directory-p (directory-files root t))))
;;     (setq dirs (delete (concat root "/.") dirs))
;;     (setq dirs (delete (concat root "/..") dirs))
;;     (append files (reduce 'append (mapcar '(lambda(d) (directory-find-files d regex)) 
;; 					  dirs)))))

;(append files (reduce 'append (mapcar 'my-recursive-files dirs)))))

  ;; (let ((filter-files '(lambda(d) (remove-if 'file-directory-p
  ;; 					     (mapcar '(lambda(f) (concat d "/" f)))))))
  ;;   (mapcar '(lambda(d) (filter-files (directory-files d nil regex)))
  ;; 	    (my-directory-tree root))))


(setq compilation-scroll-output 'first-error)

(defun compilation-window-set-height (buffer height)
  "Shrink the window if the process finished successfully."
  (let ((compilation-window-height height))
    (compilation-set-window-height (get-buffer-window buffer 0))))

(add-hook 'compilation-finish-functions
	  (lambda (buf str)
	    (if (string-match "exited abnormally" str) 
;		(next-error)
	      ;;no errors, make the compilation window go away in a few seconds
	      ;(run-at-time "2 sec" nil 'delete-windows-on (get-buffer-create "*compilation*"))
	        (compilation-window-set-height buf 20)
	        (compilation-window-set-height buf 10)
	      )
	    ))

;(add-hook 'compilation-finish-functions 'my-compilation-finish-function)


;; ;(require 'flymake-extension)
;; ;(require 'flymake)

;; (add-to-list 'flymake-allowed-file-name-masks
;; 	     '("^/home.*\\.hpp$" flymake-simple-make-init))

;; ;;flymake-ler(file line type text &optional full-file)
;; (defun show-fly-err-at-point ()
;; "If the cursor is sitting on a flymake error, display the
;; message in the minibuffer"
;; (interactive)
;; (let ((line-no (line-number-at-pos)))
;; (dolist (elem flymake-err-info)
;; (if (eq (car elem) line-no)
;; (let ((err (car (second elem))))
;; (message  "%s" (fly-pyflake-determine-message err)))))))

;; (defun fly-pyflake-determine-message (err)
;; "pyflake is flakey if it has compile problems, this adjusts the
;; message to display, so there is one ;)"
;; (cond ((not (or (eq major-mode 'Python) (eq major-mode 'python-mode) t)))
;;       ((null (flymake-ler-file err))
;;        ;; normal message do your thing
;;        (flymake-ler-text err))
;;       (t ;; could not compile err
;;        (format "compile error, problem on line %s" (flymake-ler-line err)))))

;; (defadvice flymake-goto-next-error (after display-message activate compile)
;; "Display the error in the mini-buffer rather than having to mouse over it"
;; (show-fly-err-at-point))

;; (defadvice flymake-goto-prev-error (after display-message activate compile)
;; "Display the error in the mini-buffer rather than having to mouse over it"
;; (show-fly-err-at-point))

;; (defadvice flymake-mode (before post-command-stuff activate compile)
;; "Add functionality to the post command hook so that if the
;; cursor is sitting on a flymake error the error information is
;; displayed in the minibuffer (rather than having to mouse over
;; it)"
;; (set (make-local-variable 'post-command-hook)
;; (cons 'show-fly-err-at-point post-command-hook))) 

;; (defalias 'my-emacs-command-check-syntax 'flymake-start-syntax-check)
;; (defalias 'my-emacs-command-next-syntax-error 'flymake-goto-next-error)
;; (defalias 'my-emacs-command-previous-syntax-error 'flymake-goto-prev-error)

(require 'tabbar)
(tabbar-mode t)
(setq tabbar-buffer-groups-function
      (lambda () (list "All")))

(load-file "~/emacs/elisp/my-emacs")

(add-to-list 'load-path "~/emacs/elisp/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/emacs/ac-dict")
(setq ac-user-dictionary-file nil)

(add-to-list 'ac-user-dictionary-files "~/emacs/ac-dict/cpp")
(add-to-list 'ac-user-dictionary-files "~/emacs/ac-dict/cuda")
(add-to-list 'ac-user-dictionary-files "~/emacs/ac-dict/boost")

(ac-config-default)
(add-to-list 'ac-modes 'arduino-mode)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-compile-window-height 6)
 '(ecb-layout-name "my-layout")
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.40")
 '(ecb-show-sources-in-directories-buffer (quote ("left7" "left13" "left14" "left15" "my-layout")))
 '(ede-project-directories (quote ("/home/andrey/mpqc")))
 '(term-bind-key-alist (quote (("C-c C-c" . term-interrupt-subjob) ("C-p" . previous-line) ("C-n" . next-line) ("C-s" . isearch-forward) ("C-m" . term-send-raw) ("M-f" . term-send-forward-word) ("M-b" . term-send-backward-word) ("M-o" . term-send-backspace) ("M-p" . term-send-up) ("M-n" . term-send-down) ("M-M" . term-send-forward-kill-word) ("M-N" . term-send-backward-kill-word) ("M-r" . term-send-reverse-search-history) ("M-," . term-send-input) ("M-." . comint-dynamic-complete)))))

(add-hook 'c++-mode-hook 'flymake-mode)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(flymake-warnline ((((class color)) (:background "ffff5f"))))
 '(linum ((t (:inherit (shadow default) :foreground "black" :background "color-255")))))
