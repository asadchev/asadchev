
(defvar gamess-history nil)
(defvar gamess-command "gamess.py")

(defvar gamess-debug-history nil)
(defvar gamess-debug-command nil)

(defun gamess-read-command (command history prompt)
  (read-shell-command prompt command
                      (if (equal (car history) command)
                          '(history . 1)
                        'history)))

(defun gamess(command &optional comint)
  "Run gamess"
  (interactive
   (list
    (let ((command gamess-command))
      (gamess-read-command command gamess-history "Gamess command: "))
    (consp current-prefix-arg)))
  (unless (equal command gamess-command)
    (setq gamess-command command))
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compilation-start command comint (lambda (b) "*gamess*")))

(defun debug-gamess(command)
  "Run gamess in debugger"
  (interactive
   (list
    (let ((command (if gamess-debug-command gamess-debug-command
		     (concat gamess-command " -g -- --annotate=3"))))
      (gamess-read-command command gamess-debug-history "Gamess debug command: "))))
  (unless (equal command gamess-debug-command)
    (setq gamess-debug-command command))
  (save-some-buffers (not compilation-ask-about-save) nil)
  (gdb command))

