(defun apply-vim-modeline ()
  (interactive)
  (if (evimodeline-has-emacs-modeline)
      (message "Emacs modeline detected, not applying vim modes")
    (let ((modeline (evimodeline-extract-modeline)))
      (if (null modeline)
          (message "no vim modeline found")
        (progn
          (message "vim modeline found! %S" modeline)
          (mapc 'evimodeline-apply-mode modeline))))))

(defun evimodeline-has-emacs-modeline ()
  (save-excursion
    (beginning-of-buffer)
    (search-forward-regexp "-\\*-[^\n]+-\\*-" (point-max) t)))

(defun evimodeline-apply-mode (mode)
  (let ((m (car mode))
        (v (cdr mode)))
    (cond
     ((or (equal m "textwidth") (equal m "tw"))
      (set (make-local-variable 'fill-column) (string-to-number v)))
     ((or (equal m "tabstop") (equal m "ts"))
      (set (make-local-variable 'tab-width) (string-to-number v)))
     ((or (equal m "shiftwidth") (equal m "sw"))
      (set (make-local-variable 'c-basic-offset) (string-to-number v)))
     ((or (equal m "expandtab") (equal m "et"))
      (set (make-local-variable 'indent-tabs-mode) nil))
     ((or (equal m "noexpandtab") (equal m "noet"))
      (set (make-local-variable 'indent-tabs-mode) t))
     ((or (equal m "readonly") (equal m "ro"))
      (toggle-read-only 1))
     (t (message "unknown setting %s" (car mode))))))

(defun evimodeline-extract-modeline ()
  (save-excursion
    (beginning-of-buffer)
    (when (search-forward-regexp "\\(vi\\|vim\\|ex\\):" (point-max) t)
      (skip-chars-forward " \t")
      (let ((modeline
             (if (looking-at "set? ")
                 ; compat-style modeline
                 (progn
                   (forward-char 4)
                   (skip-chars-forward " \t")
                   (let ((start (point)))
                     (end-of-line)
                     (search-backward ":" start t)
                     (buffer-substring-no-properties start (point))))
               ; new-style modeline
               (let ((start (point)))
                 (end-of-line)
                 (buffer-substring-no-properties start (point))))))
        (mapcar 'evimodeline-split-equals
                (split-string modeline ":\\|[ \t]+"))))))

(defun evimodeline-split-equals (s)
  (string-match "\\([^=]+\\)\\(?:=\\(.*\\)\\)?" s)
  (if (or (/= (match-beginning 0) 0)
          (/= (match-end 0) (length s)))
      nil
    (if (match-string 2 s)
        (cons (match-string 1 s) (match-string 2 s))
      (cons s nil))))
