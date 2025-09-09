;;; crystal-point.el --- Dynamically update cursor color based on text properties  -*- lexical-binding: t; -*-

;;; Commentary:

;; dynamically update the cursor color to match face foreground at point

;;; Code:

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme`.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook`."
  (run-hooks 'after-load-theme-hook))

(defun crystal-point/get-cursor-backup-color ()
  "Get a valid cursor backup color, falling back to default foreground if needed."
  (let ((cursor-bg (face-attribute 'cursor :background nil t)))
    (if (or (not cursor-bg) (string= cursor-bg "unspecified"))
        (face-attribute 'default :foreground)
      cursor-bg)))

(defun crystal-point/update-cursor-backup ()
  "Update the cursor-backup face color."
  (set-face-attribute 'cursor-backup nil
                      :foreground (crystal-point/get-cursor-backup-color)))

(defun crystal-point/update-cursor-color ()
  "Update the cursor color based on the foreground color of the character at point.
Works in both GUI and terminal modes."
  (let* ((char (char-after (point)))
         (fg (if (and char (not (char-equal char ?\s)) (not (char-equal char ?\t)) (not (char-equal char ?\n)))
                 (face-attribute (or (car (face-at-point nil t)) 'default) :foreground nil t)
               (face-attribute 'cursor-backup :foreground nil t)))
         (color (if (or (not fg) (string= fg "unspecified"))
                    (face-attribute 'default :foreground)
                  fg)))
    (if (display-graphic-p)
        (set-cursor-color color) ; GUI
      (send-string-to-terminal (format "\e]12;%s\a" color))))) ; Terminal

;;;###autoload
(defun crystal-point-enable ()
  "Enable dynamic cursor color updates."
  (interactive)
  (unless (facep 'cursor-backup)
    (make-face 'cursor-backup))
  (crystal-point/update-cursor-backup)
  (add-hook 'post-command-hook 'crystal-point/update-cursor-color)
  (add-hook 'after-load-theme-hook 'crystal-point/update-cursor-backup)
  (advice-add 'enable-theme :after (lambda (&rest _) (crystal-point/update-cursor-backup))))

;;;###autoload
(defun crystal-point-disable ()
  "Disable dynamic cursor color updates."
  (interactive)
  (remove-hook 'post-command-hook 'crystal-point/update-cursor-color)
  (remove-hook 'after-load-theme-hook 'crystal-point/update-cursor-backup)
  (advice-remove 'enable-theme 'crystal-point/update-cursor-backup))

(provide 'crystal-point)

;;; crystal-point.el ends here
