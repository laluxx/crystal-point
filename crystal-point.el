;;; crystal-point.el --- Dynamically update cursor color based on text properties  -*- lexical-binding: t; -*-

;;; Commentary:

;; dynamically update the point color

;;; Code:

(defun crystal-point/update-cursor-color ()
  "Update the cursor color based on the foreground color of the character at point."
  (let* ((pos (point))
         ;; Temporarily disable hl-line-mode if it's enabled.
         (hl-line-enabled (bound-and-true-p hl-line-mode))
         (face (progn
                 (when hl-line-enabled (hl-line-mode -1))
                 (or (car (face-at-point nil t))  ; Get face from overlays/text properties.
                     'default)))
         (fg-color (face-attribute face :foreground nil t))
         ;; Use the real cursor color of the theme, or the foreground of `font-lock-comment-face` as fallback.
         (theme-cursor-color (frame-parameter nil 'cursor-color))
         (fallback-color (face-attribute 'font-lock-comment-face :foreground))
         (cursor-color (if (or (not fg-color) (string= fg-color "unspecified"))
                           (or theme-cursor-color fallback-color)
                         fg-color)))
    ;; Re-enable hl-line-mode if it was previously enabled.
    (when hl-line-enabled (hl-line-mode 1))
    ;; Set the cursor color if it differs from the current one to minimize updates.
    (unless (equal cursor-color (frame-parameter nil 'cursor-color))
      (set-cursor-color cursor-color))))

;;;###autoload
(defun crystal-point-enable ()
  "Enable dynamic cursor color updates."
  (interactive)
  (add-hook 'post-command-hook 'crystal-point/update-cursor-color))

;;;###autoload
(defun crystal-point-disable ()
  "Disable dynamic cursor color updates."
  (interactive)
  (remove-hook 'post-command-hook 'crystal-point/update-cursor-color))

(provide 'crystal-point)

;;; crystal-point.el ends here
