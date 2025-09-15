;;; crystal-point.el --- Dynamic cursor color matching face at point  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laluxx
;; Author: Laluxx
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, cursor, faces
;; URL: https://github.com/laluxx/crystal-point

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;

;;; Commentary:

;; Dynamically update the cursor color to match face foreground color at point
;;
;; Usage:
;;   (crystal-point-enable)   ; Enable the mode
;;   (crystal-point-disable)  ; Disable the mode
;;
;; The package works in both GUI and terminal, and handles theme
;; changes for both `load-theme` and `consult-theme`.

;;; TODO:

;; FIX `hl-line-mode' and overlays

;;; Code:

(defun crystal-point--get-cursor-backup-color ()
  "Get a valid cursor backup color, falling back to default foreground if needed."
  (let ((cursor-bg (face-attribute 'cursor :background nil t)))
    (if (or (not cursor-bg) (string= cursor-bg "unspecified"))
        (face-attribute 'default :foreground)
      cursor-bg)))

(defun crystal-point--update-cursor-backup ()
  "Update the cursor-backup face color."
  (set-face-attribute 'cursor-backup nil
                      :foreground (crystal-point--get-cursor-backup-color)))

(defun crystal-point--update-cursor-color ()
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

(defun crystal-point--theme-advice (&rest _)
  "Advice function to update cursor backup after theme changes."
  (crystal-point--update-cursor-backup))

;;;###autoload
(defun crystal-point-enable ()
  "Enable dynamic cursor color updates."
  (interactive)
  (unless (facep 'cursor-backup)
    (make-face 'cursor-backup))
  (crystal-point--update-cursor-backup)
  (add-hook 'post-command-hook #'crystal-point--update-cursor-color)
  (advice-add 'enable-theme :after #'crystal-point--theme-advice))

;;;###autoload
(defun crystal-point-disable ()
  "Disable dynamic cursor color updates."
  (interactive)
  (remove-hook 'post-command-hook #'crystal-point--update-cursor-color)
  (advice-remove 'enable-theme #'crystal-point--theme-advice))

(provide 'crystal-point)
;;; crystal-point.el ends here
