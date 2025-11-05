;;; tiles.el --- Tiling window manager for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: edmacs Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, frames

;;; Commentary:
;; A tiling window manager for Emacs inspired by i3wm.

;;; Code:

(require 'seq)

(defgroup tiles nil
  "Tiling window manager for Emacs."
  :group 'convenience
  :prefix "tiles-")

(defcustom tiles-default-layout 'horizontal
  "Default layout for new frames."
  :type '(choice (const horizontal) (const vertical)
                 (const grid) (const master-stack))
  :group 'tiles)

(defcustom tiles-master-window-ratio 0.5
  "Ratio of space for master window."
  :type 'float
  :group 'tiles)

(defcustom tiles-auto-balance t
  "Automatically balance windows."
  :type 'boolean
  :group 'tiles)

(defvar tiles--buffer-list nil
  "Ordered list of buffers to display.")

(defvar tiles--current-layout nil
  "Current active layout.")

(defun tiles--get-displayable-buffers ()
  "Get list of buffers suitable for display."
  (seq-filter
   (lambda (buf)
     (let ((name (buffer-name buf)))
       (and (buffer-live-p buf)
            (not (string-prefix-p " " name))
            (not (minibufferp buf)))))
   (buffer-list)))

(defun tiles--calculate-grid-dimensions (num-windows)
  "Calculate grid dimensions for NUM-WINDOWS."
  (let* ((cols (ceiling (sqrt num-windows)))
         (rows (ceiling (/ (float num-windows) cols))))
    (cons rows cols)))

(defun tiles--apply-horizontal-layout (buffers)
  "Apply horizontal layout to BUFFERS."
  (delete-other-windows)
  (when (> (length buffers) 0)
    (let ((window (selected-window)))
      (set-window-buffer window (car buffers))
      (dolist (buf (cdr buffers))
        (let ((new-window (split-window window nil t)))
          (set-window-buffer new-window buf)
          (setq window new-window)))
      (when tiles-auto-balance
        (balance-windows)))))

(defun tiles--apply-vertical-layout (buffers)
  "Apply vertical layout to BUFFERS."
  (delete-other-windows)
  (when (> (length buffers) 0)
    (let ((window (selected-window)))
      (set-window-buffer window (car buffers))
      (dolist (buf (cdr buffers))
        (let ((new-window (split-window window nil nil)))
          (set-window-buffer new-window buf)
          (setq window new-window)))
      (when tiles-auto-balance
        (balance-windows)))))

(defun tiles--apply-grid-layout (buffers)
  "Apply grid layout to BUFFERS."
  (delete-other-windows)
  (when (> (length buffers) 0)
    (set-window-buffer (selected-window) (car buffers))
    (when tiles-auto-balance
      (balance-windows))))

(defun tiles--apply-master-stack-layout (buffers)
  "Apply master-stack layout to BUFFERS."
  (delete-other-windows)
  (let ((num (length buffers)))
    (cond
     ((= num 1)
      (set-window-buffer (selected-window) (car buffers)))
     (t
      (let* ((master (selected-window))
             (stack (split-window master nil t)))
        (set-window-buffer master (car buffers))
        (set-window-buffer stack (cadr buffers))
        (when tiles-auto-balance
          (balance-windows)))))))

(defun tiles--apply-layout (layout buffers)
  "Apply LAYOUT to BUFFERS."
  (pcase layout
    ('horizontal (tiles--apply-horizontal-layout buffers))
    ('vertical (tiles--apply-vertical-layout buffers))
    ('grid (tiles--apply-grid-layout buffers))
    ('master-stack (tiles--apply-master-stack-layout buffers))
    (_ (tiles--apply-horizontal-layout buffers))))

(defun tiles--get-managed-buffers ()
  "Get list of managed buffers."
  (when (null tiles--buffer-list)
    (setq tiles--buffer-list (tiles--get-displayable-buffers)))
  (seq-filter 'buffer-live-p tiles--buffer-list))

(defun tiles-refresh ()
  "Refresh the current tiling layout."
  (interactive)
  (let ((buffers (tiles--get-managed-buffers))
        (layout (or tiles--current-layout tiles-default-layout)))
    (when buffers
      (tiles--apply-layout layout buffers))))

(defun tiles-set-layout (layout)
  "Set the tiling LAYOUT."
  (interactive
   (list (intern (completing-read "Layout: "
                                   '("horizontal" "vertical" "grid" "master-stack")
                                   nil t))))
  (setq tiles--current-layout layout)
  (tiles-refresh))

(defun tiles-cycle-layout ()
  "Cycle through available layouts."
  (interactive)
  (let* ((layouts '(horizontal vertical grid master-stack))
         (current (or tiles--current-layout tiles-default-layout))
         (idx (cl-position current layouts))
         (next-idx (mod (1+ idx) (length layouts)))
         (next (nth next-idx layouts)))
    (tiles-set-layout next)
    (message "Layout: %s" next)))

(defun tiles-focus-next ()
  "Focus next window."
  (interactive)
  (other-window 1))

(defun tiles-focus-previous ()
  "Focus previous window."
  (interactive)
  (other-window -1))

;;;###autoload
(define-minor-mode tiles-mode
  "Minor mode for tiling window management."
  :global t
  :lighter " Tiles"
  :group 'tiles
  (if tiles-mode
      (progn
        (setq tiles--buffer-list (tiles--get-displayable-buffers))
        (tiles-refresh))
    (setq tiles--buffer-list nil)
    (setq tiles--current-layout nil)))

;;;###autoload
(defun tiles-setup ()
  "Initialize tiles with current buffers."
  (interactive)
  (setq tiles--buffer-list (tiles--get-displayable-buffers))
  (setq tiles--current-layout tiles-default-layout)
  (tiles-mode 1))

(provide 'tiles)
;;; tiles.el ends here
