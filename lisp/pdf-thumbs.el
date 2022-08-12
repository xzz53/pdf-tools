;; pdf-thumbs.el --- Thumb side-window for PDF view.   -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Daniel Nicolai <dalanicolai@gmail.com>
;; Keywords: files, multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

(defcustom pdf-thumbs-width 175
  "Width of thumbnails in pixels.
In order to limit memory usage, the maximum width is 'hard-coded'
to 400."
  :type 'integer
  :group 'pdf-thumbs)

(defcustom pdf-thumbs-side 'left
  "Create thumbs window at this side.
Side should be a symbol left or right (default)."
  :type 'symbol
  :group 'pdf-thumbs)

(defcustom pdf-thumbs-show-page-numbers nil
  "Maximum number of PNG images per buffer."
  :type 'boolean
  :group 'pdf-thumbs)

(defcustom pdf-thumbs-show-page-after nil
  "Maximum number of PNG images per buffer."
  :type 'boolean
  :group 'pdf-thumbs)

(defcustom pdf-thumbs-mouse-face nil
  "Maximum number of PNG images per buffer."
  :type 'boolean
  :group 'pdf-thumbs)

(defcustom pdf-thumbs-mouse-face-color "blue"
  "Maximum number of PNG images per buffer."
  :type 'color
  :group 'pdf-thumbs)

(defvar-local pdf-thumbs-columns 1)

(defun pdf-create-thumb-files (&optional file force)
  (setq file (or file (buffer-file-name)))
  (let* ((output-dir (concat "/tmp/"
                             (file-name-as-directory (file-name-base file))))
         (dir-exists (file-exists-p output-dir)))
    (when (and dir-exists
               (not force))
      (user-error "Thumb dir exists. Add force argument to force overwrite."))
    (unless dir-exists
      (make-directory output-dir))
    (let ((proc (start-process "mutool-draw" "mutool draw" "mutool"
                               "draw"
                               "-o" (concat "/tmp/"
                                            (file-name-as-directory (file-name-base file))
                                            "thumb%d.png")
                               "-w" "175"
                               file)))
      (set-process-sentinel proc (lambda (process event)
                                   (message "Create thumbs process %s" event))))))


(defun pdf-thumbs (&optional columns)
  "Show thumbs in a side window.
The number of COLUMNS can be set with a numeric prefix argument."
  (interactive "p")
  (let* ((buffer-name "*thumbs*")
         (buf (get-buffer buffer-name))
         (file (buffer-file-name))
         (output-dir (concat "/tmp/"
                             (file-name-as-directory (file-name-base file))))
         (last-page (pdf-info-number-of-pages))
         (win (selected-window)))
    (or (and buf
             (= (buffer-local-value 'pdf-thumbs-columns buf) columns)) 

        (with-current-buffer (get-buffer-create buffer-name)
          (unless (file-exists-p output-dir)
            (pdf-create-thumb-files file))
          (pdf-thumbs-mode)
          (setq pdf-thumbs-columns columns)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (let* ((columns (or columns 1))
                   (w 100)
                   (h (* w 1.6))
                   (source-svg (svg-create w h)))
              (svg-rectangle source-svg 0 0 w h :fill "white")
              (dotimes (i last-page)
                (let* ((p (1+ i))
                       (svg (copy-sequence source-svg))
                       (im (create-image (concat output-dir
                                                 (format "thumb%d.png" p))
                                         'png
                                         nil
                                         :margin '(2 . 1))))
                  (apply #'insert-button (format "%03d " p)
                         (append (list 'page (number-to-string p)
                                        'win win
                                        'face 'default
                                        'display im
                                        'action (lambda (b)
                                                  (with-selected-window (button-get b 'win)
                                                    (pdf-view-goto-page
                                                     (string-to-number (button-get b 'page))
                                                     (button-get b 'win)))))
                                 (if pdf-thumbs-show-page-numbers
                                     (list (if pdf-thumbs-show-page-after
                                               'after-string
                                             'before-string)
                                           (format "%4d " p)) ;either this or help-echo
                                   (list 'help-echo (number-to-string p)))
                                 (when pdf-thumbs-mouse-face
                                   (list 'mouse-face (list :background pdf-thumbs-mouse-face-color))))))
                (when (= (% i columns) (1- columns)) (insert "\n")))
              (goto-char (point-min))

              (unless pdf-thumbs-show-page-numbers
                (add-hook 'post-command-hook #'display-local-help nil t))))))

    (pdf-thumbs-show columns)))

(defun pdf-thumbs-show (columns)
  (let ((win (split-window nil
                           (- (+ (* columns (float (+ pdf-thumbs-width 4 (if pdf-thumbs-show-page-numbers 41 0))))
                                 (- (window-pixel-width)
                                    (window-body-width nil t))))
                           pdf-thumbs-side t)))
    (set-window-buffer win "*thumbs*")
    (set-window-dedicated-p win t)
    (select-window win)))


(define-derived-mode pdf-thumbs-mode special-mode "PDFThumbs")

(define-key pdf-view-mode-map "t" #'pdf-thumbs)

(provide 'pdf-thumbs)

;;; pdf-thumbs.el ends here
