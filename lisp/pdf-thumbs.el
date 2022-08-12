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

(require 'pdf-cache)

(defcustom pdf-cache-thumb-limit 5000
  "Maximum number of cached PNG images per buffer."
  :type 'integer
  :group 'pdf-cache
  :group 'pdf-thumbs)

(defcustom pdf-thumbs-width 175
  "Width of thumbnails in pixels.
In order to limit memory usage, the maximum width is 'hard-coded'
to 400. The size of the cache can be checked using the command
`pdf-thumbs-cache-size'."
  :type 'integer
  :group 'pdf-thumbs)

(defcustom pdf-thumbs-side 'left
  "Create thumbs window at this side.
Side should be a symbol left or right (default)."
  :type 'symbol
  :group 'pdf-thumbs)

(defcustom pdf-thumbs-show-page-numbers nil
  "Maximum number of cached PNG images per buffer."
  :type 'boolean
  :group 'pdf-thumbs)

(defcustom pdf-thumbs-show-page-after nil
  "Maximum number of cached PNG images per buffer."
  :type 'boolean
  :group 'pdf-thumbs)

(defcustom pdf-thumbs-mouse-face nil
  "Maximum number of cached PNG images per buffer."
  :type 'boolean
  :group 'pdf-thumbs)

(defcustom pdf-thumbs-mouse-face-color "blue"
  "Maximum number of cached PNG images per buffer."
  :type 'color
  :group 'pdf-thumbs)

(defvar-local pdf-cache--thumb-cache nil)
(defvar-local pdf-cache--prefetch-thumbs nil)
(defvar-local pdf-thumbs-columns nil)

(defun pdf-cache--fetch-thumbs ()
  "Internal function to prefetch pages and store them in the cache.

WINDOW and IMAGE-WIDTH decide the page and scale of the final image."
  (interactive)
  ;; (when (and (eq window (selected-window))
  ;;            (pdf-util-pdf-buffer-p))
  (dolist (p (number-sequence 1 (pdf-info-number-of-pages)))
    (let* ((data (pdf-info-renderpage p pdf-thumbs-width)))
          (pdf-cache-put-thumb p pdf-thumbs-width data)
          (pdf-view-create-image data))))

(defun pdf-cache-lookup-thumbs (page min-width &optional max-width hash)
  "Return PAGE's cached PNG data as a string or nil.

Return an image of at least MIN-WIDTH and, if non-nil, maximum
width MAX-WIDTH and `eql' HASH value.

Does not modify the cache.  See also `pdf-cache-get-image'."
  (let ((image (car (cl-member
                     (list page min-width max-width hash)
                     pdf-cache--image-cache
                     :test (lambda (spec image)
                             (apply #'pdf-cache--image-match image spec))))))
    (and image
         (pdf-cache--image/data image))))

(defun pdf-cache--prefetch-thumbs (window image-width)
  "Internal function to prefetch pages and store them in the cache.

WINDOW and IMAGE-WIDTH decide the page and scale of the final image."
  (when (and (eq window (selected-window))
             (pdf-util-pdf-buffer-p))
    (let ((page (pop pdf-cache--prefetch-thumbs)))
      (while (and page
                  (pdf-cache-lookup-thumbs
                   page
                   image-width
                   image-width))
        (setq page (pop pdf-cache--prefetch-thumbs)))
      (when (null page)
        (message  "Prefetching done."))
      (when page
        (let* ((buffer (current-buffer))
               (pdf-info-asynchronous
                (lambda (status data)
                  (when (and (null status)
                             (eq window
                                 (selected-window))
                             (eq buffer (window-buffer)))
                    (with-current-buffer (window-buffer)
                      (when (derived-mode-p 'pdf-view-mode)
                        (pdf-cache-put-thumb
                         page image-width data)
                        (pdf-view-create-image data)
                        (message "Prefetched thumb %s." page)
                        ;; Avoid max-lisp-eval-depth
                        (run-with-timer
                         0.001 nil
                         #'pdf-cache--prefetch-thumbs window image-width)))))))
          (condition-case err
              (pdf-info-renderpage page image-width)
            (error
             (pdf-cache-prefetch-thumbs-minor-mode -1)
             (signal (car err) (cdr err)))))))))

(defun pdf-cache-put-thumb (page width data &optional hash)
  "Cache image of PAGE with WIDTH, DATA and HASH.

DATA should the string of a PNG image of width WIDTH and from
page PAGE in the current buffer.  See `pdf-cache-get-image' for
the HASH argument.

This function always returns nil."
  (unless pdf-cache--thumb-cache
    (add-hook 'pdf-info-close-document-hook #'pdf-cache-clear-thumbs nil t))
  (push (pdf-cache--make-image page width data hash)
        pdf-cache--thumb-cache)
  ;; Forget old image(s).
  (when (> (length pdf-cache--thumb-cache)
           pdf-cache-thumb-limit)
    (if (> pdf-cache-thumb-limit 1)
        (setcdr (nthcdr (1- pdf-cache-thumb-limit)
                        pdf-cache--thumb-cache)
                nil)
      (setq pdf-cache--thumb-cache nil)))
  nil)

(defun pdf-cache-clear-thumbs ()
  "Clear the image cache."
  (setq pdf-cache--thumb-cache nil))

(define-minor-mode pdf-cache-prefetch-thumbs-minor-mode
  "Try to load images which will probably be needed in a while."
  :group 'pdf-cache
  (cond
   (pdf-cache-prefetch-thumbs-minor-mode
    (pdf-util-assert-pdf-buffer)
    (let* ((file (buffer-file-name))
           (output-dir (concat "/tmp/"
                              (file-name-as-directory (file-name-base file)))))
      (unless (file-exists-p output-dir)
        (pdf-create-thumb-files file))))))
    ;; (setq pdf-cache--prefetch-thumbs (number-sequence 1 (pdf-info-number-of-pages)))
    ;; (setq pdf-cache--prefetch-thumbs-timer
    ;;       (run-with-idle-timer (or pdf-cache-prefetch-delay 1) t
    ;;                            #'pdf-cache--prefetch-thumbs (selected-window) pdf-thumbs-width)))))

(defun pdf-thumbs-cache-size ()
  (interactive)
  (require 'memory-report)
  (message (file-size-human-readable
            (memory-report-object-size pdf-cache--thumb-cache))))

(defun pdf-thumbs (&optional columns)
  "Show thumbs in a side window.
The number of COLUMNS can be set with a numeric prefix argument."
  (interactive "p")
  (unless pdf-cache--thumb-cache
    (pdf-cache--fetch-thumbs))
    (let ((cache (reverse pdf-cache--thumb-cache))
          (last-page (pdf-info-number-of-pages))
          (win (selected-window)))
      (with-current-buffer (get-buffer-create "*thumbs*")
        (let* ((columns (or columns 1))
               (w 100)
               (h (* w 1.6))
               (source-svg (svg-create w h)))
          (svg-rectangle source-svg 0 0 w h :fill "white")
          (dotimes (i last-page)
            (let* ((p (1+ i))
                   (svg (copy-sequence source-svg))
                   (im (create-image (pdf-cache--image/data (nth i cache))
                                     'png
                                     t
                                     :margin '(2 . 1))))
              (apply #'insert-button (format "%03d " p)
                     (append (list 'page (number-to-string p)
                                   'win win
                                   'face 'default
                                   'display im
                                   'action (lambda (b)
                                             (pdf-view-goto-page
                                              (string-to-number (button-get b 'page))
                                              (button-get b 'win))))
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
          (pop-to-buffer (current-buffer)
                         `(display-buffer-in-direction (side . ,pdf-thumbs-side)))
          (window-resize (selected-window)
                         (- (* columns (float (+ pdf-thumbs-width (if pdf-thumbs-show-page-numbers 45 0))))
                            (window-text-width nil t)) ;negative difference window width and total columns width
                         t nil t)
          (unless pdf-thumbs-show-page-numbers
            (add-hook 'post-command-hook #'display-local-help nil t))))))

