;;; eww-plz.el --- eww plz work!!!                   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Visuwesh

;; Author: Visuwesh <visuweshm@gmail.com>
;; Package-Requires: ((emacs "28.1") (plz "0.7"))
;; Keywords: comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package hacks `eww' to use `plz' for doing most network
;; requests instead of url.el, the latter of which is slow and
;; notorious for dropping images.  See bug#22921, bug#37577 for some
;; details.  You only need to require this file, rest will be setup.
;; Note that enabling this mode will make `eww-retrieve-command' a
;; NOOP.

;; Rather than fixing url.el, we just work around the problem here
;; instead!

;; Thanks to Adam Porter for the solid networking package without
;; which this annoyance would plague humanity till the end of time!

;;; Todo:

;; - Setup a global minor mode instead that will setup the hacks.

;;; Code:
(require 'plz)
(require 'url-http)
(require 'dom)

(defvar eww-plz-curl-args
  ;; --compressed is too risky of a flag since curl behaves strictly
  ;; even for poor servers.  See
  ;; https://github.com/alphapapa/plz.el/issues/46 and
  ;; https://github.com/curl/curl/issues/5200.
  (remove "--compressed" plz-curl-default-args)
  "Default curl args to use for `eww-plz-retrieve'.")

;; 6 is the default value of `url-queue-parallel-processes'.
(defvar eww-plz-img-queue-limit 6
  "Number of images to fetch in parallel.")

(defvar-local eww-plz-img-queue nil
  "Queue for fetching images.")

(defun eww-plz--setup-req-buf (url)
  "Setup plz data buffer for URL to resemble url.el minimally.
It returns the redirection status, if any."
  (let (status)
    (setq-local url-current-object (url-generic-parse-url url))
    (setq-local url-http-end-of-headers (point-marker))
    (widen)
    (save-excursion
      (set-buffer-multibyte nil)
      (url-http-clean-headers)
      (let ((encoding (mail-fetch-field "content-encoding" nil nil nil t)))
        (when (and encoding (equal (downcase encoding) "gzip"))
          (zlib-decompress-region url-http-end-of-headers (point-max) t))))
    (save-excursion
      (save-restriction
        (narrow-to-region (point-min) url-http-end-of-headers)
        (while (= (/ (url-http-parse-response) 100) 3)
          (let ((headers (eww-parse-headers)))
            (delete-region (point-min) (point))
            (and (assoc-default "location" headers)
                 (setq status (list :redirect (assoc-default "location" headers))))))))
    status))

(defun eww-plz--headers ()
  "Return the headers to use for the plz requests."
  `(("Accept" . "text/html, text/plain, text/sgml, text/css, application/xhtml+xml, image/png, */*;q=0.01")
    ("User-Agent" . ,(url-http--user-agent-default-string))
    ,(when (and (fboundp 'zlib-available-p)
                (zlib-available-p))
       (cons "Accept-Encoding" 'gzip))))

(defun eww-plz-retrieve (url callback cbargs)
  "Replacement for `eww-retrieve', which see."
  (require 'url-http)
  (message "Fetching %s..." url)
  (if (plz-queue-p eww-plz-img-queue)
      (plz-clear eww-plz-img-queue)
    (setq-local eww-plz-img-queue (make-plz-queue :limit eww-plz-img-queue-limit)))
  (dlet ((plz-curl-default-args eww-plz-curl-args))
    (plz 'get url :as 'buffer :decode nil
      :headers (eww-plz--headers)
      :connect-timeout 10
      :then (lambda (buffer)
              (with-current-buffer buffer
                (apply callback (eww-plz--setup-req-buf url) cbargs)))
      :else (lambda (error)
              (with-current-buffer (get-buffer-create " *eww-plz-error*")
                (erase-buffer)
                (if (plz-error-response error)
                    (progn
                      (pcase-dolist (`(,header . ,field) (plz-response-headers (plz-error-response error)))
                        (insert (symbol-name header) ": " field "\n"))
                      (insert "\n")
                      (insert (plz-response-body (plz-error-response error))))
                  (insert "Content-type: text/html\n\n")
                  (insert "<title>Failed to fetch URL!</title>")
                  (insert (format "<p>Failed to fetch %s, due to error:</p>" url))
                  (insert (format "<p>%s</p>"
                                  (format "Curl error: %d %s"
                                          (car (plz-error-curl-error error))
                                          (cdr (plz-error-curl-error error))))))
                (apply callback nil cbargs))))))
(advice-add 'eww-retrieve :override #'eww-plz-retrieve)

(defun eww-plz-tag-img (dom &optional url)
  "Replacement for `shr-tag-img', which see."
  (let ((alt (dom-attr dom 'alt))
        (width (shr-string-number (dom-attr dom 'width)))
        (height (shr-string-number (dom-attr dom 'height)))
	(url (shr-expand-url (or url (shr--preferred-image dom)))))
    ;; These must be handled by shr.el itself!
    (if (or shr-inhibit-images
            (null url)
            (member (dom-attr dom 'height) '("0" "1"))
            (member (dom-attr dom 'width) '("0" "1"))
            (string-match "\\`data:" url)
            (string-match "\\`cid:" url)
            (shr-image-blocked-p url)
            (url-is-cached url))
        (shr-tag-img dom url)
      ;; Create the queue here too, if not already alive.  It might
      ;; help in replacing `shr-tag-img' globally.
      (unless (plz-queue-p eww-plz-img-queue)
        (setq-local eww-plz-img-queue (make-plz-queue :limit eww-plz-img-queue-limit)))
      ;; TODO: Too much code duplication.  :-(
      (when (and (not shr-max-inline-image-size)
                 (> (current-column) 0))
        (insert "\n"))
      (let ((shr-buf (current-buffer))
            (start (point-marker))
            end)
        (and (length= alt 0) (setq alt "*"))
        (when (image-type-available-p 'svg)
          (insert-image
           (shr-make-placeholder-image dom)
           (or (string-trim alt) "")))
        (unless shr-max-inline-image-size
	  (insert " "))
        (setq end (point-marker))
        (dlet ((plz-curl-default-args eww-plz-curl-args))
          (plz-run (plz-queue eww-plz-img-queue
                     'get url
                     :headers (eww-plz--headers)
                     :as 'buffer
                     :decode nil
                     ;; TODO: Error handling lol.
                     ;; :else (lambda (error) (message "%S" error))
                     :then (lambda (buffer)
                             (with-current-buffer buffer
                               (eww-plz--setup-req-buf url)
                               (apply #'shr-image-fetched nil shr-buf
                                      start end
                                      (list (list :width width :height height))))))))
        (when (zerop shr-table-depth) ;; We are not in a table.
	  (put-text-property start (point) 'keymap shr-image-map)
	  (put-text-property start (point) 'shr-alt alt)
	  (put-text-property start (point) 'image-url url)
	  (put-text-property start (point) 'image-displayer
			     (shr-image-displayer shr-content-function))
	  (put-text-property start (point) 'help-echo
			     (shr-fill-text
			      (or (dom-attr dom 'title) alt))))))))

(defun eww-plz--hook ()
  "Setup the image tag routines to use the plz backed ones."
  (setq-local shr-external-rendering-functions
              (cons (cons 'img #'eww-plz-tag-img)
                    shr-external-rendering-functions))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when (plz-queue-p eww-plz-img-queue)
                (plz-clear eww-plz-img-queue)))
            nil t))
(add-hook 'eww-mode-hook #'eww-plz--hook)

(provide 'eww-plz)
;;; eww-plz.el ends here
