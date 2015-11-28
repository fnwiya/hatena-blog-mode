;;; hatena-blog-mode.el --- Hatena Blog API Library for Emacs

;; Author: fnwiya
;; URL: https://github.com/fnwiya/hatena-blog-mode
;; Package-Requires: ()
;; Keywords: hatena-blog
;; Version: 0.1

;; Copyright (c) 2015 fnwiya
;;
;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This program is a tool for hatena-blog (http://hatenablog.com/).

;;; Usage:

;;
;; (require 'hatena-blog-mode.el)
;; (setq hatena-id "XXXXXXXX")
;; (setq hatena-blog-api-key  "XXXXXXXX")
;; (setq hatena-blog-id "XXXXXXXX")
;; set if you want to backup your post.
;; (setq hatena-blog-backup-dir "XXXXXXXX")


;;; Code:

(require 'xml)
(require 'url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AtomPubSetting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar hatena-id nil
  "User id for Hatena.")
(defvar hatena-blog-api-key  nil
  "Blog api key for Hatena.")
(defvar hatena-blog-id nil
  "Blog id for Hatena<XXX.hatenablog.com/>.")
(defvar hatena-blog-file-path nil)
(defvar hatena-blog-backup-dir nil)
(defvar hatena-blog-xml-template nil)
(setq hatena-blog-file-path "~/hatena-post.md")
(setq hatena-blog-xml-template "<?xml version='1.0' encoding='utf-8'?>
<entry xmlns='http://www.w3.org/2005/Atom'
       xmlns:app='http://www.w3.org/2007/app'>
  <title>%s</title>
  <author><name>%s</name></author>
  <content type='text/plain'>%s</content>
  <updated>%s</updated>
  <category term='%s' />
  <app:control>
    <app:draft>%s</app:draft>
  </app:control>
</entry>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minor-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar hatena-blog-mode nil)
(defvar hatena-blog-mode-map nil)

(if (not (assq 'hatena-blog-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons '(hatena-blog-mode "Hatena-blog-mode")
                minor-mode-alist)))

(defun hatena-blog-mode (&optional arg)
  "hatena-blog-mode"
  (interactive)
  (cond
   ((< (prefix-numeric-value arg) 0)
    (setq hatena-blog-mode nil))
   (arg
    (setq hatena-blog-mode t))
   (t
    (setq hatena-blog-mode (not hatena-blog-mode))))
  (if hatena-blog-mode
    nil))

(defun define-hatena-blog-mode-map ()
  (unless (keymapp hatena-blog-mode-map)
    (setq hatena-blog-mode-map (make-sparse-keymap))
    (setq minor-mode-map-alist
          (cons (cons 'hatena-blog-mode hatena-blog-mode-map)
                minor-mode-map-alist))))
(define-hatena-blog-mode-map)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hatena-blog-build-xml ()
  (interactive)
  (let (
        (blog-title (read-string "Title: "
                                 (save-excursion (goto-char (point-min))
                                                 (search-forward-regexp "#* \\(.*\\)" nil t)
                                                 (match-string 1))))
        (blog-category (read-string "Category: "))
        (blog-is-draft (if (y-or-n-p "Send as draft? ") "yes" "no"))
        )
    (princ (format hatena-blog-xml-template
                   (xml-escape-string blog-title)
                   hatena-id
                   (xml-escape-string (buffer-string))
                   (format-time-string "%Y-%m-%dT%H:%M:%S")
                   (xml-escape-string blog-category)
                   blog-is-draft))
    ))

(defun hatena-blog-pre-post ()
  (interactive)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/x-www-form-urlencoded")
            ("Authorization" . ,(concat "Basic " (base64-encode-string (concat hatena-id ":" hatena-blog-api-key))))))
         (url-request-data
          (encode-coding-string (hatena-blog-build-xml) 'utf-8))
         (post-url (format "https://blog.hatena.ne.jp/%s/%s/atom/entry" hatena-id hatena-blog-id)))

    (url-retrieve post-url (lambda (data)
                             (with-current-buffer (current-buffer)
                               (if (search-forward-regexp "HTTP/1.1 201 Created" nil t)
                                   (message "Entry posted.")
                                 (progn
                                   (message "Failed.")))
                               )))
    ));

(defun hatena-blog-write ()
  (interactive)
  (hatena-blog-mode t)
  (find-file hatena-blog-file-path)
  )

(defun hatena-blog-post ()
  (interactive)
  (hatena-blog-pre-post)
  (if (equal hatena-blog-backup-dir nil)
      nil
    (write-file (concat hatena-blog-backup-dir (format-time-string "%Y-%m-%d-%H-%M-%S") ".md")))
  (move-file-to-trash hatena-blog-file-path)
  )


(defun hatena-blog-show ()
  (interactive)
  )

(defun hatena-blog-get ()
  (interactive)
  )

(defun hatena-blog-put ()
  (interactive)
  )

(defun hatena-blog-delete ()
  (interactive)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key-binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'hatena-blog-mode)

;;; hatena-blog-mode.el ends here
