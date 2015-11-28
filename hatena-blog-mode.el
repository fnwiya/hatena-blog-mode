;;; hatena-blog-mode.el --- Hatena Blog API Library for Emacs

;; Author: fnwiya
;; URL: https://github.com/fnwiya/hatena-blog-mode
;; Package-Requires: ()
;; Keywords: hatena-blog

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
;;

;;; Code:

(require 'xml)

(defvar hatena-id nil)
(defvar hatena-blog-api-key  nil)
(defvar hatena-blog-id nil)
(defvar hatena-blog-file-path nil)
(defvar hatena-blog-backup-dir nil)
(defvar hatena-blog-xml-template "
<?xml version='1.0' encoding='utf-8'?>
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

(defun hatena-blog-post2 ()
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
                                   (message "Failed."))))))
    ));

(defun hatena-blog-write ()
  (interactive)
  (find-file hatena-blog-file-path))

(defun hatena-blog-post ()
  (interactive)
  (hatena-blog-post2)
  (write-file (concat hatena-blog-backup-dir (format-time-string "%Y-%m-%d-%H-%M-%S") ".md"))
  (move-file-to-trash hatena-blog-file-path))

(global-set-key (kbd "C-x h") 'hatena-blog-write)
(global-set-key (kbd "C-x P") 'hatena-blog-post)


(provide 'hatena-blog-mode)

;;; hatena-blog-mode.el ends here
