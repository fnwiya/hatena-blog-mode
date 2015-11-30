;;; hatena-blog-mode.el --- Hatena Blog API Library for Emacs

;; Author: fnwiya
;; URL: https://github.com/fnwiya/hatena-blog-mode
;; Package-Requires: ()
;; Keywords: hatena blog
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
;; var-setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom hatena-id nil
  "User id for Hatena."
  :group 'hatena-blog)
(defcustom hatena-blog-api-key  nil
  "Blog api key for Hatena."
  :group 'hatena-blog)
(defcustom hatena-blog-id nil
  "Blog id for Hatena<XXX.hatenablog.com/>."
  :group 'hatena-blog)
(defcustom hatena-blog-backup-dir nil
  "Backup dir for Hatena."
  :group 'hatena-blog)
(defvar hatena-blog-file-path nil)
(defcustom hatena-blog-editing-mode "md"
  "Editing mode for Hatena."
  :group 'hatena-blog)
(defconst hatena-blog-xml-template "<?xml version='1.0' encoding='utf-8'?>
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
  "Toggle hatena-blog-mode with ARG."
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
  "Key-map for hatena-blog-mode."
  (unless (keymapp hatena-blog-mode-map)
    (setq hatena-blog-mode-map (make-sparse-keymap))
    (setq minor-mode-map-alist
          (cons (cons 'hatena-blog-mode hatena-blog-mode-map)
                minor-mode-map-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hatena-blog-build-xml ()
  "Build xml for hatena-blog."
  (interactive)
  (let ((blog-title (read-string "Title: "
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
                   blog-is-draft))))

(defun hatena-blog-pre-post ()
  "Post-request using hatena-blog-API."
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
                               (cond ((search-forward-regexp "HTTP/1.1 201 Created" nil t)
                                   (message "Entry posted.")
                                   (if (equal hatena-blog-backup-dir nil)
                                       (kill-buffer (concat "hatena-new-entry." hatena-blog-editing-mode))))
                                 (t
                                  (message "Failed."))
                                 ))))));
;;;###autoload
(defun hatena-blog-write ()
  "Write new entry."
  (interactive)
  (hatena-blog-mode t)
  (setq hatena-blog-file-path (concat "~/hatena-new-entry." hatena-blog-editing-mode))
  (find-file hatena-blog-file-path)
  )

;;;###autoload
(defun hatena-blog-post ()
  "Post new entry."
  (interactive)
  (hatena-blog-pre-post)
  (if (equal hatena-blog-backup-dir nil)
      nil
    (defvar hatena-blog-backup-file (concat (format-time-string "%Y-%m-%d-%H-%M-%S") "." hatena-blog-editing-mode))
    (write-file (concat hatena-blog-backup-dir hatena-blog-backup-file))
    (kill-buffer hatena-blog-backup-file))
  (move-file-to-trash hatena-blog-file-path)
  )

;; ;;;###autoload
;; (defun hatena-blog-show ()
;;   (interactive)
;;   )
;;
;; ;;;###autoload
;; (defun hatena-blog-get ()
;;   (interactive)
;;   )
;;
;; ;;;###autoload
;; (defun hatena-blog-put ()
;;   (interactive)
;;   )
;;
;; ;;;###autoload
;; (defun hatena-blog-delete ()
;;   (interactive)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key-binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-hatena-blog-mode-map)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'hatena-blog-mode)

;;; hatena-blog-mode.el ends here
