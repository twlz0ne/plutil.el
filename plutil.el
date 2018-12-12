;;; plutil.el --- Apple plist file utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2018/11/27
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/twlz0ne/plutil
;; Keywords: apple, osx, files, plist, xml

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

;; Some utility functions to help read/write Apple plist file.
;; See more at README.md

;;; Change Log:

;;  0.1.0  2018/11/27  Initial version.

;;; Code:

(require 'json)
(require 'timezone)

;;; internal

(defun plutil--execute (file cmd &optional key type value)
  "Execute CMD on FILE.

KEY        <key>[.(index|key)...]

# read

 TYPE    |  VALUE
-----------------------------------------------
'xml1    | an XML property list
'json    | a JSON fragment

# write

 TYPE    |  VALUE
-----------------------------------------------
'bool    | YES if passed \"YES\" or \"true\", otherwise NO
'integer | any valid 64 bit integer
'float   | any valid 64 bit float
'string  | UTF8 encoded string
'date    | a date in XML property list format, not supported if outputting JSON
'data    | a base-64 encoded string
'xml     | an XML property list, useful for inserting compound values
'json    | a JSON fragment, useful for inserting compound values"
  (let* ((readp (cond ((memq cmd '(convert extract)) t)
                      ((memq cmd '(insert replace remove)) nil)
                      (t (signal (format "[plutil] Unknown command '%s'!" cmd))))))
    (unless (and (and readp (memq type '(json xml1)))
                 (and (not readp) (memq type '(bool integer float string data date xml json)))
              (signal (format "[plutil] Unknown type '%s'!" type))))
    (let* (no-error
           (command
            (mapconcat
             'identity
             `("plutil"
               ,(format "-%s" cmd) ;; ---------------------------------- cmd
               ,(and key (format "'%s'" key)) ;; ----------------------- key
               ,@(and type (cond (readp (list  ;; ---------------------- [read]
                                         (format "%s" type) ;; --------- output format
                                         "-o -"))  ;; ------------------ output path
                                 (t     (list ;; ----------------------- [write]
                                         (format "-%s" type) ;; -------- input format
                                         (format "'%s'" value))))) ;; -- input data
               ,file) ;; ----------------------------------------------- file
             " "))
           (output
            (with-output-to-string
              (with-current-buffer standard-output
                (setq no-error (= 0 (call-process-shell-command command nil standard-output)))))))
      (if no-error
          output
        (signal 'error (list (format "[plutil] shell command error: %s" output)))))))

;;; encode

(defun plutil-xml-encode (obj)
  "Convert OBJ as a plist XML representation."
  (when obj
    (cond
     ((listp obj)
      (let ((head (car obj))
            (rest (cdr obj)))
        (cond
         ((keywordp head)
          (let ((val (car rest)))
            (cond
             ((eq head :array)
              (concat "<array>" (plutil-xml-encode-array (car rest) t) "</array>"))
             ((eq head :dict)
              (concat "<dict>" (plutil-xml-encode-dict (car rest) t) "</dict>"))
             ((eq head :bool)
              (cond
               ((member (downcase val) '("yes" "true"))
                (format "<true/>"))
               ((member (downcase val) '("no" "false"))
                (format "<false/>"))
               (t (signal 'error (list (format "[plutil] Invalid bool value '%s'" val))))))
             ((eq head :date)
              (let ((timev (timezone-parse-date val)))
                (cond
                 ((elt (reverse (append timev nil)) 0)
                  (format "<date>%s</date>" val))
                 (t (signal 'error (list (format "[plutil] Expected an ISO 8601 formatted string bug got '%s'" val)))))))
             ((eq head :data)
              (condition-case err
                  (progn
                    (base64-decode-string val)
                    (format "<data>%s</data>" val))
                (error (signal 'error (list (format "[plutil] Expected a base64 string but got '%s'" val))))))
             (t (let ((key (substring (symbol-name head) 1)))
                  (format "<%s>%s</%s>" key val key)))
             )))
         (t (plutil-xml-encode head)
            (plutil-xml-encode rest)))))
     (t (cond ((stringp obj) (format "<string>%s</string>" obj))
              ((numberp obj) (format "<integer>%s</integer>" obj))
              (t (signal (format "[plutil] Unknown type of value '%s'" obj))))))))

(defun plutil-xml-encode-dict (alist &optional nowrap)
  "Return a plist XML representation of ALIST.
If NOWRAP not nil, inhibit root wrapper <dict></dict>."
  (let ((key (caar alist))
        (val (car (cdar alist)))
        (rest (cdr alist)))
    (concat (unless nowrap "<dict>")
            (format "<key>%s</key>" key)
            (plutil-xml-encode val)
            (when rest
              (plutil-xml-encode-dict rest t))
            (unless nowrap "</dict>"))))

(defun plutil-xml-encode-array (array &optional nowrap)
  "Return a plist XML representation of ARRAY.
If NOWRAP not nil, inhibit root wrapper <array></array>."
  (let ((val (car array))
        (rest (cdr array)))
    (concat (unless nowrap "<array>")
            (plutil-xml-encode val)
            (when rest
              (plutil-xml-encode-array rest t))
            (unless nowrap "</array>"))))

;;; decode

(defun plutil--xml1-parse (xml1str &optional noheader)
  "Parse XML1STR.
If NOHEADER no nil, skip header checking,
Data nodes at `(nth 3 xml1obj)'."
  (with-temp-buffer
    (insert xml1str)
    (let ((xml1obj (car (xml-parse-region (point-min) (point-max)))))
      (unless (or noheader
                  (and (eq (nth 0 xml1obj) 'plist)
                       (equal (nth 1 xml1obj) '((version . "1.0")))
                       (string-match-p "\\`[\n\s\t]*\\'" (nth 2 xml1obj))))
        (signal 'error (list (format "[plutil] Invalid plist xml1 string: %s" xml1str))))
      xml1obj)))

(defun plutil--xml1-purge (xml1nodes)
  "Convert XML1NODES to string, remove nil or blanks."
  (let ((root (car xml1nodes))
        (rest (cdr xml1nodes)))
    (cond ((or (eq root 'true)
               (eq root 'false))
           (format "<%s/>" root))
          (t
           (concat
            (format "<%s>" root)
            (mapconcat
             'identity
             (mapcar (lambda (node)
                       (cond ((and (listp node) (not (eq node nil)))
                              (plutil--xml1-purge node))
                             ((or (not node) (string-match-p "\\`[\n\s\t]*\\'" node)) nil)
                             (t (format "%s" node))))
                     rest)
             "")
            (format "</%s>" root))))))

(defun plutil--xml1-decode (xml1nodes)
  "Decode XML1NODES, return a list."
  (when (and (listp xml1nodes) xml1nodes)
    (let ((head (car xml1nodes))
          (rest (cdr xml1nodes)))
      (cond ((memq head '(true false)) (list (intern ":bool") (format "%s" head)))
            ((eq head 'real) (list (intern ":float") (string-to-number (cadr rest))))
            ((eq head 'data) (list (intern ":data") (cadr rest)))
            ((eq head 'date) (list (intern ":date") (cadr rest)))
            ((eq head 'integer) (string-to-number (cadr rest)))
            ((eq head 'string) (cadr rest))
            ((eq head 'key) (cadr rest))
            ((eq head 'array)
             (list :array
                   (let ((array '()))
                     (while rest
                       (let ((elm (car rest)))
                         (setq rest (cdr rest))
                         (when (and elm (listp elm))
                           (setq array (append array (list (plutil--xml1-decode elm)))))))
                     array)))
            ((eq head 'dict)
             (list :dict
                   (let ((dict '()))
                     (while rest
                       (let ((keynode nil)
                             (valnode nil))
                         (while (and rest (not (consp keynode)))
                           (setq keynode (car rest))
                           (setq rest (cdr rest)))
                         (while (and rest (not (consp valnode)))
                           (setq valnode (car rest))
                           (setq rest (cdr rest)))
                         (when (and (consp keynode) (consp valnode))
                           (setq dict
                                 (append
                                  dict
                                  (list
                                   (list
                                    (plutil--xml1-decode keynode)
                                    (plutil--xml1-decode valnode))))))))
                     dict)))
            (t (signal 'error (list (format "[plutil] Invalid plist xml1 nodes: %s" rest))))))))

(defun plutil-xml1-purge (xml1str &optional noheader)
  "Purge XML1STR, remove header and blanks."
  (plutil--xml1-purge
   (if noheader
       (plutil--xml1-parse xml1str t)
     (nth 3 (plutil--xml1-parse xml1str)))))

(defun plutil-xml1-decode (xml1str &optional noheader)
  "Convert XML1STR to list."
  (plutil--xml1-decode
   (if noheader
       (plutil--xml1-parse xml1str t)
     (nth 3 (plutil--xml1-parse xml1str)))))

;;; read & write

(defun plutil-create (file &optional init-data)
  "Create an empty apple plist FILE with optional xml INIT-DATA string.
Return no-nil if success."
  (if (file-exists-p file)
      (signal (format "File '%s' already exists!" file))
    (with-temp-buffer
      (insert (concat "\
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
<dict>"
                      init-data "\
</dict>
</plist>"))
      (write-region (point-min) (point-max) file))
    file))

(defun plutil-insert (file key type value)
  "Insert KEY/VALUE paire to FILE."
  (plutil--execute
   file 'insert key type value))

(defun plutil-update (file key type value)
  "Update KEY/VALUE paire in FILE."
  (plutil--execute
   file 'replace key type value))

(defun plutil-delete (file key)
  "Delete KEY/value pair from FILE."
  (plutil--execute file 'remove key))

(defun plutil-read (file &optional key fmt)
  "Read value from FILE by KEY.
If KEY nil, return all.
FMT specific ouput format, it can be 'json(default) or 'xml1.

:FIXME
Because the XML1 format carries additional header information,
JSON is used as the default format here to get a more readable output,
but sometimes it will return an error, E.g:

    $ plutil -convert json -o - ~/Library/Preferences/com.github.GitHub.plist
    /Users/$USER/Library/Preferences/com.github.GitHub.plist: invalid object in plist for destination format"
  (if key
      (plutil--execute file 'extract key (or fmt 'json))
    (plutil--execute file 'convert nil (or fmt 'json))))

(provide 'plutil)

;;; plutil.el ends here
