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

;;; Change Log:

;;  0.1.0  2018/11/27  Initial version.

;;; Code:

(require 'json)

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
    (shell-command-to-string
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
      " ")
     )))

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
          (cond
           ((eq head :array)
            (concat "<array>" (plutil-xml-encode-array (car rest) t) "</array>"))
           ((eq head :dict)
            (concat "<dict>" (plutil-xml-encode-dict (car rest) t) "</dict>"))
           ((eq head :bool)
            (let ((value (car rest)))
              (cond
               ((member value '("yes" "no" "true" "false"))
                (format "<%s/>" value))
               (t (signal (format "[plutil] Unknown bool value '%s'" value))))))
           (t (let ((key (substring (symbol-name head) 1))
                    (val (car rest)))
                (format "<%s>%s</%s>" key val key)))
           ))
         (t (plutil-xml-encode head)
            (plutil-xml-encode rest)))))
     (t (cond ((stringp obj) (format "<string>%s</string>" obj))
              ((numberp obj) (format "<integer>%s</integer>" obj))
              (t (signal (format "[plutil] Unknown type of value '%s'" obj))))))))

(defun plutil-xml-encode-dict (alist &optional nowrap)
  "Return a plist XML representation of ALIST.
If NOWRAP not nil, inhibit root wrapper <dict></dict>."
  (let ((key (caar alist))
        (val (cadar alist))
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

(defun plutil-read (file &optional fmt key)
  "Read value from FILE by KEY.
FMT specific ouput format, it can be 'json or 'xml1.
If KEY nil, return all."
  (if key
      (plutil--execute file 'extract key fmt)
    (plutil--execute file 'convert nil fmt)))

(provide 'plutil)

;;; plutil.el ends here
