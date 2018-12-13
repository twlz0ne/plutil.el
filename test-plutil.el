;;; test-plutil.el --- Test plutil -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Gong Qijian <gongqijian@gmail.com>

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

;;; Code:

(require 'ert)
(require 'plutil)

;;; utils

(defun -xml-ugly (xml-string)
  "Make XML-STRING ugly."
  (mapconcat
   (lambda (string)
     (replace-regexp-in-string "^[ ]*" "" string))
   (split-string xml-string "\n")
   ""))

(defun -create-testfile (&optional init-data)
  "Create test file with optional xml INIT-DATA."
  (let* ((tmpdir (make-temp-file "test-plutil--" 'tmpdir "/"))
         (test-file (concat tmpdir "test.plist")))
    (plutil-create test-file init-data)
    test-file))

(defun -sort-json-dict (json &optional alistp)
  "Sort JSON by dict value.
If ALISTP not nil, treat JSON as an alist."
  (let ((al (if alistp json (json-read-from-string json))))
      (json-encode-alist
       (sort al
             (lambda (a b)
               (< (cdr a) (cdr b)))))))

;;; encode

;; value

(ert-deftest test-plutil-xml-encode-integer-value ()
  (should
   (equal
    "<integer>1</integer>"
    (plutil-xml-encode 1))))

(ert-deftest test-plutil-xml-encode-string-value ()
  (should
   (equal
    "<string>foo</string>"
    (plutil-xml-encode "foo"))))

(ert-deftest test-plutil-xml-encode-bool-value ()
  (should (equal "<true/>" (plutil-xml-encode '(:bool "true"))))
  (should (equal "<true/>" (plutil-xml-encode '(:bool "True"))))
  (should (equal "<true/>" (plutil-xml-encode '(:bool "TRUE"))))
  (should (equal "<true/>" (plutil-xml-encode '(:bool "yes"))))
  (should (equal "<true/>" (plutil-xml-encode '(:bool "Yes"))))
  (should (equal "<true/>" (plutil-xml-encode '(:bool "YES"))))
  (should (equal "<false/>" (plutil-xml-encode '(:bool "false"))))
  (should (equal "<false/>" (plutil-xml-encode '(:bool "False"))))
  (should (equal "<false/>" (plutil-xml-encode '(:bool "FALSE"))))
  (should (equal "<false/>" (plutil-xml-encode '(:bool "no"))))
  (should (equal "<false/>" (plutil-xml-encode '(:bool "No"))))
  (should (equal "<false/>" (plutil-xml-encode '(:bool "NO"))))
  (should
   (equal
    '(plutil-invalid-bool-value "OK")
    (condition-case err
        (plutil-xml-encode '(:bool "OK"))
      (error err)))))

(ert-deftest test-plutil-xml-encode-date-value ()
  (should
   (equal
    "<date>2018-11-28T06:42:23Z</date>"
    (plutil-xml-encode '(:date "2018-11-28T06:42:23Z"))))
  (should
   (equal
    "<date>2018-11-28T06:42Z</date>"
    (plutil-xml-encode '(:date "2018-11-28T06:42Z"))))
  (should (equal
    "<date>2018-11-28T06Z</date>"
    (plutil-xml-encode '(:date "2018-11-28T06Z"))))
  (should
   (equal
    '(plutil-invalid-date-value "2018-11-28T")
    (condition-case err
        (plutil-xml-encode '(:date "2018-11-28T"))
      (error err)))))

(ert-deftest test-plutil-xml-encode-data-value ()
  (should
   (equal
    "<data>Zm9vYmFy</data>"
    (plutil-xml-encode '(:data "Zm9vYmFy"))))
  (should
   (equal
    '(plutil-invalid-data-value "foobar")
    (condition-case err
        (plutil-xml-encode '(:data "foobar"))
      (error err)))))

;; array

(ert-deftest test-plutil-xml-encode-array-nested-0 ()
  (should
   (equal
    (-xml-ugly
     "<array>
        <integer>1</integer>
        <integer>2</integer>
        <integer>3</integer>
      </array>")
    (plutil-xml-encode-array '(1 2 3)))))

(ert-deftest test-plutil-xml-encode-array-nested-1 ()
  (should
   (equal
    (-xml-ugly
     "<array>
        <integer>1</integer>
        <array>
          <integer>2</integer>
          <integer>3</integer>
        </array>
      </array>")
    (plutil-xml-encode-array '(1 (:array (2 3)))))))

(ert-deftest test-plutil-xml-encode-array-nested-2 ()
  (should
   (equal
    (-xml-ugly
     "<array>
        <integer>1</integer>
        <array>
          <integer>2</integer>
          <array>
            <integer>3</integer>
          </array>
        </array>
      </array>")
    (plutil-xml-encode-array '(1 (:array (2 (:array (3)))))))))

;; dict

(ert-deftest test-plutil-xml-encode-dict-nested-0 ()
  (should
   (equal
    (-xml-ugly
     "<dict>
        <key>foo</key>
        <integer>1</integer>
        <key>bar</key>
        <integer>2</integer>
        <key>qux</key>
        <integer>3</integer>
      </dict>")
    (plutil-xml-encode-dict '((foo 1) (bar 2) (qux 3))))))

(ert-deftest test-plutil-xml-encode-dict-nested-1 ()
  (should
   (equal
    (-xml-ugly
     "<dict>
        <key>foo</key>
        <dict>
          <key>bar</key>
          <integer>2</integer>
          <key>qux</key>
          <integer>3</integer>
        </dict>
      </dict>")
    (plutil-xml-encode-dict '((foo (:dict ((bar 2) (qux 3)))))))))

(ert-deftest test-plutil-xml-encode-dict-nested-2 ()
  (should
   (equal
    (-xml-ugly
     "<dict>
        <key>foo</key>
        <dict>
          <key>bar</key>
          <dict>
            <key>qux</key>
            <integer>3</integer>
          </dict>
        </dict>
      </dict>")
    (plutil-xml-encode-dict '((foo (:dict ((bar (:dict ((qux 3))))))))))))

;; mixed

(ert-deftest test-plutil-xml-encode-mixed ()
  (should
   (equal
    (-xml-ugly
     "<array>
        <string>foo</string>
        <string>bar</string>
        <date>2018-11-28T06:42:23Z</date>
        <dict>
          <key>qux</key>
          <integer>3</integer>
          <key>outdated</key>
          <true/>
        </dict>
      </array>")
    (plutil-xml-encode
     '(:array
       ("foo"
        "bar"
        (:date "2018-11-28T06:42:23Z")
        (:dict
         (("qux" 3)
          ("outdated" (:bool "yes"))) )))))))

;;; decode

(ert-deftest test-plutil-xml1-purge ()
  (should
   (equal
    (-xml-ugly
     "<array>
        <string>foo</string>
        <string>bar</string>
        <date>2018-11-28T06:42:23Z</date>
        <data>Zm9vYmFy</data>
        <real>1.23</real>
        <dict>
          <key>qux</key>
          <integer>3</integer>
          <key>outdated</key>
          <true/>
        </dict>
      </array>")
    (plutil-xml1-purge
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
      <!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
      <plist version=\"1.0\">
      <array>
        <string>foo</string>
        <string>bar</string>
        <date>2018-11-28T06:42:23Z</date>
        <data>Zm9vYmFy</data>
        <real>1.23</real>
        <dict>
          <key>qux</key>
          <integer>3</integer>
          <key>outdated</key>
          <true/>
        </dict>
      </array>
      </plist>"))))

(ert-deftest test-plutil-xml1-purge-noheader ()
  (should
   (equal
    (-xml-ugly
     "<array>
        <string>foo</string>
        <string>bar</string>
        <date>2018-11-28T06:42:23Z</date>
        <data>Zm9vYmFy</data>
        <real>1.23</real>
        <dict>
          <key>qux</key>
          <integer>3</integer>
          <key>outdated</key>
          <true/>
        </dict>
      </array>")
    (plutil-xml1-purge
     "<array>
        <string>foo</string>
        <string>bar</string>
        <date>2018-11-28T06:42:23Z</date>
        <data>Zm9vYmFy</data>
        <real>1.23</real>
        <dict>
          <key>qux</key>
          <integer>3</integer>
          <key>outdated</key>
          <true/>
        </dict>
      </array>
      </plist>"
     t))))

(ert-deftest test-plutil-xml1-decode ()
  (should
   (equal
    '(:array
      ("foo"
       "bar"
       (:date "2018-11-28T06:42:23Z")
       (:data "Zm9vYmFy")
       (:float 1.23)
       (:dict
        (("qux" 3)
         ("outdated" (:bool "true"))))))
    (plutil-xml1-decode
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
      <!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
      <plist version=\"1.0\">
      <array>
        <string>foo</string>
        <string>bar</string>
        <date>2018-11-28T06:42:23Z</date>
        <data>Zm9vYmFy</data>
        <real>1.23</real>
        <dict>
          <key>qux</key>
          <integer>3</integer>
          <key>outdated</key>
          <true/>
        </dict>
      </array>
      </plist>")
    )))

(ert-deftest test-plutil-xml1-decode-noheader ()
  (should
   (equal
    '(:array
      ("foo"
       "bar"
       (:date "2018-11-28T06:42:23Z")
       (:data "Zm9vYmFy")
       (:float 1.23)
       (:dict
        (("qux" 3)
         ("outdated" (:bool "true"))))))
    (plutil-xml1-decode
     "<array>
        <string>foo</string>
        <string>bar</string>
        <date>2018-11-28T06:42:23Z</date>
        <data>Zm9vYmFy</data>
        <real>1.23</real>
        <dict>
          <key>qux</key>
          <integer>3</integer>
          <key>outdated</key>
          <true/>
        </dict>
      </array>
      </plist>"
     t))))

;;; read & write

(ert-deftest test-plutil-create ()
  (should
   (file-exists-p (-create-testfile))))

(ert-deftest test-plutil-create-with-initdata ()
  (should
   (file-exists-p
    (-create-testfile
     "<key>array0</key>
      <array>
        <integer>1</integer>
        <integer>2</integer>
        <integer>3</integer>
      </array>"))))

(ert-deftest test-plutil-read-array ()
  (let ((test-file
         (-create-testfile
          "<key>array0</key>
           <array>
             <integer>1</integer>
             <integer>2</integer>
             <integer>3</integer>
           </array>")))
    (should (equal "[1,2,3]" (plutil-read test-file "array0" 'json)))))

(ert-deftest test-plutil-read-nested-array ()
  (let ((test-file
         (-create-testfile
          "<key>array0</key>
           <array>
             <integer>1</integer>
             <array>
               <integer>2</integer>
               <integer>3</integer>
             </array>
           </array>")))
    (should (equal "[2,3]" (plutil-read test-file "array0.1" 'json)))))

(ert-deftest test-plutil-read-dict ()
  (let ((test-file
         (-create-testfile
          "<key>dict0</key>
           <dict>
             <key>foo</key><integer>1</integer>
             <key>bar</key><integer>2</integer>
             <key>qux</key><integer>3</integer>
           </dict>")))
    (should (equal "{\"foo\":1,\"bar\":2,\"qux\":3}" (-sort-json-dict (plutil-read test-file "dict0" 'json))))))

(ert-deftest test-plutil-read-nested-dict ()
  (let ((test-file
         (-create-testfile
          "<key>dict0</key>
           <dict>
             <key>foo</key>
             <dict>
               <key>bar</key><integer>2</integer>
               <key>qux</key><integer>3</integer>
             </dict>
           </dict>
")))
    (should (equal "{\"bar\":2,\"qux\":3}" (-sort-json-dict (plutil-read test-file "dict0.foo" 'json))))))

(ert-deftest test-plutil-insert-array ()
  (let ((test-file (-create-testfile))
        (test-data (plutil-xml-encode '(:array (1 2 3)))))
    (should (plutil-insert test-file "array1" 'xml test-data))
    (should (equal "[1,2,3]" (plutil-read test-file "array1" 'json)))))

(ert-deftest test-plutil-insert-dict ()
  (let ((test-file (-create-testfile))
        (test-data (plutil-xml-encode '(:dict ((foo 1) (bar 2) (qux 3))))))
    (should (plutil-insert test-file "dict1" 'xml test-data))
    (should (equal "{\"foo\":1,\"bar\":2,\"qux\":3}" (-sort-json-dict (plutil-read test-file "dict1" 'json))))))

(ert-deftest test-plutil-replace-array ()
  (let ((test-file
         (-create-testfile
          "<key>array0</key>
           <array>
             <integer>1</integer>
             <integer>2</integer>
             <integer>3</integer>
           </array>")))
    (should (plutil-update test-file "array0" 'xml (plutil-xml-encode '(:array (11 22 33)))))
    (should (equal "[11,22,33]" (plutil-read test-file "array0" 'json)))))

(ert-deftest test-plutil-replace-dict ()
  (let ((test-file
         (-create-testfile
          "<key>dict0</key>
           <dict>
             <key>foo</key><integer>1</integer>
             <key>bar</key><integer>2</integer>
             <key>qux</key><integer>3</integer>
           </dict>")))
    (should (plutil-update test-file "dict0" 'xml (plutil-xml-encode '(:dict ((foo 11) (bar 22) (qux 33))))))
    (should (equal "{\"foo\":11,\"bar\":22,\"qux\":33}" (-sort-json-dict (plutil-read test-file "dict0" 'json))))))

(ert-deftest test-plutil-delete-array ()
  (let ((test-file
         (-create-testfile
          "<key>array0</key>
           <array>
             <integer>1</integer>
             <integer>2</integer>
             <integer>3</integer>
           </array>
           <key>dict0</key>
           <dict>
             <key>foo</key><integer>1</integer>
             <key>bar</key><integer>2</integer>
             <key>qux</key><integer>3</integer>
           </dict>")))
    (should (plutil-delete test-file "array0"))
    (let* ((json (json-read-from-string (plutil-read test-file nil 'json)))
           (sorted-string (concat (format "{\"%s\":" (caar json)) (-sort-json-dict (cdar json) t) "}")))
      (should (equal "{\"dict0\":{\"foo\":1,\"bar\":2,\"qux\":3}}" sorted-string)))))

(ert-deftest test-plutil-delete-dict ()
  (let ((test-file
         (-create-testfile
          "<key>array0</key>
           <array>
             <integer>1</integer>
             <integer>2</integer>
             <integer>3</integer>
           </array>
           <key>dict0</key>
           <dict>
             <key>foo</key><integer>1</integer>
             <key>bar</key><integer>2</integer>
             <key>qux</key><integer>3</integer>
           </dict>")))
    (should (plutil-delete test-file "dict0"))
    (should (equal "{\"array0\":[1,2,3]}" (plutil-read test-file nil 'json)))))

;; test-plutil.el ends here
