[![Build Status](https://travis-ci.com/twlz0ne/plutil.el.svg?branch=master)](https://travis-ci.com/twlz0ne/plutil.el)

## plutil.el

Some utility functions to help read/write Apple plist file.

## Installation

Clone this repository to directory `~/.emacs.d/site-lisp/plutil/`, for example, and add this to your .emacs

```elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/plutil"))
(require 'plutil)
```

## Usage

Generate plist xml using sexps with function `plutil-xml-encode`:

```elisp

(plutil-xml-encode "foo")           ;; => <string>foo</string>
(plutil-xml-encode 123)             ;; => <integer>123</integer>
(plutil-xml-encode '(:bool "true")) ;; => <true/>

(plutil-xml-encode '(:array ("foo" "bar")))
;; =>
;; <array>
;;   <string>foo</string>
;;   <string>bar</string>
;; </array>

(plutil-xml-encode '(:dict (("foo" 1) ("bar" 2))))
;; =>
;; <dict>
;;   <key>foo</key>
;;   <integer>1</integer>
;;   <key>bar</key>
;;   <integer>2</integer>
;; </dict>

(plutil-xml-encode
  '(:array
    ("foo"
     "bar"
     (:date "2018-11-28T06:42:23Z")
     (:dict
      (("qux" 3)
       ("outdated" (:bool "true")))))))
;; =>
;; <array>
;;   <string>foo</string>
;;   <string>bar</string>
;;   <date>2018-11-28T06:42:23Z</date>
;;   <dict>
;;     <key>qux</key>
;;     <integer>3</integer>
;;     <key>outdated</key>
;;     <true/>
;;   </dict>
;; </array>
```

Insert a value in json/xml(recommend) form to plist file at `<KEYPATH>`:

```elisp
(plutil-insert
 (expand-file-name "/path/to/test.plist")
 "<KEYPATH>"
 'xml
 (plutil-xml-encode
  '(:array
    ...
    )))
```

Read value from plist file by `<KEYPATH>`:

```elisp
(plutil-read
 (expand-file-name "/path/to/test.plist")
 "<KEYPATH>"
 'json)
```

Other functions:

- `plutil-create`
- `plutil-update`
- `plutil-delete`
