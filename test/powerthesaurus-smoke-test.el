;;; powerthesaurus-smoke-test.el --- Tests for powerthesaurus -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Valeriy Savchenko (GNU/GPL Licence)

;; This file is NOT part of GNU Emacs.

;; jeison is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; jeison is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with jeison.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'ert)
(require 'powerthesaurus)

(defun powerthesaurus:test-get (term type)
  (let ((results nil))
    (powerthesaurus--query
     term
     type
     (cl-function
      (lambda (&key data &allow-other-keys)
        (setq results (powerthesaurus--response-extract data type))))
     t)
    results))

(ert-deftest powerthesaurus:check-synonyms ()
  (let ((results (powerthesaurus:test-get "good" "synonyms")))
    (should (> (length results) 0))))

(ert-deftest powerthesaurus:check-antonyms ()
  (let ((results (powerthesaurus:test-get "good" "antonyms")))
    (should (> (length results) 0))))

(ert-deftest powerthesaurus:check-related ()
  (let ((results (powerthesaurus:test-get "good" "related")))
    (should (> (length results) 0))))

(ert-deftest powerthesaurus:check-definitions ()
  (let ((results (powerthesaurus:test-get "good" "definitions")))
    (should (> (length results) 0))))

(ert-deftest powerthesaurus:check-sentences ()
  (let ((results (powerthesaurus:test-get "good" "sentences")))
    (should (> (length results) 0))))
