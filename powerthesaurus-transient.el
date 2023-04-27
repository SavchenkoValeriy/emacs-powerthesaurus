;;; powerthesaurus.el --- Powerthesaurus integration -*- lexical-binding: t; -*-

;; Copyright (c) 2018-2023 Valeriy Savchenko (GNU/GPL Licence)

;; Authors: Valeriy Savchenko <sinmipt@gmail.com>
;; URL: http://github.com/SavchenkoValeriy/emacs-powerthesaurus

;; This file is NOT part of GNU Emacs.

;; powerthesaurus.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; powerthesaurus.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with powerthesaurus.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ===============================================================
;; UI shortcuts
;; ===============================================================


;;; Code:

(require 'powerthesaurus)
(require 'transient)


;;;###autoload
(autoload #'powerthesaurus-transient "powerthesaurus-transient" nil t)

(transient-define-prefix powerthesaurus-transient ()
  "Transient for Power Thesaurus."
  [["Similarity"
    ("s" "Synonyms" powerthesaurus-lookup-synonyms-dwim)
    ("a" "Antonyms" powerthesaurus-lookup-antonyms-dwim)
    ("r" "Related Words" powerthesaurus-lookup-related-dwim)]
   ["Information"
    ("d" "Definitions" powerthesaurus-lookup-definitions-dwim)
    ("e" "Example Sentences" powerthesaurus-lookup-sentences-dwim)]])

(provide 'powerthesaurus-transient)

;;; powerthesaurus-transient.el ends here
