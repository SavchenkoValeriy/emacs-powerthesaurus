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


;;; Transient

;;;###autoload
(autoload #'powerthesaurus-transient "powerthesaurus-ui-shortcuts" nil t)

(when (require 'transient nil :noerror)
  (transient-define-prefix powerthesaurus-transient ()
    "Transient for Power Thesaurus."
    [["Similarity"
      ("s" "Synonyms" powerthesaurus-lookup-synonyms-dwim)
      ("a" "Antonyms" powerthesaurus-lookup-antonyms-dwim)
      ("r" "Related Words" powerthesaurus-lookup-related-dwim)]
     ["Information"
      ("d" "Definitions" powerthesaurus-lookup-definitions-dwim)
      ("e" "Example Sentences" powerthesaurus-lookup-sentences-dwim)]]))

;;; Hydra

;;;###autoload
(autoload #'powerthesaurus-hydra/body "powerthesaurus-ui-shortcuts" nil t)

(when (require 'hydra nil :noerror)
  (defhydra powerthesaurus-hydra (:color blue :hint nil)
    "
  Power Thesaurus
  ^Similarity^           ^Information^
  ---------------------------------------
  _s_: Synonyms          _d_: Definitions
  _a_: Antonyms          _e_: Example Sentences
  _r_: Related Words
  _q_: Quit
  "
    ("s" powerthesaurus-lookup-synonyms-dwim)
    ("a" powerthesaurus-lookup-antonyms-dwim)
    ("r" powerthesaurus-lookup-related-dwim)
    ("d" powerthesaurus-lookup-definitions-dwim)
    ("e" powerthesaurus-lookup-sentences-dwim)
    ("q" nil)))

;;; powerthesaurus-ui-shortcuts.el ends here

(provide 'powerthesaurus-ui-shortcuts)

;; Disable compilation, because transient and hydra are optional arguments, the
;; compiler may warn against unsatisfied requirements. Also speed isn't so
;; crucial here and lazy loading is already achieved.

;; Local Variables:
;; no-byte-compile: t
;; End:
