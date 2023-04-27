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
(require 'hydra)

;;;###autoload
(autoload #'powerthesaurus-hydra/body "powerthesaurus-hydra" nil t)

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
  ("q" nil))


(provide 'powerthesaurus-hydra)

;;; powerthesaurus-hydra.el ends here
