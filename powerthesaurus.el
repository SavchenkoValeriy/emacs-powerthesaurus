;;; powerthesaurus.el --- Powerthesaurus integration -*- lexical-binding: t; -*-

;; Copyright (c) 2018-2023 Valeriy Savchenko (GNU/GPL Licence)

;; Authors: Valeriy Savchenko <sinmipt@gmail.com>
;; URL: http://github.com/SavchenkoValeriy/emacs-powerthesaurus
;; Version: 0.3.6
;; Package-Requires: ((emacs "26.1") (jeison "1.0.0") (s "1.13.0"))
;; Keywords: convenience, writing

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

;;; This package is an integration with powerthesaurus.org.
;;; It helps to look up a word in powerthesaurus and either replace or
;;; insert selected option in the buffer (depending on the current selection).

;;; Code:
(require 'url)
(require 's)
(require 'subr-x)
(require 'jeison)

(defvar powerthesaurus-request-headers
  '(("Content-Type" . "application/json"))
  "List of headers included in the request sent to 'powerthesaurus.org'.")

(defvar powerthesaurus-user-agent "Chrome/74.0.3729.169")

(defvar powerthesaurus-synchronous-requests nil
  "If non-nil, requests send to 'powerthesaurus.org' are synchronous.")

(defconst powerthesaurus-supported-query-types
  (list :synonyms :antonyms :related :definitions :sentences))

(defconst powerthesaurus-api-url "https://api.powerthesaurus.org")

;;;###autoload
(defun powerthesaurus-lookup-dwim (&optional action-type query-type)
  "Wrapper function for general lookup commands.

When called interactively, optional argument ACTION-TYPE corresponds to
the prefix argument passed to this command, which is translated to an action
using `powerthesaurus-prefix-to-action'.  When called programmatically,
its value can either be nil or a symbol that can be possibly returned by
`powerthesaurus-prefix-to-action' (e.g., `action-insert' or `action-display').

The argument passed to QUERY-TYPE should be the same as in
`powerthesaurus-lookup' or nil; in the latter case,
the user will be prompt for a valid value."
  (interactive "P")
  (pcase-let ((`(,query-term ,beg ,end)
               ;; selection is active -> look up whatever is selected
               (if (use-region-p)
                   (powerthesaurus--extract-query-region)
                 ;; point is is at a word -> look it up
                 (if (thing-at-point 'word)
                     (powerthesaurus--extract-original-word)
                   ;; nothing appropriate nearby -> ask the user
                   (list nil nil nil)))))
    (setq query-term (read-string "Term: " query-term)
          query-type (or query-type
                         (intern (completing-read "Query type: "
                                                  powerthesaurus-supported-query-types
                                                  nil t)))
          action-type (powerthesaurus-prefix-to-action action-type query-type))
    (cond
     ((eq action-type 'action-insert)
      (when (null beg)
        (setq beg (point) end (point))))
     ((eq action-type 'action-display)
      (when (or beg end)
        (setq beg nil end nil))))
    (funcall 'powerthesaurus-lookup query-term query-type beg end)))

;;;###autoload
(defun powerthesaurus-lookup-synonyms-dwim (&optional action-type)
  "Wrapper function for synonym lookup.
ACTION-TYPE accepts the same arguments as in `powerthesaurus-lookup-dwim'."
  (interactive "P")
  (powerthesaurus-lookup-dwim action-type :synonyms))

;;;###autoload
(defun powerthesaurus-lookup-antonyms-dwim (&optional action-type)
  "Wrapper function for antonym lookup.
ACTION-TYPE accepts the same arguments as in `powerthesaurus-lookup-dwim'."
  (interactive "P")
  (powerthesaurus-lookup-dwim action-type :antonyms))

;;;###autoload
(defun powerthesaurus-lookup-related-dwim (&optional action-type)
  "Wrapper function for related lookup.
ACTION-TYPE accepts the same arguments as in `powerthesaurus-lookup-dwim'."
  (interactive "P")
  (powerthesaurus-lookup-dwim action-type :related))

;;;###autoload
(defun powerthesaurus-lookup-definitions-dwim (&optional action-type)
  "Wrapper function for definition lookup.
ACTION-TYPE accepts the same arguments as in `powerthesaurus-lookup-dwim'."
  (interactive "P")
  (powerthesaurus-lookup-dwim action-type :definitions))

;;;###autoload
(defun powerthesaurus-lookup-sentences-dwim (&optional action-type)
  "Wrapper function for sentence lookup.
ACTION-TYPE accepts the same arguments as in `powerthesaurus-lookup-dwim'."
  (interactive "P")
  (powerthesaurus-lookup-dwim action-type :sentences))

;;;###autoload
(defun powerthesaurus-lookup (query-term query-type &optional beg end)
  "Retrieve the given QUERY-TERM's synonyms, antonyms, etc... online.

Argument QUERY-TYPE specifies the type of query and must be an element of
`powerthesaurus-supported-query-types'.
QUERY-TERM corresponds to the word/term/sentence to look up.

If specified, BEG and END specify the beginning and end positions of
the text in the buffer to be replaced by the selected result.
Particularly, if both BEG and END are both nil, then the results of the queries
will be displayed on a distinct buffer.  If only BEG is specified or
both BEG and END are the same, then the user will be prompted to select one of
the results to be inserted at BEG.  Finally, if both BEG and END are specified
and are different, then the user will be prompted to select a result
which will replace the text between these bounds."
  (powerthesaurus--query
   query-term
   query-type
   (powerthesaurus--make-callback query-term query-type beg end)))

;; ===============================================================
;; UX functions implementation
;; ===============================================================

(defun powerthesaurus-prefix-to-action (uarg qtype)
  "Map given prefix argument UARG to corresponding action type.

A single universal argument (\\[universal-argument]) indicates that
the result of the query should be inserted at point,
potentially replacing the word under it or the selected phrase.
This corresponds to returning the symbol `action-insert'.

A double universal argument (\\[universal-argument] \\[universal-argument])
indicates that the results of the query should be displayed on
a separate buffer without modifying the current one.
This corresponds to returning the symbol `action-display'.

If no prefix argument is given,
then the type of the query specified via QTYPE is used for
determining the action that should be preferred.
Particularly, if the type is one of 'synonyms', 'antonyms' or 'related',
then the result defaults to `action-insert'.
In any other case, it defaults to `action-display'."
  (cond
   ((null uarg)
    (if (member qtype '(:synonyms :antonyms :related))
        'action-insert
      'action-display))
   ((memq uarg '(action-insert action-display)) uarg)
   ((equal uarg '(4)) 'action-insert)
   ((equal uarg '(16)) 'action-display)
   (t (error "Unexpected prefix argument"))))

(defun powerthesaurus--extract-original-word (&optional pnt)
  "Parse the word under point to look up.

If optional argument PNT is not specified,
default to cursor's current location."
  (setq pnt (or pnt (point)))
  (save-mark-and-excursion
    (goto-char pnt)
    (unless (looking-at-p "\\<")
      (backward-word))
    (let (beg end)
      (setq beg (point))
      (forward-word)
      (setq end (point))
      (powerthesaurus--extract-query-region beg end))))

(defun powerthesaurus--extract-query-region (&optional beg end)
  "Parse the phrase in region.

If optional arguments BEG and END are not specified,
the contents of the current active region are used."
  (cl-flet ((substring-and-bounds
             (lambda (beg end)
               (list (buffer-substring-no-properties beg end)
                     beg end))))
    ;; If *either* BEG or END have been specified,
    ;; then try to get the specified substring.
    ;; Notice that in case only one of them has been passed,
    ;; then `buffer-substring-no-properties' will take care of throwing an error.
    (if (or beg end)
        (substring-and-bounds beg end)
      (if (use-region-p)
          (substring-and-bounds (region-beginning) (region-end))
        (error "Failed parsing query term from active region")))))

(defun powerthesaurus--read-query-term (&optional prompt)
  "Ask the user for which word to look up.
If PROMPT is not specified, a default one will be used."
  (setq prompt (or prompt "Term: "))
  (list (substring-no-properties (read-string prompt)) nil nil))

(defun powerthesaurus--make-callback (query-term query-type
                                                 &optional beg end)
  "Generate a callback to be executed upon successful completion of request.

If BEG and/or END are non-nil, then `powerthesaurus--make-insert-callback'
will be used as the underlying callback generator, otherwise it defaults to
`powerthesaurus--make-display-callback'.

QUERY-TYPE and QUERY-TERM will be passed to
the underlying callback generator, possibly altering its behavior to
better accommodate the corresponding type of query."
  (cond
   ((or beg end)
    (powerthesaurus--make-insert-callback query-term query-type
                                          (current-buffer)
                                          beg end))
   (t
    (powerthesaurus--make-display-callback query-term query-type))))

(defun powerthesaurus--make-insert-callback (query-term
                                             query-type
                                             buffer beg end)
  "Generate a callback that will insert the query's result to buffer.

The callback generated by this function accepts the data belonging to
the response to a previously made request as its sole argument.

If END is nil or BEG and END are equal,
the generated callback will prompt the user to select a returned result and
insert it at point.
Otherwise, if BEG and END differ,
then the region between these points will be replaced by the selected result.
BEG must be non-nil.

BUFFER is the buffer object where term will be replaced and
should be explicitly specified since, in case of asynchronous execution,
the callback may be executed with cursor under a different buffer.

QUERY-TERM corresponds to the text to be replaced by
the generated callback, and BEG and END correspond to the substituted text's
beginning and ending positions within the buffer.

QUERY-TYPE must be an element of `powerthesaurus-supported-query-types' and
is used for determining how to parse the aforementioned data.
In general, its argument should be the same as the type specified when
creating the corresponding request."
  (let ((backend (if (or (null end)
                         (equal beg end))
                     (lambda (new original)
                       (with-current-buffer buffer
                         (powerthesaurus--insert-text new original)))
                   (lambda (new original)
                     (with-current-buffer buffer
                       (powerthesaurus--replace-text new beg end original))))))
    (lambda (results)
      (funcall backend
               (powerthesaurus--select-candidate results)
               query-term))))

(defun powerthesaurus--make-display-callback (query-term query-type)
  "Generate a callback that will display the query's results to another buffer.

The callback generated by this function accepts the data belonging to
the response to a previously made request as its sole argument.
The results of the query will then be extracted and displayed on
a different buffer.

QUERY-TERM corresponds to the original text that was queried online.

QUERY-TYPE must be an element of `powerthesaurus-supported-query-types' and
is used for determining how to parse the aforementioned data.
In general, its argument should be the same as the type specified when
creating the corresponding request.
Additionally, it affects aspects of the generated callback's behavior,
such as the default string used for separating the results displayed
in the buffer."
  (lambda (results)
    (powerthesaurus--display-results
     results
     query-term
     query-type)))

(defun powerthesaurus--replace-text (replacement beg end original)
  "Pick an alternative from response and replace the selected text.

REPLACEMENT corresponds to the new text to be inserted in place of ORIGINAL.
BEG and END correspond to the bounds of the selected text to be replaced."
  (delete-region beg end)
  (powerthesaurus--insert-text replacement original (min beg end)))

(defun powerthesaurus--preprocess-text (text reference)
  "Adjust cases of TEXT according to REFERENCE.

For now, it supports upcasing and capitalization."
  (cond ((s-uppercase-p reference) (upcase text))
        ((s-capitalized-p reference) (capitalize text))
        (t text)))

(defun powerthesaurus--insert-text (text reference &optional pnt)
  "Insert TEXT at the point after preprocessing it according to REFERENCE.

REFERENCE corresponds to the term whose query yielded TEXT.

If optional argument PNT is given, the insert text there.  Otherwise,
insert text under cursor."
  (when pnt (goto-char pnt))
  (insert (powerthesaurus--preprocess-text text reference)))

(defun powerthesaurus--display-results (results query-term query-type
                                                &optional sep)
  "Display results on a dedicated buffer.

RESULTS must be a list of `powerthesaurus-result' instances.
QUERY-TERM and QUERY-TYPE must be the text that was queried online
and the corresponding query type that yielded the results to be displayed.

Optional argument SEP is the string that will be used to separate
the displayed results in the buffer.  If not specified,
its default value varies depending on value of QUERY-TYPE."
  (unless sep
    (cond
     ((member query-type '(:definitions :sentences))
      (setq sep "\n----------------\n"))
     (t (setq sep "\n"))))
  (let* ((buf-name (format "*Powerthesaurus - \"%s\" - %s*"
                           query-term query-type))
         (buf-exists (get-buffer buf-name))
         (buf (or buf-exists (get-buffer-create buf-name))))
    (with-current-buffer buf
      (when buf-exists
        (fundamental-mode)
        (read-only-mode -1)
        (erase-buffer))
      (dolist (elt results)
        (insert (oref elt text) sep))
      (help-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun powerthesaurus--compose-completion-candidate (result)
  "Compose completion candidate out of the given RESULT.

RESULT should be an instance of `powerthesaurus-result'."
  (let* ((text (oref result text))
         (rating (format "%s★ " (oref result rating))) )
    (propertize text 'line-prefix rating)))

(defun powerthesaurus--compose-completion-candidates (results)
  "Compose completion candidates out of the given RESULTS.

RESULT should be a list of `powerthesaurus-result'."
  (mapcar #'powerthesaurus--compose-completion-candidate results))

(defun powerthesaurus--select-candidate (candidates)
  "Prompt the user to select one of the CANDIDATES returned from a query."
  (let* ((candidates-processed (powerthesaurus--compose-completion-candidates candidates))
         ;; this is the only way we can keep the order while using
         ;; the default implementation of completing-read function
         ;; see: https://emacs.stackexchange.com/a/41808/23751
         (completion-table
          (lambda (string pred action)
            (if (eq action 'metadata)
                '(metadata (display-sort-function . identity)
                           (cycle-sort-function . identity))
              (complete-with-action
               action candidates-processed string pred))))
         ;; ivy still will try to sort it lexicographically: deny it
         (ivy-sort-functions-alist '((t . (lambda (x y) 0))))
         ;; ivy-rich can mess up our efforts of displaying rating
         (ivy--display-transformers-alist nil))
    ;; If we try to call completing-read with an active company popup,
    ;; we inherit its key map. That leads to some funny bugs (see issue#33).
    (when (fboundp 'company-uninstall-map)
      (company-uninstall-map))
    (substring-no-properties
     (completing-read "Choose a candidate: " completion-table nil nil))))

;; ===============================================================
;; Requests and JSON parsing
;; ===============================================================

(jeison-defclass powerthesaurus-result nil
  ((text :initarg :text :type string :path (node targetTerm name)
         :documentation "Actual text of the word from Powerthesaurus")
   (rating :initarg :rating :type number :path (node rating)
           :documentation "User rating of the word")))

(jeison-defclass powerthesaurus-definition nil
  ((text :initarg :text :type string :path (node definition)
         :documentation "Definition from Powerthesaurus")
   (rating :initarg :rating :type number :path (node rating)
           :documentation "User rating of the definition")))

(jeison-defclass powerthesaurus-sentence nil
  ((text :initarg :text :type string :path (node sentence)
         :documentation "Sentence example from Powerthesaurus")
   (rating :initarg :rating :type number :path (node rating)
           :documentation "User rating of the sentence")))

(defun powerthesaurus--query (term type &optional callback sync)
  "Make a query to Powerthesaurus.

TERM is the main text of the query.
TYPE should be a query type for thesaurus (e.g. ':synonyms' or ':related').
CALLBACK gets called whenever the response is received and processed.
SYNC is t for synchronous version of the request."
  (let ((query (pcase type
                 ((pred powerthesaurus--is-thesaurus-query-type)
                  #'powerthesaurus--query-thesaurus)
                 (:definitions #'powerthesaurus--query-definition)
                 (:sentences #'powerthesaurus--query-sentence)
                 (_ (error "Unknown query type '%s'" type)))))
    (funcall query term type callback sync)))

(defun powerthesaurus--request-term-id (term callback &optional sync)
  "Request id for the given TERM.

CALLBACK gets called whenever the response is received and processed.
SYNC is t for synchronous version of the request.

Powerthesaurus APIs require explicit IDs assigned to every term.
This request fetches it for the further use."
  (powerthesaurus--query-impl
   `(("query" . ,term))
   powerthesaurus--search-query
   callback
   (lambda (data) (jeison-read t data '(data search terms 0 id)))
   sync))

(defmacro powerthesaurus--with-term-id (term name sync &rest body)
  "Request id for the given TERM, bind it to NAME, and execute BODY.

TERM is the term to get ID for.
SYNC is t for synchronous version of the request."
  (declare (indent 3) (debug t))
  `(let ((on-success
          (lambda (,name)
            ,@body)))
     (powerthesaurus--request-term-id ,term on-success sync)))

(defun powerthesaurus--query-thesaurus (term type &optional callback sync)
  "Request thesaurus information from Powerthesaurus.

TERM is the text to get definition for.
TYPE should be a query type for thesaurus (e.g. ':synonyms' or ':related').
CALLBACK gets called whenever the response is received and processed.
SYNC is t for synchronous version of the request."
  (powerthesaurus--with-term-id term term-id sync
    (powerthesaurus--query-impl
     `(("type" . ,(powerthesaurus--type-of-thesaurus-query type))
       ("termID" . ,term-id)
       ("sort" .
        (("field" . "RATING")
         ("direction" . "DESC"))))
     powerthesaurus--thesaurus-query
     callback
     (lambda (data) (jeison-read '(list-of powerthesaurus-result) data '(data thesauruses edges)))
     sync)))

(defun powerthesaurus--is-thesaurus-query-type (query-type)
  "Return 't' if the given QUERY-TYPE is for thesaurus queries."
  (member query-type '(:synonyms :antonyms :related)))

(defun powerthesaurus--type-of-thesaurus-query (type)
  "Return an API type corresponding to the given query TYPE."
  (pcase type
    (:synonyms "SYNONYM")
    (:antonyms "ANTONYM")
    (:related  "RELATED")
    (_ (error "Unknown thesaurus query type '%s'" type))))

(defun powerthesaurus--query-definition (term type &optional callback sync)
  "Request definitions from Powerthesaurus.

TERM is the text to get definition for.
TYPE should be nothing but ':definitions'.
CALLBACK gets called whenever the response is received and processed.
SYNC is t for synchronous version of the request."
  (powerthesaurus--with-term-id term term-id sync
    (powerthesaurus--query-impl
     `(("termID" . ,term-id))
     powerthesaurus--definition-query
     callback
     (lambda (data) (jeison-read '(list-of powerthesaurus-definition) data '(data definitions edges)))
     sync)))

(defun powerthesaurus--query-sentence (term type &optional callback sync)
  "Request sentences from Powerthesaurus.

TERM is the text for sentence examples.
TYPE should be nothing but ':sentences'.
CALLBACK gets called whenever the response is received and processed.
SYNC is t for synchronous version of the request."
  (powerthesaurus--with-term-id term term-id sync
    (powerthesaurus--query-impl
     `(("termID" . ,term-id))
     powerthesaurus--sentence-query
     callback
     (lambda (data) (jeison-read '(list-of powerthesaurus-sentence) data '(data sentences edges)))
     sync)))

(defun powerthesaurus--query-impl (variables query &optional callback postprocess sync)
  "Request data from Powerthesaurus GraphQL API.

VARIABLES is an alist of query-specific parameters.
QUERY is the actual GraphQL query.
CALLBACK gets called whenever the response is received and processed.
POSTPROCESS is the additional processing of the JSON response alist.
SYNC is t for synchronous version of the request."
  (make-local-variable 'url-show-status)
  (let* ((post (or postprocess 'identity))
         (sync (or (null callback)
                   sync
                   powerthesaurus-synchronous-requests))
         (url-request-data (json-encode `(("variables" . ,variables)
                                          ("query" . ,query))))
         (url-request-extra-headers powerthesaurus-request-headers)
         (url-request-method "POST")
         ;; User agent has to be set separately like this, so that
         ;; url-retrieve won't add anything else to it.
         (url-user-agent powerthesaurus-user-agent)
         ;; Prohibit it from writing "Contacting host:..." every
         ;; time we send a request, it's not informative.
         (url-show-status nil)
         (callback (lambda (&rest _)
                     (with-local-quit
                       (let* ((raw (string-trim
                                    (buffer-substring
                                     url-http-end-of-headers (point-max))))
                              ;; TODO: parse JSON more efficiently
                              ;;       if native method is available
                              (json (json-read-from-string raw))
                              (data (funcall post json)))
                         (funcall callback data))))))
    (if (not sync)
        (url-retrieve powerthesaurus-api-url callback)
      (with-current-buffer
          (url-retrieve-synchronously powerthesaurus-api-url)
        (funcall callback)))))

(defun powerthesaurus--wrap-as-callback (fun)
  "Wrap the given FUN function as a `request' callback."
  (cl-function
   (lambda (&key data &allow-other-keys)
     ;; in order to allow users to quit powerthesaurus
     ;; prompt with C-g, we need to wrap callback with this
     (with-local-quit (funcall fun data)))))

;; ===============================================================
;; Define old API's now deprecated functions.
;; ===============================================================

;;;###autoload
(defun powerthesaurus-lookup-word-dwim ()
  "Wrapper function for powerthesaurus-lookup-word commands.

If a region is selected use powerthesaurus-lookup-word
if a thing at point is not empty use powerthesaurus-lookup-word-at-point
otherwise as for word using powerthesaurus-lookup-word"
  (interactive)
  (let (beg end)
    ;; selection is active -> look up whatever is selected
    (if (use-region-p)
        (progn
          (setq beg (region-beginning))
          (setq end (region-end))
          (powerthesaurus-lookup-word beg end))
      ;; point is is at a word -> look it up
      (if (thing-at-point 'word)
          (powerthesaurus-lookup-word-at-point (point))
        ;; nothing appropriate nearby -> ask the user
        (powerthesaurus-lookup-word)))))

;;;###autoload
(defun powerthesaurus-lookup-word-at-point (word-point)
  "Find word at `WORD-POINT', look it up in powerthesaurs, and replace it."
  (interactive (list (point)))
  (pcase-let ((`(,word ,beg ,end)
               (powerthesaurus--extract-original-word word-point)))
    (powerthesaurus-lookup word :synonyms beg end)))

;;;###autoload
(defun powerthesaurus-lookup-word (&optional beginning end)
  "Find the given word's synonyms at powerthesaurus.org.

`BEGINNING' and `END' correspond to the selected text with a word to replace.
If there is no selection provided, additional input will be required.
In this case, a selected synonym will be inserted at the point."
  (interactive
   ;; it is a simple interactive function instead of interactive "r"
   ;; because it doesn't produce an error in a buffer without a mark
   (if (use-region-p) (list (region-beginning) (region-end))
     (list nil nil)))
  (pcase-let ((`(,word _ _)
               (if beginning
                   (powerthesaurus--extract-query-region beginning end)
                 (powerthesaurus--read-query-term "Word to fetch: "))))
    (powerthesaurus-lookup word :synonyms (or beginning (point)) end)))

(make-obsolete 'powerthesaurus-lookup-word
               'powerthesaurus-lookup "0.2.0")
(make-obsolete 'powerthesaurus-lookup-word-at-point
               'powerthesaurus-lookup "0.2.0")
(make-obsolete 'powerthesaurus-lookup-word-dwim
               'powerthesaurus-lookup "0.2.0")

;; GraphQL queries
(defconst powerthesaurus--search-query
  "query SEARCH($query: String!) {
  search(query: $query) {
    terms {
      id
      name
    }
  }
}")

(defconst powerthesaurus--thesaurus-query
  "query THESAURUS($termID: ID!, $type: List!, $sort: ThesaurusSorting!)  {
  thesauruses(termId: $termID, sort: $sort, list: $type) {
    edges {
      node {
        targetTerm {
          name
        }
        rating
        votes
      }
    }
  }
}")

(defconst powerthesaurus--definition-query
  "query DEFINITION($termID: ID!)  {
  definitions(termId: $termID) {
    edges {
      node {
        definition
        rating
        votes
      }
    }
  }
}")

(defconst powerthesaurus--sentence-query
  "query SENTENCE($termID: ID!)  {
  sentences(termId: $termID) {
    edges {
      node {
        sentence
        rating
        votes
      }
    }
  }
}")

;; ===============================================================
;; UI shortcuts
;; ===============================================================

;;;###autoload
(when (require 'hydra nil :noerror)
  (eval '(defhydra powerthesaurus-hydra (:color blue :hint nil)
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
           ("q" nil))))

;;;###autoload
(when (require 'transient nil :noerror)
  (eval '(transient-define-prefix powerthesaurus-transient ()
           "Transient for Power Thesaurus."
           [["Similarity"
             ("s" "Synonyms" powerthesaurus-lookup-synonyms-dwim)
             ("a" "Antonyms" powerthesaurus-lookup-antonyms-dwim)
             ("r" "Related Words" powerthesaurus-lookup-related-dwim)]
            ["Information"
             ("d" "Definitions" powerthesaurus-lookup-definitions-dwim)
             ("e" "Example Sentences" powerthesaurus-lookup-sentences-dwim)]])))

(provide 'powerthesaurus)
;;; powerthesaurus.el ends here
