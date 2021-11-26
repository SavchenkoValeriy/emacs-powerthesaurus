;;; powerthesaurus.el --- Powerthesaurus integration -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Valeriy Savchenko (GNU/GPL Licence)

;; Authors: Valeriy Savchenko <sinmipt@gmail.com>
;; URL: http://github.com/SavchenkoValeriy/emacs-powerthesaurus
;; Version: 0.2.0
;; Package-Requires: ((emacs "24") (request "0.3.0") (s "1.12.0"))
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
(require 'dom)
(require 'request)
;; TODO: Remove unnecessary dependencies.
;; (require 'rx)
(require 's)
(require 'url-util)

(defvar powerthesaurus-request-headers
  '(("User-Agent" . "Chrome/74.0.3729.169")
    ("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
    ("Accept-Encoding" . "gzip, deflate, br")
    ("Accept-Language" . "en,en-US;q=0.5")
    ("Upgrade-Insecure-Requests" . "1"))
  "List of headers included in the request sent to 'powerthesaurus.org'.")

(defvar powerthesaurus-synchronous-requests nil
  "If non-nil, requests send to 'powerthesaurus.org' are synchronous.")

(defconst powerthesaurus-supported-query-types
  (list "synonyms" "antonyms" "related" "definitions" "sentences"))

(defun powerthesaurus-compose-url (original-phrase query-type)
  "Build and return powerthesaurus url to look up ORIGINAL-PHRASE.

QUERY-TYPE must be an element of `powerthesaurus-supported-query-types'."
  (unless (member query-type powerthesaurus-supported-query-types)
    (error "Unknown query type '%s'" query-type))
  (format "https://www.powerthesaurus.org/%s/%s"
          ;; Escape text of original-phrase,
          ;; in order to properly handle spaces, etc.
          (url-encode-url original-phrase)
          query-type))

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
  (pcase-let ((`(,original-phrase ,beg ,end)
               ;; selection is active -> look up whatever is selected
               (if (use-region-p)
                   (powerthesaurus--extract-original-phrase)
                 ;; point is is at a word -> look it up
                 (if (thing-at-point 'word)
                     (powerthesaurus--extract-original-word)
                   ;; nothing appropriate nearby -> ask the user
                   (list nil nil nil)))))
    (setq original-phrase (read-string "Phrase: " original-phrase)
          query-type (or query-type
                         (completing-read "Query type: "
                                          powerthesaurus-supported-query-types
                                          nil t))
          action-type (powerthesaurus-prefix-to-action action-type query-type))
    (cond
     ((eq action-type 'action-insert)
      (when (null beg)
        (setq beg (point) end (point))))
     ((eq action-type 'action-display)
      (when (or beg end)
        (setq beg nil end nil))))
    (funcall 'powerthesaurus-lookup original-phrase query-type beg end)))

;;;###autoload
(defun powerthesaurus-lookup-synonyms-dwim (&optional action-type)
  "Wrapper function for synonym lookup.
ACTION-TYPE accepts the same arguments as in `powerthesaurus-lookup-dwim'."
  (interactive "P")
  (powerthesaurus-lookup-dwim action-type "synonyms"))

;;;###autoload
(defun powerthesaurus-lookup-antonyms-dwim (&optional action-type)
  "Wrapper function for antonym lookup.
ACTION-TYPE accepts the same arguments as in `powerthesaurus-lookup-dwim'."
  (interactive "P")
  (powerthesaurus-lookup-dwim action-type "antonyms"))

;;;###autoload
(defun powerthesaurus-lookup-related-dwim (&optional action-type)
  "Wrapper function for related lookup.
ACTION-TYPE accepts the same arguments as in `powerthesaurus-lookup-dwim'."
  (interactive "P")
  (powerthesaurus-lookup-dwim action-type "related"))

;;;###autoload
(defun powerthesaurus-lookup-definitions-dwim (&optional action-type)
  "Wrapper function for definition lookup.
ACTION-TYPE accepts the same arguments as in `powerthesaurus-lookup-dwim'."
  (interactive "P")
  (powerthesaurus-lookup-dwim action-type "definitions"))

;;;###autoload
(defun powerthesaurus-lookup-sentences-dwim (&optional action-type)
  "Wrapper function for sentence lookup.
ACTION-TYPE accepts the same arguments as in `powerthesaurus-lookup-dwim'."
  (interactive "P")
  (powerthesaurus-lookup-dwim action-type "sentences"))

;;;###autoload
(defun powerthesaurus-lookup (original-phrase query-type &optional beg end)
  "Retrieve the given ORIGINAL-PHRASE's synonyms, antonyms, etc... online.

Argument QUERY-TYPE specifies the type of query and must be an element of
`powerthesaurus-supported-query-types'.
ORIGINAL-PHRASE corresponds to the word/term/sentence to look up.

If specified, BEG and END specify the beginning and end positions of
the text in the buffer to be replaced by the selected result.
Particularly, if both BEG and END are both nil, then the results of the queries
will be displayed on a distinct buffer.  If only BEG is specified or
both BEG and END are the same, then the user will be prompted to select one of
the results to be inserted at BEG.  Finally, if both BEG and END are specified
and are different, then the user will be prompted to select a result
which will replace the text between these bounds."
  (powerthesaurus--query
   original-phrase
   query-type
   (powerthesaurus--make-callback original-phrase query-type beg end)))

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
    (if (member qtype '("synonyms" "antonyms" "related"))
        'action-insert
      'action-display))
   ((memq uarg '(action-insert action-display)) uarg)
   ((equal uarg '(4)) 'action-insert)
   ((equal uarg '(16)) 'action-display)
   (t (error "Unexpected prefix argument"))))

(cl-defstruct (powerthesaurus-result) text rating)

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
      (powerthesaurus--extract-original-phrase beg end))))

(defun powerthesaurus--extract-original-phrase (&optional beg end)
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
        (error "Failed parsing original phrase from active region")))))

(defun powerthesaurus--read-original-phrase (&optional prompt)
  "Ask the user for which word to look up.
If PROMPT is not specified, a default one will be used."
  (setq prompt (or prompt "Phrase to fetch: "))
  (list (read-string (substring-no-properties prompt)) nil nil))

(defun powerthesaurus--make-callback (original-phrase query-type
                                                      &optional beg end)
  "Generate a callback to be executed upon successful completion of request.

If BEG and/or END are non-nil, then `powerthesaurus--make-insert-callback'
will be used as the underlying callback generator, otherwise it defaults to
`powerthesaurus--make-display-callback'.

QUERY-TYPE and ORIGINAL-PHRASE will be passed to
the underlying callback generator, possibly altering its behavior to
better accommodate the corresponding type of query."
  (cond
   ((or beg end)
    (powerthesaurus--make-insert-callback original-phrase query-type
                                          (current-buffer)
                                          beg end))
   (t
    (powerthesaurus--make-display-callback original-phrase query-type))))

(defun powerthesaurus--make-insert-callback (original-phrase
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

BUFFER is the buffer object where original phrase will be replaced and
should be explicitly specified since, in case of asynchronous execution,
the callback may be executed with cursor under a different buffer.

ORIGINAL-PHRASE corresponds to the text to be replaced by
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
    (cl-function
     (lambda (&key data &allow-other-keys)
       ;; in order to allow users to quit powerthesaurus
       ;; prompt with C-g, we need to wrap callback with this
       (with-local-quit
         (funcall backend
                  (powerthesaurus--select-candidate
                   (powerthesaurus--response-extract data query-type))
                  original-phrase))))))

(defun powerthesaurus--make-display-callback (original-phrase query-type)
  "Generate a callback that will display the query's results to another buffer.

The callback generated by this function accepts the data belonging to
the response to a previously made request as its sole argument.
The results of the query will then be extracted and displayed on
a different buffer.

ORIGINAL-PHRASE corresponds to the original text that was queried online.

QUERY-TYPE must be an element of `powerthesaurus-supported-query-types' and
is used for determining how to parse the aforementioned data.
In general, its argument should be the same as the type specified when
creating the corresponding request.
Additionally, it affects aspects of the generated callback's behavior,
such as the default string used for separating the results displayed
in the buffer."
  (cl-function
   (lambda (&key data &allow-other-keys)
     ;; in order to allow users to quit powerthesaurus
     ;; prompt with C-g, we need to wrap callback with this
     (with-local-quit
       (powerthesaurus--display-results
        (powerthesaurus--response-extract data query-type)
        original-phrase
        query-type)))))

(defun powerthesaurus--query (phrase type &optional callback sync)
  "Send query to 'powerthesaurus.org' and handle response.

PHRASE corresponds to the phrase to look up and
TYPE must be an element of `powerthesaurus-supported-query-types' (e.g.,
\"synonyms\").

Optional argument CALLBACK must be a function to be called upon
successfully completing the request.

Finally, if SYNC is non-nil, the function will wait for the response from
'powerthesaurus.org' before returning.
If not specified, the value of `powerthesaurus-synchronous-requests'
will determine this behavior."
  (request
    (powerthesaurus-compose-url phrase type)
    :parser (lambda () (libxml-parse-html-region (point) (point-max)))
    :headers powerthesaurus-request-headers
    :success callback
    :sync (or (null callback)
              sync
              powerthesaurus-synchronous-requests)))

(defun powerthesaurus--replace-text (replacement beg end original)
  "Pick an alternative from response and replace the selected text.

REPLACEMENT corresponds to the new text to be inserted in place of ORIGINAL.
BEG and END correspond to the bounds of the selected text to be replaced."
  (delete-region beg end)
  (powerthesaurus--insert-text replacement original (min beg end)))

(defun powerthesaurus--preprocess-text (text reference)
  "Adjust cases of the TEXT phrase based on the REFERENCE one.

For now, it supports upcasing and capitalization."
  (cond ((s-uppercase-p reference) (upcase text))
        ((s-capitalized-p reference) (capitalize text))
        (t text)))

(defun powerthesaurus--insert-text (text reference &optional pnt)
  "Insert text at the point after preprocessing it based on reference text.

REFERENCE represents the phrase associated with the TEXT to be inserted.

If optional argument PNT is given, the insert text there.  Otherwise,
insert text under cursor."
  (when pnt (goto-char pnt))
  (insert (powerthesaurus--preprocess-text text reference)))

(defun powerthesaurus--display-results (results original-phrase query-type
                                                &optional sep)
  "Display results on a dedicated buffer.

RESULTS must be a list of `powerthesaurus-result' instances.
ORIGINAL-PHRASE and QUERY-TYPE must be the text that was queried online
and the corresponding query type that yielded the results to be displayed.

Optional argument SEP is the string that will be used to separate
the displayed results in the buffer.  If not specified,
its default value varies depending on value of QUERY-TYPE."
  (unless sep
    (cond
     ((member query-type '("definitions" "sentences"))
      (setq sep "\n----------------\n"))
     (t (setq sep "\n"))))
  (let ((buf (get-buffer-create
              (format "*Powerthesaurus - \"%s\" - %s*"
                      original-phrase query-type))))
    (with-current-buffer buf
      (dolist (elt results)
        (insert (powerthesaurus-result-text elt) sep)))
    ;; TODO: Make buffer disposable via `keyboard-quit'.
    (pop-to-buffer buf)))

(defun powerthesaurus--select-candidate (candidates)
  "Prompt the user to select one of the CANDIDATES returned from a query."
  (print candidates)
  (let* ((candidates-sorted (powerthesaurus--sort-candidates candidates))
         ;; ivy still will try to sort it lexicographically: deny it
         (ivy-sort-functions-alist '((t . (lambda (x y) 0)))))
    (completing-read "Choose a candidate: " candidates-sorted nil t)))

;; (defun powerthesaurus--select-candidate (candidates)
;;   "Prompt the user to select one of the CANDIDATES returned from a query."
;;   (let* ((candidates-sorted (powerthesaurus--sort-candidates candidates))
;;          ;; this is the only way we can keep the order while using
;;          ;; the default implementation of completing-read function
;;          ;; see: https://emacs.stackexchange.com/a/41808/23751
;;          (completion-table
;;           (lambda (string pred action)
;;             (if (eq action 'metadata)
;;                 '(metadata (display-sort-function . identity)
;;                            (cycle-sort-function . identity))
;;               (complete-with-action
;;                action candidates-sorted string pred))))
;;          ;; ivy still will try to sort it lexicographically: deny it
;;          (ivy-sort-functions-alist nil))
;;     (completing-read "Choose a candidate: " completion-table nil nil)))

(defun powerthesaurus--sort-candidates (synonyms)
  "Compose choices from the `powerthesaurus-result' list of SYNONYMS."
  (mapcar (lambda (word) (powerthesaurus-result-text word))
          (sort synonyms (lambda (x y) (< (powerthesaurus-result-rating x)
                                          (powerthesaurus-result-rating y))))))

(defun powerthesaurus--response-extract (data type)
  "Extract specified TYPE from response's DATA."
  (cond
   ((member type '("synonyms" "antonyms" "related"))
    (powerthesaurus--response-extract-sar data))
   ((member type '("definitions"))
    (powerthesaurus--response-extract-definitions data))
   ((member type '("sentences"))
    (powerthesaurus--response-extract-sentences data))
   (t (error "Unknown query type '%s'" type))))

(defun powerthesaurus--response-extract-sar (data)
  "Extract synonyms, antonyms or related from response's DATA."
  (let* ((results
          (cl-map 'list (lambda (it) (dom-text (dom-by-tag it 'a)))
                  (dom-by-id data "primary-area")))
         (results-ranked
          (cl-loop for elt in results and n from 0 by 1
                   collect (make-powerthesaurus-result :text elt
                                                       :rating n))))
    results-ranked))

(defun powerthesaurus--response-extract-definitions (data)
  "Extract definitions from response's DATA."
  (let* ((results
          (dom-children
           (dom-children
            (dom-children
             (dom-by-tag data 'main)))))
         (results (cl-map 'list
                          (lambda (it)
                            (dom-text
                             (nth 1 (dom-children
                                     (nth 1 (dom-children
                                             (dom-children it)))))))
                          results))
         (results-ranked
          (cl-loop for elt in results and n from 0 by 1
                   collect (make-powerthesaurus-result :text elt
                                                       :rating n))))
    results-ranked))

(defun powerthesaurus--response-extract-sentences (data)
  "Extract sentences from response's DATA."
  (let* ((results
          (dom-children
           (dom-children
            (dom-children
             (dom-by-tag data 'main)))))
         (results (cl-map 'list
                          (lambda (it)
                            (dom-texts
                             (dom-children
                              (nth 1 (dom-children
                                      (dom-children it)))) ""))
                          (dom-children results)))
         (results (cl-map 'list
                          (lambda (it)
                            (s-chop-prefix "\""
                                           (s-chop-suffix "\"" it)))
                          results))
         (results-ranked
          (cl-loop for elt in results and n from 0 by 1
                   collect (make-powerthesaurus-result :text elt
                                                       :rating n))))
    results-ranked))

(defun powerthesaurus-debug-connection ()
  "Debug requests to powerthesaurus.org."
  (setq request-log-level `debug)
  (setq request-message-level `debug))

(defun powerthesaurus-undebug-connection ()
  "Switch off debug information for requests."
  (setq request-log-level -1)
  (setq request-message-level -1))

;; Define old API's now deprecated functions.

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

(defun powerthesaurus-lookup-word-at-point (word-point)
  "Find word at `WORD-POINT', look it up in powerthesaurs, and replace it."
  (interactive (list (point)))
  (pcase-let ((`(,word ,beg ,end)
               (powerthesaurus--extract-original-word word-point)))
    (powerthesaurus-lookup word "synonyms" beg end)))

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
                   (powerthesaurus--extract-original-phrase beginning end)
                 (powerthesaurus--read-original-phrase "Word to fetch: "))))
    (powerthesaurus-lookup word "synonyms" (or beginning (point)) end)))

(make-obsolete 'powerthesaurus-lookup-word
               'powerthesaurus-lookup "0.2.0")
(make-obsolete 'powerthesaurus-lookup-word-at-point
               'powerthesaurus-lookup "0.2.0")
(make-obsolete 'powerthesaurus-lookup-word-dwim
               'powerthesaurus-lookup "0.2.0")

(provide 'powerthesaurus)
;;; powerthesaurus.el ends here
