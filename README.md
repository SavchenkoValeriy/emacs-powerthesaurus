![emacs-powerthesaurus](assets/emacs-powerthesaurus.png)

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) [![MELPA](https://melpa.org/packages/powerthesaurus-badge.svg)](https://melpa.org/#/powerthesaurus)

**emacs-powerthesaurus** is a simple plugin to integrate Emacs with amazing [powerthesaurus.org](https://www.powerthesaurus.org).

## Installation

**emacs-powerthesaurus** is available on MELPA. You can install it using the following command:

<kbd>M-x package-install [RET] powerthesaurus [RET]</kbd>

## How to use

**emacs-powerthesaurus** defines a single interactive function, namely `powerthesaurus-lookup-dwim`,
should be used for retrieving results from [powerthesaurus.org](https://www.powerthesaurus.org).
This command asks for the type of the query to perform and tries to infer the term for which
synonyms, antonyms, etc. should be downloaded.

Particularly:
* If the region is active, the search term defaults to the region's contents.
* If the region is inactive and point is located at a word, the search term defaults to that word.
* In any other case, the user will be directly prompted for the phrase to search.

Currently, the following query types are supported:
* synonyms,
* antonyms,
* related terms,
* definitions, and
* sentences.

Depending on the type of query performed, 
`powerthesaurus-lookup-dwim` will either 
replace the original term with one of the fetched results,
insert a selected result at point, or
display the list of all results in a pop-up buffer. 
By default,
results corresponding to "synonyms", "antonyms" and "related" queries
will be inserted in place 
whereas
results for sentences and definitions are displayed separately, 
although one can override this behavior through the prefix argument (i.e., 
<kbd>C-u M-x powerthesaurus-lookup-dwim</kbd> will cause to replace/insert 
the selected result in the current buffer independently of the query's type).

For the sake of convenience, 
the following commands are also provided 
which wrap around `powerthesaurus-lookup-dwim` and 
provide a quick way to perform a specific type of query:
* `powerthesaurus-lookup-synonyms-dwim`
* `powerthesaurus-lookup-antonyms-dwim`
* `powerthesaurus-lookup-related-dwim`
* `powerthesaurus-lookup-definitions-dwim`
* `powerthesaurus-lookup-sentences-dwim`

Finally, the function `powerthesaurus-lookup` is provided for non-interactive use.

## Old API
The following subsections describe the now obsolete API before version 0.2, which is still provided for the time being although use of it should 

### How to use

**emacs-powerthesaurus** defines three interactive functions:
* `powerthesaurus-lookup-word`
* `powerthesaurus-lookup-word-at-point`
* `powerthesaurus-lookup-word-dwim`

If you have any active selection, `powerthesaurus-lookup-word` fetches selected text at [powerthesaurus.org](https://www.powerthesaurus.org) and gives you a list of synonyms to replace it with. Without any selection it asks you for the input first, and insert selected synonyms at point.

`powerthesaurus-lookup-word-at-point` finds a word at point (according to the current mode settings), fetches it at [powerthesaurus.org](https://www.powerthesaurus.org), and also replaces it with the selected synonym.

`powerthesaurus-lookup-word-dwim` combines these two functions into one. It tries to infer whatever user wants to look up. If there is an active selection that will be the choice. Otherwise, it checks if there any word at point and fetches that word. And if there is nothing appropriate, it asks the user to provide a word.

### Demo

![demo](assets/demo.gif)

## Changelog

### Version 0.2.2
* Remove redundant headers in request messages to server that potentially cause
  failure to parse received candidates (Github issue #16).

### Version 0.2.1
* Fix regression involving result sorting during selection.
* Fix minor bugs.

### Version 0.2.0

* Revamp the package's overall architecture.
* Extend mechanism allowing different types of queries, namely
  synonyms, antonyms, related terms, definitions, and sentences.
* Support queries involving terms that consist of multiple words (e.g.,
  "give up").
* Fix Github issues #13, #14, #15 and #17.
