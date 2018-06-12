![emacs-powerthesaurus](assets/emacs-powerthesaurus.png)

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) [![MELPA](https://melpa.org/packages/powerthesaurus-badge.svg)](https://melpa.org/#/powerthesaurus)

**emacs-powerthesaurus** is a simple plugin to integrate Emacs with amazing [powerthesaurus.org](https://www.powerthesaurus.org).

## Installation

**emacs-powerthesaurus** is available on MELPA. You can install it using the following command:

<kbd>M-x package-install [RET] powerthesaurus [RET]</kbd>

## How to use

**emacs-powerthesaurus** defines two interactive functions:
* `powerthesaurus-lookup-word`
* `powerthesaurus-lookup-word-at-point`

If you have any active selection, `powerthesaurus-lookup-word` fetches selected text at [powerthesaurus.org](https://www.powerthesaurus.org) and gives you a list of synonyms to replace it with. Without any selection it asks you for the input first, and insert selected synonyms at point.

`powerthesaurus-lookup-word-at-point` finds a word at point (according to the current mode settings), fetches it at [powerthesaurus.org](https://www.powerthesaurus.org), and also replaces it with the selected synonym.

## Demo

![demo](assets/demo.gif)
