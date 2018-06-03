![emacs-powerthesaurus](assets/emacs-powerthesaurus.png)

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) [![MELPA](https://melpa.org/packages/powerthesaurus-badge.svg)](https://melpa.org/#/powerthesaurus)

**emacs-powerthesaurus** is a simple plugin to integrate Emacs with amazing [powerthesaurus.org](https://www.powerthesaurus.org).

## Installation

**emacs-powerthesaurus** is available on MELPA. You can install it using the following command:

<kbd>M-x package-install [RET] powerthesaurus [RET]</kbd>

## How to use

There is only one interactive function available to the users: `powerthesaurus-lookup-word`. 

If you have any active selection, it fetches selected text at [powerthesaurus.org](https://www.powerthesaurus.org) and gives you a list of synonyms to replace it with. Without any selection it asks you for the input first, and insert selected synonyms at point.

## Demo

![demo](assets/demo.gif)
