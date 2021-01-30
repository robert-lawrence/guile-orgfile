
guile-orgfile is a simple parser for documents written in Emacs Org mode (.org).

This was written for use as an exporter with [Haunt](https://dthompson.us/projects/haunt.html), a static site generator
written in Guile Scheme. It is not feature complete and is under active development.

Planned features:

- [x] Parse document tree into sections, with headers exported as \<h1\>-\<h6\> tags and
      section text exported as a \<p\> tag
- [x] Parse links, export using \<a\> tag
- [x] Wrap header in div that covers all subheaders
- [x] Parse org tags, convert to html class attributes for div
- [x] Parse org lists
- [ ] Parse bold, italic, underlined and strikethrough text
- [ ] Parse org code blocks
- [ ] Parse org tables

Usage
-----

``` scheme
(use-modules (orgfile)
             (sxml simple))

(define org-text
  "* A typical org file

a paragraph with an [[http://example.com][example link]]

** Sections can be nested

 1. List item 1
 2. List item 2
 
 
 another paragraph.")

;; Parse the org document
(define doc (parse-orgfile org-text))

;; Convert the document to sxml and write to current output port
(sxml->xml (orgfile->sxml doc))
```


Installation
------------

Build from git:
```sh
./bootstrap
./configure
make
make install

```


