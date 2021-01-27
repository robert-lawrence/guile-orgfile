
guile-orgfile is a simple parser for documents written in Emacs Org mode (.org).

This was written for use as an exporter with [Haunt](https://dthompson.us/projects/haunt.html), a static site generator
written in Guile Scheme. It is not feature complete and is under active development.

Planned features:

- [x] Parse document tree into sections, with headers exported as \<h1\>-\<h6\> tags and
      section text exported as a \<p\> tag
- [ ] Parse links, export using \<a\> tag
- [x] Wrap header in div that covers all subheaders
- [x] Parse org tags, convert to html class attributes for div
- [x] Parse org lists
- [ ] Parse org tables
- [ ] Parse org code blocks

Usage
-----

``` scheme
(use-modules (orgfile)
             (sxml simple))

(define doc
  "* A typical org file

a paragraph

** Sections can be nested

 1. List item 1
 2. List item 2
 
 
 another paragraph.")

;; Parse the org document
(define org-sxml (orgfile->sxml doc))

;; Write the document to current output port
(sxml->xml org-sxml)
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


