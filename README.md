
guile-orgfile is a simple parser for documents written in Emacs Org mode (.org).

This was written for use as an exporter with Haunt, a static site generator
written in Guile Scheme. It is not feature complete and is under active development.

Planned features:

- [x] Parse document tree into sections, with headers exported as \<h1\>-\<h4\> tags and
      section text exported as a \<p\> tag
- [ ] Parse links, export using \<a\> tag
- [x] Wrap header in div that covers all subheaders
- [x] Parse org tags, convert to html class attributes for div
- [ ] Parse org lists
- [ ] Parse org tables
- [ ] Parse org code blocks
