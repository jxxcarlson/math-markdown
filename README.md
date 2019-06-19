
# Math Markdown


The aim of the Math Markdown library is
to provide a tool for rendering Markdown
text with embedded math-mode TeX, e.g.,

```
This **is** a test: $a^2 + b^2 = c^2$.
```

At the moment, the "version" of Markdown
we have implemented is primitive. Nonetheless, it is quite serviceable.  See
[markdown.minilatex.app](https://markdown.minilatex.app)
for a working example.

We will be working to expand the coverage
of Math Markdown.

## Example

To convert text to Html, do something like this:

```
MMarkdown.toHtml [ ] "This **is** a test: $a^2 + b^2 = c^2$."
```

## Demo app

There is a demo in `./app`.  To run
it, go into that folder and say `sh make.sh`.  Then
double-click on `index.html`.

## Differ

The purpose of the Differ module is to speed up the parse-render
operation by reparsing and rerendering only text that has been
changed.  This module is used in the demo app.   
