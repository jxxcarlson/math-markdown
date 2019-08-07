
# MMarkdown


The aim of the MMarkdown library is
to provide a tool for rendering Markdown
text with embedded math-mode TeX, e.g.,

```
This **is** a test: $a^2 + b^2 = c^2$.
```

MMarkdown is to some extent *paragraph-centric*, meaning that certain elements, e.g., headings and
displayed math, like to be surrounded by blank lines.

In general, if you run into
something that is not working for you, please post an issue on
[https://github.com/jxxcarlson/math-markdown](GitHub).


## Example

To convert text to Html, do something like this:

```
MMarkdown.toHtml [ ] "This **is** a test: $a^2 + b^2 = c^2$."
```

## Demo app

There is a demo in `./app-math`.  To run
it, go into that folder and say `sh make.sh`.  Then
double-click on `index.html`.

## Style

The style used by MMarkdown is entirely determined by the
definitions of the CSS classes given in `index.html`.
See the code in `./app/index.html` for examles to imitate.
Consequently they are easily configured for whatever
application yuo have in mind.

## MathJax

The javascript code in `.app/index.html` is essential to the
proper rendering of mathematical text.  It should be copied
verbatim for your own app.

## Markdown extensions

I am trying to be conservative about extensions to
Markdown.  However, there are two that I thought
important enough to add: poetry and verbatim blocks.  Poetry
blocks are like quotation blocks, except that they begin
with ">>" instead of ">".  Line endings are respected.
Verbatim blocks are like code blocks,
except that they are set of by four tick marks instead of three,
and no syntax highlighting is applied.

## Technical stuff: the Differ module

Math Markdown exposes two modules of its six modules — `MMarkdown` and `Differ`.
The purpose of the Differ module is to speed up the parse-render
operation by reparsing and rerendering only text that has been
changed.  This module is used in the demo app.

I addition increased speed, using `Differ` results in a smoother
user experience, since only small parts of the document being
edited need to be re-processed.

## Running without Math

MMarkdown can also be used as a plain old Markdown parser,
with the paragraph-centric caveat above.  See `./app-plain`
