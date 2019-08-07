
# MMarkdown


The aim of the MMarkdown library is
to provide a tool for rendering Markdown
text with embedded math-mode TeX/LaTeX, e.g.,

```
This **is** a test: $a^2 + b^2 = c^2$.
```

Converting Markdown text to Html can be accomplished in one line:

```
MMarkdown.toHtml [ ] "This **is** a test: $a^2 + b^2 = c^2$."
```

MMarkdown is a pure Elm package.  No native code.  It relies on MathJax to render
mathematical formulas using a custom element.

## Notes

MMarkdown is to some extent *paragraph-centric*, meaning that certain elements, e.g.,
headings and displayed math, like to be surrounded by blank lines.

MMarkdwn uses MathJax.js to render formulas. It can, of course, be used
as a plain vanilla Markdown parser.  See the examples in
`./demo-plain` and `./app-plain`.

If you run into
something in MMarkdown that is not working for you, please post an issue on
[GitHub](https://github.com/jxxcarlson/math-markdown).

## Math demo apps

There are two demo apps.  The first, `demo-math` renders Markdown + Math
text using `MMarkdown.toHtml`.  The second, `app-math`, offers an interactive,
two-pane editor. To compile either, run `sh make.sh` and click on `index.html`

## MMarkdown without math

MMarkdown can be run without its math capabilities.  See the
`demo-plain` and `app-plain`.
To compile, proceed as above: run `sh make.sh`  and click on `index.html.

## Style

The style used by MMarkdown is entirely determined by the
definitions of the CSS classes given in `index.html`.
See the code in `./app/index.html` for examles to imitate.
Consequently they are easily configured for whatever
application yuo have in mind.

## MathJax

The javascript code in `.app/index.html` is essential to the
proper rendering of mathematical text.  It should be copied
verbatim for your own app. For `./demo-plain` and `/app-plain`,
this code is not needed.

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

Math Markdown exposes two of its six modules — `MMarkdown` and `Differ`.
The purpose of the Differ module is to speed up the parse-render
operation by reparsing and rerendering only text that has been
changed.  This module is used in `app-math` and `app-plain`, but
not in the "read-only" examples `demo-math` and `demo-plain`.

I addition increased speed, using `Differ` results in a smoother
user experience, since only small parts of the document being
edited need to be re-processed.
