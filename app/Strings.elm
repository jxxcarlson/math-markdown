module Strings exposing (initialText, mathExampleText)


initialTextAll =
    "# Tests\n\n## 1\n\nThis is a test: $a^2 + b^2 = c^2$.\n\n## 2\n\nSo is this:\n\n"
        ++ "$$\n\\int_0^1 x^n dx = \\frac{1}{n+1}\n$$\n\n"
        ++ "**bold** *italic* ~~strike it out~~\n\nCode: `a := b;`\n\n"
        ++ "## 3. Code\n\nHe said that `abc = 0` is an initialization statement.\n\n"
        ++ "```\nsum = 0\nfor n in range(1..100):\n  sum = sum + 1.0/n\nsum\n```\n\n"
        ++ "## 4. Lists\n\n- Solids\n\n    - Iron\n\n    - Copper\n\n"


initialText =
    """# Math Markdown

$$
  f(a) = \\frac{1}{2\\pi i} \\oint\\frac{f(z)}{z-a}dz
$$

This is an experimental app using the even more experimental jxxcarlson/elm-mathmarkdown
library.  The parser is still a very long ways from implementing the
CommonMark spec, something that we eventually hope to do.  Nonetheless, MathMarkdown
does offer basic functionality plus (and this is the main point), the ability to
render math-mode LaTeX: the stuff between dollar signs.  Rendering of math
text is provided by [https://mathjax.org](MathJax).

If you need a more sophisticated live math app, please take
a look at [https://minilatex.io](minilatex.io).

The present document illustrates most of the features of MathMarkdown.  Just compare
the source text on the left with the rendered text on the right.


![Hummingbird](https://www.allaboutbirds.org/guide/noindex/photo/60395551-1280px.jpg)

Hummingbird (Meditation)

Link: [http://nytimes.com](New York Times)

Text styles: **bold** *italic* ~~strike it out~~


## 1 Inline Math

This is a test: $a^2 + b^2 = c^2$.

## 2 Display Math

So is this:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$


## 3. Code

He said that `a := 0` is an initialization statement.

```
sum = 0
for n in range(1..100):
  sum = sum + 1.0/n
sum
```

## 4. Lists

- Solids

    - Iron *(metal)*

    - Selenium *(use for solar cells)*

- Liquids

    - Alcohol *(careful!)*

    - Water *(Ok to drink)*

## 5. Quotations

Of course, we would like to have quotations, like this one:

> He was said to have said, **This** is a Test.
I repeat, a test.
I repeat, I repeat, I repeat ...

Isn't that nice?

## 6. Poetry

Poetry blocks are an extension of Markdown.
Begin a poetry block with ">>".

>> Twas brillig, and the slithy toves
Did gyre and gimble in the wabe:
All mimsy were the borogoves,
And the mome raths outgrabe.

>> Beware the Jabberwock, my son!
The jaws that bite, the claws that catch!
Beware the Jubjub bird, and shun
The frumious Bandersnatch!


Etcetera!

___


##  To do

- Numbered lists

- Tight lists

- Thematic breaks

- Better error handling/messages

- Better recursion


"""


mathExampleText =
    "Nothing here"
