module Strings exposing (initialText, mathExampleText)


initialText =
    """# Math Markdown

MathMarkdown is a dialect of Markdown which renders
formulas written in TeX/LaTeX.  Suppose that you write

```
f(a) = \\frac{1}{2\\pi i}
        \\oint\\frac{f(z)}{z-a}dz
```

Then MathMarkdown uses [https://mathjax.org](MathJax)
to render the formula like this:

$$
  f(a) = \\frac{1}{2\\pi i} \\oint\\frac{f(z)}{z-a}dz
$$

The present document illustrates most of the features of MathMarkdown — math, as
you have just seen, plus headings,
bold, italic, and strike-through, links, images, etc. (You can directly
compare the source and rendered MathMarkdown text
using the demo app at [https://markdown.minilatex.io](markdown.minilatex.io))


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

## 5. Numbered lists

#### Problem Set 18

1. Compute the coefficient of $a^5b^2$ in $(a + b)^7$.

4. If $f'(2) = 0$, what can you say about the graph of $f$ at $x = 2$?


6. Suppose that in addition, $f''(2) > 0$. What else can say about the graph?


### Problem Set 19

4. Show that $u(x,t) = f(x - ct)$ is a solution to the equation $\\partial u(x,t)/\\partial x + c^{-1} \\partial u(x,t)/\\partial = 0$.

3. State the wave equation and show that $u(x,t)$ as above is a solution to it.

2. In what direction does the wave defined by $u(x,t) = f(x - ct)$ move?

4.  Find a solution of the wave equationthat represents a pulse moving in the opposite direction.


## 6. Quotations

Of course, we want to be able to set of quotations, like this one:

> He was said to have said, **This** is a Test.
I repeat, a test.
I repeat, I repeat, I repeat ... *and so he went on
on and on, caught in a whirlpool of redundancy,
spiralling ever faster as his verbal circle contracted
towards a single point, a punctuation mark that
was both beginning and end.* — Publius Minimus, Esq.

Hmmm ... don't really know what to make of that.


## 7. Poetry

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


NOTE: Math Markdown is also implemented as an option for [https://knode.io](knode.io).
knode offers MiniLaTeX, a web-friendly subset of TeX/LaTex.  To see
how it works without a sign-in, please see [https://demo.minilatex.app](demo.minilatex.app).


___


##  To do

-  Correct numbering of nested lists, reset numbering on exit from list, proper formatting.

- Tight lists

- Html ??

- Better error handling/messages

- Better recursion

- Spacing issues

"""


mathExampleText =
    "Nothing here"
