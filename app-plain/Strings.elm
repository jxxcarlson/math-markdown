module Strings exposing (initialText, mathExampleText)


initialText =
    """# MMarkdown

MMarkdown is a dialect of Markdown which can render
formulas written in TeX/LaTeX.  It can, however, be
used without the math option, as this document
demonstrates.  MMarkdown is written in pure Elm. See notes on installation
at the end.

## Demo

MMarkdown implements the usual Markdown features such as headings,
bold, italic, and strike-through, links, images, etc. There is one extension, for poetry (see below).

MMarkdown is *paragraph-centric*, meaning that elements — headings, list elements,
displayed math, etc., must be separated by blank lines.

MMarkdown is now one of the markup options at [https://knode.io](knode.io).

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
# Partial sum of the harmonic series:

sum = 0
for n in range(1..100):
  sum = sum + 1.0/n
sum
```

## 4. Verbatim

A verabatim block begins and ends with four tick marks.
It is just like a code block, except that there is no
syntax highlighting.  Verbatim blocks are an extension
of normal Markdown.

````
Verbatim text has many uses:

   Element    |    Z
   --------------------
   Altium     |    4/5
   Brazilium  |    7/5
   Certium    |    9/5
````

## 5. Lists

Indent by four spaces for each level.  List items
are separated by blank lines.

- Solids

    - Iron *(metal)*

        - Iron disulfide (Pyrite), crystalline

        - Iron(II) sulfide, not stable, amorphous

    - Selenium *(use for solar cells)*

- Liquids

    - Alcohol *(careful!)*

    - Water *(Ok to drink)*

## 6. Numbered lists

### Final Exam


1. Discuss the shape of the earth, now viewed as a sphere.

  1. What is the earliest textual evidence that the earth was viewed as a sphere?

  2. Discuss the problem of navigation at sea as it relates to the shape of the earth.


4. The earth is said to be some 4 billion years old. How does one justify such a statement?


6. The universe is said to be some 14 billion years old.  How do we know that this is so?


## 7. Quotations


Quotations are offset:

> Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal.

> Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this.

> But, in a larger sense, we can not dedicate—we can not consecrate—we can not hallow—this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us—that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion—that we here highly resolve that these dead shall not have died in vain—that this nation, under God, shall have a new birth of freedom—and that government of the people, by the people, for the people, shall not perish from the earth.

— Abraham Lincoln, *Gettysbug Address*

## 8. Poetry

Poetry blocks, an extension of normal Markdown,
 begin with ">>"; line endings are respected.

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

## Installation

MMarkdown is designed to work with some CSS definitions. See the file `./index.html`
for these.
Compile using

```
elm make --optimize --output=Main.js
```

You can change the CSS to suit your needs.
You could also use

```
elm make --optimize --output=plain.html
```

However, at the present time, the text is not rendered properly in this case.



##  To do


- Tight lists ??

- Html ??

- Better error handling/messages

- Better recursion

- Spacing issues

"""


mathExampleText =
    "Nothing here"
