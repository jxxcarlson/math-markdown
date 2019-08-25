module MDExample exposing (code, ll, math, md1, md1a, md2, md3, md3b, par1, par1a, par2, par2a, verbatim1, verbatim2, verbatim3)


md1 =
    """
$$
a^2 + b^2 = c^2
$$
"""


math =
    """# MMarkdown

$$
\\int_{-\\infty}^\\infty e^{-x^2} dx = \\pi
$$
"""


verbatim1 =
    """````
one
  two
    three
"""


verbatim2 =
    """````
one
  two
    three

three
   four
      five
"""


verbatim3 =
    """````
one
  two
    three



three
   four
      five
"""


code =
    """```
one == two
```
"""


md1a =
    """
one
$$
a^2 + b^2 = c^2
$$
"""


ll =
    """
- A
- B
  - uuu
  - vvv
- C
"""


md2 =
    """# Test document

This is a test.
I repeat.  A test.

## Lists

- Eggs
- Bread
  - White
  - Whole wheat
  - Hard rolls
- Tomatoes

````
Verbatim
  Block
    1
      2
````

```
for i in L:
  i * 8
```

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

> This is quote
which extends over
several lines

>> Twas brillig
and the slithey toves
did follow the damn borogroves

"""


par1 =
    """# Test document

This *is* a test.
I repeat.  A test.
I repeat!!!
a
b
c
"""


par1a =
    """# Test document:
a second try

This *is* a test.
I repeat.  A test.
I repeat!!!
a
b
c
"""


par2 =
    """# Test document

This *is* a test.
I repeat.  A test.

New paragaph:
red
blue
green
"""


par2a =
    """
This *is* a test.
I repeat.  A test.

New paragaph:
red
blue
green
"""


md3b =
    """# Test document

- One
This *is* a test.
I repeat.  A test.
"""


md3 =
    """# Test document

This *is* a test.
I repeat.  A test.

## Lists

- Eggs
- Bread
  - White
  - Whole wheat
  - Hard rolls
- Tomatoes

````
Verbatim
  Block
    1
      2
````

```
for i in L:
  i * 8
```

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

> This is quote
which extends over
several lines

>> Twas brillig
and the slithey toves
did follow the damn borogroves

"""
