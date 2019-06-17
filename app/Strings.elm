module Strings exposing (initialText, mathExampleText)


initialText =
    "# Tests\n\n## 1\n\nThis is a test: $a^2 + b^2 = c^2$.\n\n## 2\n\nSo is this:\n\n"
        ++ "$$\n\\int_0^1 x^n dx = \\frac{1}{n+1}\n$$\n\n"
        ++ "**bold** *italic* ~~strike it out~~\n\nCode: `a := b;`\n\n"
        ++ "## 3. Code\n\nHe said that `abc = 0` is an initialization statement.\n\n"
        ++ "```\nsum = 0\nfor n in range(1..100):\n  sum = sum + 1.0/n\nsum\n```\n\n"
        ++ "## 4. Lists\n\n- Solids\n\n    - Iron\n\n    - Copper\n\n"


initialTextAll =
    """
# Tests

## 1

This is a test: $a^2 + b^2 = c^2$.

## 2

So is this:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

**bold** *italic* ~~strike it out~~

Code: `a := b;`

## 3. Code

He said that `sum = 0` is an initialization statement.

```
sum = 0
for n in range(1..100):
  sum = sum + 1.0/n
sum
```

### 4. Lists

- Solids

    - Bacon

    - Eggs

- Liquids

    - Orange Juice

    - Beer

"""


initialText1 =
    """

# Tests

## 1

This is a test: $a^2 + b^2 = c^2$.

## 2

So is this:

$$
    \\int_0^1 x^n dx = \\frac{1}{n + 1}
$$


I think that **bold text** is working,
as well as *italic text*.  ~~If not, let's try again.~~

[Working Example](https://ellie-app.com/5M6pVvF5BRta1)

[Working Example 2](https://ellie-app.com/5M7wMJZ4T83a1)

"""


mathExampleText =
    """
ddd

"""
