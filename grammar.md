# Grammar

## Blocks

### Leaf Blocks

ThematicBlock -> Three or more consecutive characters ***... | ---... | ===...
  -- May also be intersperesed with spaces, and spaces may occur at the end


ATXHeader -> {0-3 spaces indentation}#{1-6} InlineContent #*

IndentedCodeChunk -> ({4+ spaces}NonBlankText)+

IndentedCodeBlock ->  (IndententedCodeChunk BlankLines)+

FencedCodeBlock -> ```AnyLines``` | ~~~AnyLines~~~

HTMLBlock ->   (BTAG string) lines (string ETAG)

BTAG = <script ...> | <pre ...> | <style ...> | <!-- ..
     | <? .. | <!  .. ETC
ETAG ...

LinkRef ->

Paragraph -> NonBlankLines+

MathBlock -> $$ String $$

### Container Blocks

Blockquote ->

MMListItem ->

MMList ->
