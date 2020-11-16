
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rsyntax recipes

This is a repository for sharing
[rsyntax](https://github.com/vanatteveldt/rsyntax) code. The goal is to
provide useful tqueries and reshape functions, that you can use (as a
starting point) for text analysis using syntactic dependency trees.

# Universal Dependencies

We will mainly focus (and currently only focus) on code for dependency
tree data using the [Universal
Dependencies](https://universaldependencies.org/) scheme. This scheme is
widely used (e.g., spaCy, UDpipe). However, note that due to differences
in how these parsers are designed, they do not always give the same
parse trees, meaning that recipes are not directly transferable without
some loss of performance.

Also, the UD scheme aims to be universal across languages. Hopefully,
this means that queries developed for English language also work for
other languages. However, note that this has limitations. In particular,
some features of rsyntax might rely on implicit conventions in language
rather than explicit syntactic structures. This is especially the case
for the more complicated tree simplifications.

# How to use

The easiest way to use these recipes is by using either the
[spacyr](https://github.com/quanteda/spacyr) or
[udpipe](https://github.com/bnosac/udpipe) package to parse texts. Make
sure to enable dependency parsing. For example, here we use spacyr.

``` r
library(rsyntax)
#> rsyntax uses the data.table package, but limits the number of threads used:
#>  - data.table currently uses 4 threads
#>  - rsyntax uses 2 threads
#> 
#> You can use set_rsyntax_threads() to use all data.table threads, or set a specific number
library(rsyntaxRecipes)
library(spacyr)
txt = 'Trump has "cloaked America in darkness for too long", Joe Biden said'
tokens = spacy_parse(txt, dependency=T)
#> Found 'spacy_condaenv'. spacyr will use this environment
#> successfully initialized (spaCy Version: 2.1.9, language model: en_core_web_sm)
#> (python options: type = "condaenv", value = "spacy_condaenv")
tokens = as_tokenindex(tokens)
```

With `as_tokenindex` the tokenindex is transformed to a `data.table`
with certain special characteristics. This function works on both the
output of `udpipe` and `spacyr` (and on other data, but you might have
to specify column names).

*Note: For some parsers, you might have to rename some columns. The
as\_tokenindex function automatically renames some critical columns for
rsyntax, but the recipes in this repository also use the pos and lemma
columns. In some parsers these columns have different names, such as
`upos` in udpipe. If you get the warning “‘pos’ is not a valid column
name in tokens”, it means you’ll have to rename the part-of-speech tag
column to ‘pos’.*

As an example, we’ll apply the clause queries. The
`ud_clause_tqueries()` function returns a list of tqueries. In `rsyntax`
this acts as a chain of queries, to be applied in succession.

``` r
tq = ud_clause_tqueries()
tokens = annotate_tqueries(tokens, column='clause', tq)
```

The tokens data now has a `clause` and `clause_id` column, assigning the
`subject`, `verb` and `predicate` labels to tokens. One way to
investigate how well this performs on your data is to use the plot\_tree
function, and passing the column name for the annotations to the
annotation argument.

``` r
plot_tree(tokens, annotation='clause')
#> Document: text1
#> Sentence: 1
```

Here we see that “Trump” is the subject, “has cloaked” is the verb, and
“America in darkness for too long” is the predicate.

Another example is quote extraction. Here we first annotate the
`ud_quote_tqueries()`. But in addition, we use the `ud_span_quotes()`
function, which helps deal with quotes that span across sentences.

``` r
tokens = tokens %>%
  annotate_tqueries('quote', ud_quote_tqueries()) %>%
  ud_span_quotes()
```

As an alternative method to validate results, you can create a full text
browser with the annotations highlighed.

``` r
syntax_reader(tokens, annotation='quote', value='source', value2='quote')
```

Here we see that “Joe Biden” is the source, who said that “Trump has
‘cloaked America in darkness for too long’”.

# How to modify

The idea of this cookbook is not to provide perfect solutions for every
occasion. Just as with off-the-shelf dictionaries, it is recommended to
manually validate results, and if necessary adjust the queries (add,
remove, modify) to work better on your use case. Towards this end, you
can copy the code (or clone this repository) as a starting point. Please
see the `rsyntax` github page for instructions on how to work with this
package.
