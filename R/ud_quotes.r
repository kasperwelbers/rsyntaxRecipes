#' Function that wraps tqueries for extracting quotes
#'
#' @param say_verbs       Specify which verbs are used as quote indicators.
#'                        By default uses the verbs in verb_lemma('quote').
#'                        Note that there is no perfect list of say_verbs, and
#'                        what verbs should be used is open to interpretation
#'
#' @return                A tokenindex
#' @export
#' @import rsyntax
ud_quote_tqueries <- function(say_verbs = verb_lemma('quote')) {
  source_fill = rsyntax::fill(NOT(relation=c('case','advmod','relcl','acl','acl:relcl')), connected=T)
  quote_fill = rsyntax::fill()
  verb_fill = rsyntax::fill(relation=c('prt','aux'), pos = c('PART','VERB'), connected=T)

  parataxis = tquery(label='quote', quote_fill,
                     children(label='verb', relation='parataxis', lemma=say_verbs, verb_fill,
                              children(label='source', relation= c('su', 'nsubj', 'agent', 'nmod:agent'), source_fill)))

  relcl = tquery(label='verb', relation = 'relcl', lemma=say_verbs, verb_fill,
                 parents(label='source', pos=c('PROPN','PRON'), source_fill),
                 children(label='quote', NOT(relation=c('su', 'nsubj', 'agent', 'nmod:agent','advmod')), quote_fill))

  direct = tquery(label='verb', lemma = say_verbs, verb_fill,
                  children(req=F, relation = c('npadvmod'), block=T),
                  children(label = 'source', relation=c('su', 'nsubj', 'agent', 'nmod:agent'), source_fill),
                  children(label = 'quote', NOT(relation=c('mark', 'advmod','conj', 'cc', 'prep','advcl')), quote_fill,
                           children(relation='mark', block = T, req = F)))

  nosrc = tquery(pos='VERB',
                 children(label='source', relation= c('su', 'nsubj', 'agent', 'nmod:agent'), source_fill),
                 children(label='verb', lemma = say_verbs, relation='xcomp', verb_fill,
                          children(label='quote', relation=c("ccomp", "dep", "parataxis", "dobj", "nsubjpass", "advcl"), quote_fill)))

  according = tquery(label='quote',
                      children(label = 'verb', lemma='accord',
                               children(lemma = 'to',
                                        children(label='source'))))

  advcl = tquery(label='verb', relation = 'advcl', lemma=say_verbs, verb_fill,
                 parents(pos='VERB',
                         children(label='source', relation=c('su', 'nsubj', 'agent', 'nmod:agent'), source_fill)),
                 children(label='quote', NOT(relation=c('su', 'nsubj', 'agent', 'nmod:agent', 'advmod', 'conj', 'cc', 'prep')), quote_fill))

  list(par=parataxis, relcl=relcl, dir=direct, nos=nosrc, acc=according, advcl=advcl)

}

#' Add multiline quotes
#'
#' Quotes can span across sentences. This function adds span quotes, as indicated
#' with quotation symbols, to an existing annotation column in tokenIndex.
#' The function first looks for all quotes (text between quotation marks) that
#' has not yet been labeled, and then determines who the most likely source is.
#'
#' NOTE THAT this function should be applied AFTER using (a chain of) tqueries
#' for finding sources. With the quote_column, source_value, and quote_value arguments
#' you refer to the results of these queries.
#'
#' @param tokens          a tokenIndex
#' @param quote_column    The column in which the annotations for the quotes tqueries
#'                        (that need to have been applied before) are stored. Typically
#'                        this is the value for the 'column' argument in the annotate_tqueries function.
#' @param source_value    The label of the source nodes (who says) in the quotes tqueries
#' @param quote_value     The label of the quote nodes (what is said) in the quotes tqueries
#' @param say_verbs       A vector of verb lemma indicating speech. By default uses
#'                        the list in verb_lemma('quote')
#'
#' @return A tokenIndex
#' @export
#' @import rsyntax
ud_span_quotes <- function(tokens, quote_column = 'quote', source_value='source', quote_value='quote', say_verbs = verb_lemma('quote')) {
  span1 = tquery(pos = 'VERB', lemma = say_verbs,
                 children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), pos=c('PRON','PROPN'), label='source'))
  span2 = tquery(pos = 'VERB',
                 children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), pos=c('PRON','PROPN'), label='source'))

  tokens = add_span_quotes(tokens, 'token', quote_col = quote_column, source_val = source_value,
                           quote_val = quote_value, tqueries=list(span1,span2),
                           add_quote_symbols="'‘’", quote_subset = pos != "PART")

  tokens

}
