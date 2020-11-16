#' Get a vector of lemma
#'
#' For certain semantic roles syntactic information is not sufficient.
#' Distinguishing between saying and doing something, for instance, requires
#' looking at the specific lemma of the verb.
#' This function returns a vector of lemma for certain categories of verbs.
#'
#' Note that these are not complete lists, and what list works best
#' also depends on the interests of the researcher. For instance, does "believe"
#' (e.g., "Trump believed that ...") indicate a quote or not?
#'
#' @param what Name of the verb category.
#'
#' @return
#' @export
verb_lemma <- function(what = c('be', 'quote','should')) {
  what = match.arg(what)
  if (what == 'be') lemma = c('be')
  if (what == 'quote') lemma = c("tell", "acknowledge", "admit", "affirm", "allege", "argue", "announce", "assert", "attest", "avow", "call", "claim", "comment", "concede", "confirm", "declare", "deny", "describe","exclaim", "express", "insist", "mention", "note", "post","predict", "proclaim", "promise", "reply", "refer", "remark", "report", "say", "state", "suggest", "talk", "tell", "think","warn","write", "add")
  if (what == 'should') lemma = c('should','must')
  lemma
}

#' Function that adds column for modifiers
#'
#' Adds a column 'mod' that indicates if a token is negated (not, never), or has certain aux relation (should, must)
#'
#' @param tokens          A tokenindex
#' @param verbose         If true, report progress of queries
#'
#' @return                A tokenindex
#' @export
ud_mods <- function(tokens, should_verbs = verb_lemma('should'), verbose=T) {
  neg = tquery(label='not', fill=F,
               children(label='not', relation='dobj', req=F),
               children(relation='neg'))
  should = tquery(label='should', fill=F,
                  children(label='should', relation='dobj', req=F),
                  children(relation = 'aux', lemma=should_verbs))
  should_not = tquery(label='should_not', fill=F,
                      children(label='should_not', relation='dobj', req=F),
                      children(relation = 'neg'),
                      children(relation = 'aux', lemma=should_verbs))
  tokens = rsyntax::annotate(tokens, 'mod', sn=should_not, neg=neg, should=should, overwrite=T, verbose=verbose)
  tokens$mod_id = NULL
  tokens$mod_fill = NULL
  tokens
}



