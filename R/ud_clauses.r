#' Function that wraps tqueries for extracting verb clauses
#'
#' @param verbs           Optionally, a character vector to specify specific verb lemma.
#' @param exclude_verbs   Optionally, a character vector to specify verb lemma to exclude.
#'                        By default uses verb_lemma('quote'), which is a vector of verbs indicating quotes (e.g., 'say','tell').
#'                        This excludes quotes, which requires a different approach (see ud_quotes)
#'
#' @return                A list with tqueries
#' @export
ud_clause_tqueries <- function(verbs=NULL, exclude_verbs=verb_lemma('quote')) {
  subject_lookup = AND(relation=c('nsubj', 'agent', 'nmod:agent','pobj','dobj','nsubjpass'))
  subject_fill = rsyntax::fill(NOT(relation=c('case','advmod','relcl','acl','acl:relcl','punct')), connected=T)

  verb_lookup = AND(lemma=verbs, NOT(lemma = exclude_verbs))
  verb_fill = rsyntax::fill(relation=c('prt','aux','neg','punct','advmod','acomp'), pos = c('PART','VERB','AUX','ADV','ADJ'), connected=T)

  predicate_lookup = NOT(relation=c('su', 'nsubj', 'agent', 'nmod:agent','aux','prt','advmod','neg','aux','auxpass', 'punct','mark'))
  predicate_fill = rsyntax::fill(NOT(relation=c('relcl','acl','acl:relcl','punct')), connected=T)


  relcl = tquery(label='verb', relation = 'relcl', verb_lookup, verb_fill,
                 parents(label='subject', subject_lookup, subject_fill),
                 children(label='predicate', predicate_lookup, predicate_fill))

  relcl_xcomp = tquery(label='verb', relation = 'relcl', verb_fill,
                       children(label='verb', verb_lookup,
                                children(label='predicate', predicate_lookup, predicate_fill)),
                       parents(label='subject', subject_lookup, subject_fill))

  passive = tquery(label='verb', pos = 'VERB', verb_lookup, verb_fill,
                   children(label='subject', relation = c('agent'), subject_fill),
                   children(label='predicate', predicate_lookup, predicate_fill))

  direct = tquery(label='verb', pos = 'VERB', verb_lookup, verb_fill,
                  children(label='subject', subject_lookup, subject_fill),
                  children(label='predicate', predicate_lookup, predicate_fill))

  cop1 = tquery(label='predicate', pos = 'VERB', verb_lookup, predicate_lookup, predicate_fill,
                  parents(label='verb', verb_lookup, verb_fill,
                          children(label='subject', subject_lookup, subject_fill)))

  cop2 = tquery(label='verb', pos = c('VERB','AUX'), verb_lookup, verb_fill,
                parents(label='predicate', NOT(relation=c('aux','prt','advmod')), predicate_fill,
                        children(label='subject', subject_lookup, subject_fill)))

  poss_acl = tquery(label='verb',
               children(label='verb', relation='acl', verb_lookup, verb_fill,
                        children(label='predicate', predicate_lookup, predicate_fill)),
               children(label='subject', relation = c('poss'), subject_fill))

  acl = tquery(label='predicate', predicate_fill,
                children(label='verb', relation='acl', verb_lookup, verb_fill,
                         children(label='subject', subject_lookup, subject_fill)))

  xcomp = tquery(label = 'verb', relation='xcomp', verb_fill,
                 parents(label='subject', subject_fill),
                 children(label='predicate', predicate_lookup, predicate_fill))

  list(relcl=relcl, relcl_xcomp=relcl_xcomp, pas=passive, dir=direct, cop1=cop1, cop2=cop2, pacl=poss_acl, acl=acl, xcomp=xcomp)
}







