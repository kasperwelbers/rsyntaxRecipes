#' Function that wraps tqueries for extracting associations
#'
#' @description
#' A collection of tqueries for indicating associations for a subject. This includes
#' modifiers, but also clauses where the verb indicates associations (see verbs argument)
#'
#' @param verbs           Verbs that indicate associations. Default is only 'be'.
#'
#' @return                A list with tqueries
#' @export
ud_association_tqueries <- function(verbs = c('be')) {
  prep = tquery(label='subject', fill(relation=c('compound*','flat')),
                not_parents(relation='prep'),
                children(relation=c('prep'), lemma = c('of','from'), label='predicate'))

  mod = tquery(label='subject', pos=c('NOUN','PROPN'), NOT(relation='compound*'), fill(relation='compound*'),
               children(relation=c('appos','amod'), label='predicate',
                        fill(relation=c('compound*','flat','appos','amod'))))


  compound_mod = tquery(label='subject', pos=c('NOUN','PROPN'), NOT(relation='compound*'), fill=F,
                        children(relation=c('compound*','flat'), label='subject',
                                 children(relation=c('compound*','flat','appos','amod'), label='predicate')))

  c(ud_clause_tqueries(verbs = 'be'),
    list(prep=prep, mod=mod, commod = compound_mod))
}



