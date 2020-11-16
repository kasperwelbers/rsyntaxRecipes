ud_subject_does_by <- function(tokens) {
  # [subject] does something by [doing] [something else]
  #   - [subject] does something
  #   - [subject] [doing] [something else]

  tq = tquery(label='verb',
              children(relation='nsubj', label='subject'),
              children(relation='prep', lemma = 'by', label='prep',
                       children(relation='pcomp', label='pcomp')))

  select_nodes(tokens, tq) %>%
    copy_nodes('subject', new = 'subject_copy', copy_fill=T) %>%
    mutate_nodes('subject_copy', parent = pcomp$token_id) %>%
    mutate_nodes('pcomp', parent = NA, relation='ROOT') %>%
    remove_nodes('prep')
}

ud_relcl <- function(tokens) {
  # [subject], who [did] [something], [did] [something else]

  tq = tquery(relation = "relcl", label = "relcl",
              children(lemma = c("who", "that", "which"), label = "ref"),
              parents(label = "parent"))

  select_nodes(tokens, tq) %>%
    copy_nodes("parent", new = "copy", copy_fill = T) %>%
    mutate_nodes("copy", parent = ref$parent, relation = ref$relation) %>%
    remove_nodes("ref", with_fill = T) %>%
    mutate_nodes("relcl", parent = NA, relation = "ROOT")
}

ud_advcl <- function(tokens) {
  ## [subject] [verb] [something], [verb] [something else]

  tq = tquery(label = 'verb', pos = 'VERB',
              children(label = 'advcl', relation='advcl'),
              children(label = 'subject', relation = 'nsubj'))

  select_nodes(tokens, tq) %>%
    copy_nodes("subject", new = "copy", copy_fill = T) %>%
    mutate_nodes("copy", parent = advcl$token_id) %>%
    mutate_nodes("advcl", parent = NA, relation = "ROOT")
}

ud_object_for_doing_what <- function(tokens) {
  # something happens to [object] for [doing] [something else]
  #   - something happens to [object]
  #   - [object] [doing] [something else]

  tq = tquery(relation='pobj', label='object',
              children(relation='prep', lemma = c('for','on'), label='prep',
                       children(relation='pcomp', label='what')))


  select_nodes(tokens, tq) %>%
    copy_nodes('object', new = 'object_copy', copy_fill=T) %>%
    mutate_nodes('object_copy', parent = what$token_id, relation='nsubj') %>%
    mutate_nodes('what', parent = NA, relation='ROOT') %>%
    remove_nodes('prep')
}


ud_object_for_what <- function(tokens) {
  tq = tquery(label='verb',
              children(relation=c('dobj','nsubjpass'), label='object'),
              children(relation='prep', lemma = c('for','on'), label='prep',
                       children(relation='pobj', label='what')))


  select_nodes(tokens, tq) %>%
    copy_nodes('object', new = 'object_copy', copy_fill=T) %>%
    mutate_nodes('object_copy', parent = prep$token_id) %>%
    mutate_nodes('prep', parent = NA, relation='ROOT')
}

ud_object_to <- function(tokens) {
  tq = tquery(label='verb',
              children(relation=c('dobj','nsubjpass'), label='object'),
              children(relation='prep', lemma = c('to','with','out'), label='prep'))

  select_nodes(tokens, tq) %>%
    copy_nodes('verb', new = 'verb_copy', copy_fill=F) %>%
    copy_nodes('object', new = 'object_copy', copy_fill=T) %>%
    mutate_nodes('object_copy', parent = verb_copy$token_id) %>%
    mutate_nodes('prep', parent=verb_copy$token_id) %>%
    mutate_nodes('verb_copy', parent = NA, relation='ROOT')
}

ud_conj_compound <- function(tokens, conj_rel='conj', cc_rel='cc') {
  ## Change certain relations between two nodes with the same entity label,
  ## to compound, to prevent splitting them
  tq = tquery(label='parent', NOT(entity = ''),
              children(label= cc_rel, relation='cc'),
              children(label= conj_rel, relation='conj'))

  select_nodes(tokens, tq) %>%
    subset_nodes(subset = conj$entity == parent$entity & conj$entity == cc$entity) %>%
    mutate_nodes(node = 'conj', relation = 'compound') %>%
    mutate_nodes(node = 'cc', relation = 'compound')
}

ud_split_conjunctions <- function(tokens) {
  ## Use different fill settings for long and short distance conjunctions (as a rough heuristic for argument drop)
  no_fill_long_dist = c('acl:relcl','acl','appos','relcl', 'cop',
                        'advmod','advcl','xcomp','obl','ccomp','aux','det')
  no_fill_short_dist = c('acl:relcl','relcl', 'conj', 'cop')

  tokens %>%
    split_UD_conj(pos = 'VERB', right_fill_dist=F, no_fill=no_fill_long_dist) %>%
    split_UD_conj(min_dist = 3, no_fill=no_fill_long_dist) %>%
    split_UD_conj(no_fill= no_fill_short_dist)
}

#' Simplify tokenIndex created with the ud parser
#'
#' This is an off-the-shelf implementation of the different transformation
#' rules defined in rsyntaxRecipes.
#'
#' @param tokens A tokenIndex, based on output from the ud parser.
#'
#' @return a tokenIndex
#' @export
ud_simplify <- function(tokens, split_conj=T, rm_punct=F) {
  tokens = tokens %>%
    ud_conj_compound() %>%
    ud_relcl() %>%
    ud_advcl() %>%
    ud_object_for_doing_what() %>%
    ud_object_for_what() %>%
    ud_object_to() %>%
    ud_subject_does_by() #%>%
    #isolate_branch(relation = c('appos','acl','acl:relcl','relcl','advcl'))

  if (split_conj) tokens = ud_split_conjunctions(tokens)
  if (rm_punct) tokens = chop(tokens, relation = 'punct')
  tokens
}
