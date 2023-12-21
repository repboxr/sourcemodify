
#' Key internal function fo sourcemodify
#' Returns parsed code as a tibble with detailed information
#' where each subexpression is found in the code
getAugmentedParseData = function(code, calls=parse(text=code,keep.source = TRUE), line.starts=find.line.starts(code), add_funid=TRUE) {
  restore.point("getAugmentedParseData")
  pd = getParseData(calls, includeText=FALSE)

  pd$start = line.starts[pd$line1] + pd$col1-1
  pd$end = line.starts[pd$line2] + pd$col2-1
  #pd$text = substring(code, pd$start, pd$end)

  res = try(substring(code, pd$start, pd$end))
  # For bug checking in Github Actions container
  if (is(res,"try-error")) {
    print(pd$start)
    print(pd$end)
    cat("\npd$col1\n")
    print(pd$col1)
    cat("\npd$col2\n")
    print(pd$col2)
    cat("\nline.starts\n")
    print(line.starts)
    pd$text = rep("", length(pd$text))
  } else {
    pd$text = res
  }


  # Set id equal to row number plus offset
  # parent=0 will be set to -i
  ids = pd$id
  pd$id = match(pd$id, ids)
  pd$parent = ifelse(pd$parent == 0, 0, match(pd$parent, ids))


  if (add_funid) {
    pd = pd_add_funid(pd)
  } else {
    pd$funid = rep(NA_integer_, NROW(pd))
  }
  pd
}


extractFunArgsParseData = function(pd, id) {
  restore.point("extractFunArgsParseData")
  crows = which(pd$parent == id)
  tokens = pd$token[crows]

  erows.ind = which(tokens=="expr")
  fun.name = pd$text[crows[erows.ind[1]]]
  erows.ind = erows.ind[-1]
  is.named = tokens[erows.ind-1] == "EQ_SUB"
  arg_name = ifelse(is.named, pd$text[crows[erows.ind-2]],NA)

  res.rows = crows[erows.ind]
  data.frame(row=res.rows,pid = id, fun.name=fun.name, arg_pos = seq_along(erows.ind), arg_name=arg_name, is.named = is.named, start = pd$start[res.rows], end=pd$end[res.rows])

}


getFunCallsParseData = function(pd) {
  restore.point("getFunCallsParseData")
  rows = which(pd$token == "expr")
  if (length(rows)==0) return(NULL)
  ids = pd$id[rows]
  txt = pd$text[rows]

  # Currently ignore calls with spaces after function name, like print (2)
  # also captures calls of the form pkg::fun or pkg:::fun
  is.fun.call = grepl("^(((([.[:alpha:]])[.[:alnum:]]*))::[:]?)?((([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*)|[.])\\(",txt)

  #is.fun.call = grepl("^((([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*)|[.])\\(",txt)

  rows = rows[is.fun.call]
  if (length(rows)==0) return(NULL)
  df = pd[rows,]
  df$pd.row = rows
  lhs = str.left.of(df$text,"(")
  df$pkg = str.left.of(lhs,"::", not.found=NA)
  df$fun = str.right.of(lhs, "::", not.found=lhs)
  df$parsed.args = FALSE
  df$args.df = vector("list", length(rows))
  df
}

getAssignmentParseData = function(pd) {
  restore.point("getAssignmentParseData")
  rows = which(pd$token %in% c("EQ_ASSIGN","LEFT_ASSIGN"))
  if (length(rows)==0) return(NULL)

  parents = pd$parent[rows]
  assign_ids = pd$id[rows]
  assign_df = tibble(ancestor=parents, assign_id = assign_ids)

  desc_df = find_descendant_ids(pd, parents, max.level=2) %>%
    arrange(ancestor, id) %>%
    left_join(select(pd, id, token, text, funid), by="id") %>%
    left_join(assign_df, by="ancestor")

  var_df = desc_df %>%
    group_by(ancestor) %>%
    mutate(is_fun = any(token=="FUNCTION")) %>%
    filter(token=="SYMBOL", id < assign_id) %>%
    slice(1) %>%
    mutate(lhs_parent_expr = pd$text[parent]) %>%
    mutate(lhs_parent_expr = ifelse(lhs_parent_expr==text,"", lhs_parent_expr))

  var_df = var_df %>% select(id=ancestor, var = text, is_fun, funid, lhs_parent_expr)
  var_df
}

# Adds an id of the function scope
pd_add_funid = function(pd) {
  restore.point("pd_add_funid")
  rows = which(pd$token %in% c("FUNCTION"))
  if (length(rows)==0) {
    pd$funid = rep(0, NROW(pd))
    return(pd)
  }
  ids = pd$parent[rows]

  desc_df = find_descendant_ids(pd, ids) %>%
    arrange(ancestor, id) %>%
    select(funid=ancestor, id=id) %>%
    group_by(id) %>%
    slice(n()) %>%
    ungroup()

  pd = left_join(pd,desc_df, by="id")
  pd$funid = ifelse(is.na(pd$funid),0, pd$funid)
  pd
}



# Find new line positions in a string that
# contains line breaks \n
find.line.starts = function(txt) {
  c(1,stringi::stri_locate_all_fixed(txt,"\n")[[1]][,1]+1)
}
