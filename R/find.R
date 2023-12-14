
#' Returns the results from the last somo_find_* call as a tibble
somo_found = function(somo) {
  somo$found
}

#' A simplified interface to somo_find_fun_arg
#'
#' Only works with named arguments.
#'
somo_calls_info = function(somo, fun, arg_names, arg_pos = NULL, arg_def = NULL, call_pd_rows=NULL, arg_prefix="") {
  restore.point("somo_calls_info")

  call_pd = somo$call_pd

  if (is.null(arg_def))
    arg_def = get.fun.args.def(fun)

  if (is.null(call_pd_rows)) {
    call_pd_rows = which(call_pd$fun == fun)
  }

  res.df = tibble(call_pd_row = call_pd_rows, fun=fun)

  if (NROW(res.df)==0) {
    # Add empty columns
    for (arg_name in arg_names) {
      res.df[[paste0(arg_prefix,arg_name)]] = character(0)
    }
    somo$found = res.df
    return(somo)
  }

  for (i in seq_along(arg_names)) {
    arg_name = arg_names[i]
    somo = somo_find_fun_arg(somo, fun, arg_name=arg_name,arg_pos = arg_pos[i], arg_def=arg_def, call_pd_rows = call_pd_rows)
    found = somo$found
    res.df[[paste0(arg_prefix,arg_name)]] = found$text
  }
  somo$found = res.df
  somo
}

#' Find the text of the function argument in all calls to fun
somo_find_fun_arg = function(somo, fun, arg_name=NULL, arg_pos=NULL, arg_def = NULL, call_pd_rows=NULL) {
  restore.point("somo_find_fun_arg")
  call_pd = somo$call_pd

  if (is.null(arg_def))
    arg_def = get.fun.args.def(fun)

  if (is.null(call_pd_rows)) {
    rows = which(call_pd$fun == fun)
  } else {
    rows = call_pd_rows
  }
  if (length(rows)==0) {
    somo$found = NULL
    return(somo)
  }
  res.li = vector("list", length(rows))
  search.arg_name = ifelse(is.null(arg_name), NA_character_, arg_name)
  search.arg_pos = ifelse(is.null(arg_pos), NA_integer_, arg_pos)


  i = 1
  for (i in seq_along(rows)) {
    row = rows[i]

    if (!call_pd$parsed.args[row]){
      call_pd$args.df[[row]] = extractFunArgsParseData(somo$pd, call_pd$id[[row]])
      call_pd$parsed.args[[row]] = TRUE
    }
    args.df = call_pd$args.df[[row]]
    ma.row = match.arg.with.args.df(arg_name, arg_pos, args.df, arg_def, pd=somo$pd)
    if (!is.na(ma.row)) {
      code_ind = call_pd$code_ind[row]
      res.li[[i]] = cbind(data.frame(call_pd_row = row, fun=fun,search.arg_name=search.arg_name, search.arg_pos = search.arg_pos, found.arg=TRUE, text=substring(somo$code_df$code[code_ind], args.df$start[ma.row], args.df$end[ma.row])))
    } else {
      res.li[[i]] = data.frame(call_pd_row = row, fun=fun,search.arg_name=search.arg_name, search.arg_pos = search.arg_pos, found.arg=FALSE, text=NA)
    }
  }
  somo$call_pd = call_pd
  somo$found = bind_rows(res.li)
  somo
}

#' Find the text of the function argument in all calls to fun
somo_find_fun_call = function(somo, fun) {
  restore.point("somo_find_fun_call")
  call_pd = somo$call_pd
  rows = which(call_pd$fun == fun)
  if (length(rows)==0) {
    somo$found = NULL
    return(somo)
  }
  somo$found = somo$call_pd[rows,]
  somo
}

find_somo_fun_call = function(somo, fun) {
  somo_find_fun_call(somo, fun) %>% somo_found()
}

find_somo_descendant_ids = function(somo, ids, max.level=Inf) {
  find_descendant_ids(somo$pd, ids, max.level)
}

find_descendant_ids = function(pd, ids, max.level=Inf) {
  pd_df = select(pd, parent, id)

  anc_df = tibble(ancestor = ids, parent=ids)

  max.level = min(n_distinct(pd$parent), max.level)

  df_li = vector("list", max.level)

  i = 1
  df_li[[i]] = inner_join(anc_df, pd_df, by=c("parent")) %>% mutate(level=i)

  i = 2
  for (i in setdiff(seq_len(max.level),1)) {
    prev_df = df_li[[i-1]] %>% mutate(parent=id) %>% select(-id)
    new_df = inner_join(prev_df, pd_df, by=c("parent")) %>% mutate(level=i)
    if (NROW(new_df)==0) break
    df_li[[i]] = new_df
  }

  df = bind_rows(df_li)
  df
}

