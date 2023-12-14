add_id_col = function(df, idname, val) {
  df[[idname]] = rep(val, NROW(df))
  df = df[union(idname, names(df))]
  df
}

get.fun.args.def = function(fun) {
  res = try(names(as.list(args(fun))), silent=TRUE)
  if (is(res, "try-error")) return(NULL)
  res
}

has.col = function(x, col) {
  col %in% names(x)
}

remove.col = function(x, col) {
  x[setdiff(colnames(x),col)]
}
