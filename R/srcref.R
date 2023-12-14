# Utility functions for working with source references

sys_calls_df = function(calls = sys.calls(),remove_start=0, remove_end = 0, only_with_srcref=FALSE) {
  from = 1+remove_start
  to = length(calls) - remove_end
  if (from > to) return(NULL)

  res_li = lapply(from:to, function(i) {
    ca = calls[[i]]
    srcref = attr(ca, "srcref")
    if (is.null(srcref)) {
      if (only_with_srcref) return(NULL)
      code = deparse1(ca)
      data.frame(stack_pos = i, code=code, file=NA_character_, line1 = NA_integer_, line2 = NA_integer_, col1 = NA_integer_, col2 = NA_integer_)
    } else {
      srcref_to_df(srcref,i)
    }
  })
  df = bind_rows(res_li)
  #df$stack_pos = seq_len(NROW(df))
  df
}

srcref_to_df = function(srcref, stack_pos = NA_integer_) {
  code = as.character(srcref)
  srcfile = attr(srcref, "srcfile")
  file = srcfile$filename

  data.frame(stack_pos = stack_pos, code=code,file=file, line1 = srcref[1], line2 = srcref[3], col1 = srcref[5], col2 = srcref[6])
}

# join_calls_df_with_pd_id = function(calls_df,pd) {
#   if (has.col(calls_df,"id")) {
#     calls_df = remove.col(calls_df,"id")
#   }
#   left_join(calls_df, select(pd, file=codeid,id, line1, line2, col1, col2), by=c("file","line1","line2","col1","col2"))
#
# }
