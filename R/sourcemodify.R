example = function() {
  code = "
myfun = function(x=5) {
  z*5
  myfun2 = function(y=3) {
    a = 5
  }
  return(x+2)
}

# First comment
install.packages(c('pkg1','pkg2'))
x <- 5
a = 5
y[1]=3

y = sum(1:sum(2:3), na.rm=TRUE)
if (x < 4) {
  read.csv(sep=',','/mypath/myfile.R')
  base::read.csv(file='/mypath/myfile.R')
}
# Here is a comment
stats::lm(y~x, data=dat)
x
"
  code = "
x=1:2
for (i in 1:2) {
  print(i)
}

y=sum(x)
z=sum(x) + sum(y)
"

  somo = somo_init(list(c1 = code, c2=code))
  pd = somo$pd
  call_pd = somo$call_pd
  as_df = somo$as_df

  somo = somo %>%
    somo_change_calls_fun_name("sum","my.sum",ignore_if_pkg = TRUE)


  somo_make_pd_code(somo)

    somo_surround_calls(funs = "lm",pre = "repbox.reg(",post=")") %>%
    somo_surround_calls_arg(fun = "read.csv", arg_name = "file",pre="repbox.path(", post=")")
  cat(somo_make_code(somo)[2])



  somo = somo %>%
    somo_change_calls_fun_name("sum","my.sum",ignore_if_pkg = TRUE) %>%
    somo_surround_calls(funs = "lm",pre = "repbox.reg(",post=")") %>%
    somo_surround_calls_arg(fun = "read.csv", arg_name = "file",pre="repbox.path(", post=")")
  cat(somo_make_code(somo)[2])

  somo_find_fun_arg(somo,"read.csv","file",1) %>%
    somo_found()

  find_somo_fun_call(somo,"read.csv")

  somo_find_fun_arg(somo,"read.csv","sep") %>%
    somo_found()

  somo_find_fun_arg(somo,"install.packages","pkgs",1) %>%
    somo_found()

  somo_calls_info(somo, "read.csv",c("file","sep"))

  somo$found

  cat(somo_make_code(somo))






    res = pos.for.surround.fun.args(code, "read.csv","find.path(",")",arg="file", first.arg=TRUE)
  res
  cat(res$code)


  calls = parse(text=code)
  pd = getAugmentedParseData(code)
  call_pd = get.fun.calls.parse.data(code=code)
  res = surround.fun.calls(code, "read.csv","check.path(",")")
  cat(res$code)




  code = "read.csv('myfile')"
  new.code = surround.fun.argument(code, "read.csv","utils",arg="file",pre = "find.path(", post=")", first.arg=TRUE)
  cat(new.code)
}

#' Init a new source modifier object
#'
#' @param code The R source code as text file
#' @param add_funid Shall for each expression be determined inside which function it is? Computation sometimes be quite time intensive.
somo_init = function(code=NULL,files=NULL, start.size = 200, encoding="UTF-8", add_funid=TRUE) {
  restore.point("somo_init")
  if (is.null(code) & !is.null(files)) {
    code = lapply(files, function(file) {
      txt = readLines(file, encoding=encoding, warn = FALSE)
      paste0(txt, collapse = "\n")
    })
    names(code) = files
  } else if (!is.null(files)) {
    names(code) = files
  }

  code_li = as.list(code)

  files = names(code_li)
  if (is.null(files)) files = rep("", length(code_li))

  res_li = vector("list", length(code_li))
  offset = 0
  i = 1
  for (i in seq_along(code_li)) {
    res = init.somo.single.code(i=i,code_li[[i]], offset=offset, add_funid=add_funid)
    res_li[[i]] = res
    offset = offset + NROW(res$pd[[1]])
  }

  res_df = bind_rows(res_li)

  cat("\nres_df")
  print(res_df)

  offsets = c(0, sapply(res_df$pd, NROW))
  cat("\noffsets")
  print(offsets)

  pd = bind_rows(res_df$pd)
  cat("\npd")
  print(pd)

  somo = list(
    code_df = tibble(
      code_ind=seq_along(code_li), file = files, code=res_df$code, start_id=offsets[-length(offsets)]+1, end_id = offsets[-1]
    ),
    pd = pd,
    call_pd = bind_rows(res_df$call_pd),
    as_df = getAssignmentParseData(pd),
    mod_li = vector("list", start.size),
    num_mod = 0
  )
  somo
}

init.somo.single.code = function(i, code, offset, add_funid = TRUE) {
  restore.point("init.somo.single.code")
  calls = parse(text=code, keep.source=TRUE)
  line.starts = find.line.starts(code)
  pd = getAugmentedParseData(code,calls=calls, line.starts = line.starts,add_funid=add_funid) %>% add_id_col("code_ind",i)

  # Set id equal to row number plus offset
  # parent=0 will be set to -i
  pd$id = pd$id + offset
  pd$parent = ifelse(pd$parent == 0, -i, pd$parent + offset)
  pd$funid = ifelse(pd$funid == 0, 0, pd$funid + offset)

  call_pd = getFunCallsParseData(pd)
  tibble(code_ind=i, code=code, calls=list(calls), pd = list(pd), call_pd = list(call_pd))
}

#' Change the name of a called function without adapting any arguments
#'
#' If the function call directly
#'
#' @param somo The somo object generated with somo_init.
#' @param fun Function name that shall be changed in calls
#' @param new New function name to be called
#' @param ignore_if_pkg If TRUE don't change calls of the form pkg::fun(). If FALSE a call like pkg::fun() will be replaced by just new().
#' @returns The modified somo object
somo_change_calls_fun_name = function(somo,  fun, new, ignore_if_pkg=TRUE) {
  restore.point("somo_change_calls_fun_name")
  pd = somo$call_pd
  if (ignore_if_pkg) {
    rows = which(pd$fun %in% fun & is.na(pd$pkg))
    if (length(rows)==0) return(somo)
    len = nchar(pd$fun[rows])
  } else {
    rows = which(pd$fun %in% fun)
    if (length(rows)==0) return(somo)
    text = pd$text[rows]
    len = str.locate.first(text,"(")[,1]-1
  }
  mod = data.frame(code_ind=pd$code_ind[rows],type="r", start=pd$start[rows], end = pd$start[rows]+len-1, new = new, id=pd$id[rows])
  somo$num_mod = somo$num_mod+1
  somo$mod_li[[somo$num_mod]] = mod
  somo
}




#' Surround calls to the specified functions
#'
#' Will by typically used to wrap a function call into
#' another function call that can analyse or modify the result.
#'
#' @param somo The somo object generated with somo_init.
#' @param funs One or several function names.
#' @param pre Text to be added before the function call.
#' @param post Text to be added the function call.
#' @returns The modified somo object
somo_surround_calls = function(somo,  funs=NULL, pre, post, call_pd_rows=NULL) {
  restore.point("somo_surround_calls")
  pd = somo$call_pd

  if (!is.null(funs) & is.null(call_pd_rows)) {
    rows = which(pd$fun %in% funs)
  } else {
    rows = call_pd_rows
  }
  if (length(rows)==0) {
    return(somo)
  }
  somo$num_mod = somo$num_mod+1
  somo$mod_li[[somo$num_mod]]  = data.frame(code_ind = pd$code_ind[rows],type="pp", start=pd$start[rows], end = pd$end[rows], pre=pre, post=post, id=pd$id[rows])
  somo
}




#' Surround an argument of specific function
#'
#' Will by typically used to wrap the argument (like a filenam)
#' of function call into another function call
#' that can analyse or modify the argument.
#'
#' @param somo The somo object generated with somo_init.
#' @param fun Name of the function whose argument shall be wrapped
#' @param arg_name The name of the argument (must be given if arg_pos is NULL)
#' @param arg_pos Position of the argument (must be given if arg_name is NULL)
#' @param pre Text to be added before the function call.
#' @param post Text to be added the function call.
#' @param arg_def Names of all function arguments in the correct order. The result of calling names(as.list(args(fun))). This argument is optional, but without it matching may fail.
#' @returns The modified somo object
somo_surround_calls_arg = function(somo,  fun=NULL, arg_name=NULL, arg_pos=NULL, pre, post, arg_def=NULL, call_pd_rows=NULL) {
  restore.point("somo_surround_calls_arg")
  call_pd = somo$call_pd

  if (!is.null(fun) & is.null(call_pd_rows)) {
    rows = which(call_pd$fun == fun)
    rows = which(call_pd$fun %in% fun)
  } else {
    rows = call_pd_rows
    fun = first(call_pd$fun[rows])
  }

  pre = rep(pre, length.out=length(rows))
  post = rep(post, length.out=length(rows))

  if (length(rows)==0) {
    return(NULL)
  }

  if (is.null(arg_def))
    arg_def = get.fun.args.def(fun)

  row = rows[1]

  for (i in seq_along(rows)) {
    row = rows[i]
    if (!call_pd$parsed.args[row]){
      call_pd$args.df[[row]] = extractFunArgsParseData(somo$pd, call_pd$id[[row]])
      call_pd$parsed.args[[row]] = TRUE
    }
    args.df = call_pd$args.df[[row]]
    ma.row = match.arg.with.args.df(arg_name, arg_pos, args.df, arg_def, pd=somo$pd)
    if (!is.na(ma.row)) {
      somo$num_mod = somo$num_mod+1
      somo$mod_li[[somo$num_mod]]  = data.frame(code_ind = call_pd$code_ind[row],type="pp", start=args.df$start[ma.row], end = args.df$end[ma.row], pre=pre[i], post=post[i], id=call_pd$id[row])
    }
  }
  somo$call_pd = call_pd
  somo
}

# Tries to match a specified argument with the row of
# the args.df extracted from the parsing data
match.arg.with.args.df = function(arg_name=NULL, arg_pos=NULL, args.df, arg_def = NULL,pd, verbose=TRUE) {
  restore.point("match.arg.with.args.df")
  if (NROW(args.df)==0) return(NA)
  if (!is.null(arg_name)) {
    ma.row = match(arg_name, args.df$arg_name)
    if (!is.na(ma.row)) return(ma.row)
  }
  if (!is.null(arg_pos)) {
    if (NROW(args.df)<arg_pos)
      return(NA)
    if (all(is.na(args.df$arg_name[1:arg_pos]))) {
      return(arg_pos)
    }
  }
  if (is.null(arg_def)) {
    if (verbose) {
      pd.row = match(args.df$pid[1], pd$id)
      call = pd$text[pd.row]
      cat(paste0("\nCould not non-ambiogously match argument ", arg_name, " ", arg_pos, " in call ", call, " since no function definition could be obtained. Make sure that the corresponding library is loaded."))
    }
    return(NA)
  }
  if (!is.null(arg_name) & is.null(arg_pos)) {
    arg_pos = match(arg_name, arg_def)
    if (is.na(arg_pos)) {
      if (verbose) {
        pd.row = match(args.df$pid[1], pd$id)
        call = pd$text[pd.row]
        cat(paste0("\nCould not match argument ", arg_name, " for call ", call, " because the argument ", arg_name, " is not found in the specified function definition. Perhaps the wrong library was used for the function definition."))
      }
      return(NA)
    }
  }

  # We now have an arg_pos. We should shift arg_pos for named
  # arguments that were placed before arg_pos but are later in
  # the function definition
  later.def = arg_def[-(1:arg_pos)]
  pos.later.def = match(later.def, args.df$arg_name)
  shift = sum(pos.later.def <= arg_pos, na.rm=TRUE)
  arg_pos = arg_pos + shift
  if (arg_pos > NROW(args.df)) return(NA)
  return(arg_pos)
}


#' Returns the modified code of a somo object
somo_make_code = function(somo) {
  restore.point("somo_make_code")
  if (somo$num_mod==0) {
    return(somo$code_df$code)
  }

  all_mod_df = bind_rows(somo$mod_li[1:somo$num_mod]) %>%
    group_by(code_ind) %>%
    arrange(start) %>%
    mutate(
      is.nested = end < cummax(end),
      nest.level = 0
    ) %>%
    ungroup()


  cid = 1
  codeinds = sort(unique(all_mod_df$code_ind))
  code = somo$code_df$code
  code[codeinds] = sapply(codeinds, function(cid) {
    mod_df = all_mod_df[all_mod_df$code_ind==cid,]
    i = cid
    code = somo$code_df$code[i]

    rows = which(mod_df$is.nested)

    # No nested replacements, we can directly use stringi
    if (length(rows)==0) {
      value = get.mod_df.value(mod_df, code)
      new.code = stringi::stri_sub_replace_all(code, from=list(mod_df$start), to = list(mod_df$end), value = value)
      return(new.code)
    }

    #
    # NESTED REPLACEMENTS ARE NOT YET WELL TESTED!
    #
    # We have nested replacements
    # First perform all nested replacements from inner
    # to outer and adapt start and end of mod_df
    # Afterwards use stringi::stri_sub_replace_all
    end = mod_df$end
    for (row in rows) {
      mod_df$nest.level[row] = sum(end[seq_len(row-1)]>mod_df$end[row])
    }

    for (level in max(mod_df$nest.level):1) {
      rows = which(mod_df$nest.level == level)
      for (row in rows) {

        start = mod_df$start[row]
        end = mod_df$end[row]
        # Add prefix and postfix
        if (mod_df$type[row]=="pp") {
          pre = mod_df$pre[row]
          post = mod_df$post[row]
          value = paste0(pre, substring(code, start, end), post)
          code = stringi::stri_sub_replace(code, from=start, to = end, value = value)
          add = nchar(pre)+nchar(post)
        # Directly replace
        } else {
          value = mod_df$new[row]
          code = stringi::stri_sub_replace(code, from=start, to = end, value = value)
          add = nchar(value) - (end-start+1)
        }

        arows = which(mod_df$start > end)
        mod_df$start[arows] = mod_df$start[arows]+add
        arows = which(mod_df$end > end)
        mod_df$end[arows] = mod_df$end[arows]+add
      }
    }

    rows = which(mod_df$nest.level == 0)
    value = paste0(mod_df$pre[rows], substring(code, mod_df$start[rows], mod_df$end[rows]), mod_df$post[rows])

    new.code = stringi::stri_sub_replace_all(code, from=list(mod_df$start[rows]), to = list(mod_df$end[rows]), value = value)

    new.code
  })
  return(code)
}

#' Computes for the specified rows in somo$pd the modified code
#' after replacements have been performed.
#'
#' WARNING: Works currently only for non-nested replacements
#'
#' @param somo A somo object
#' @param pd_rows An integer vector of rows of somo$pd for which the modified code shall be computed.
#' @returns A character vector with resulting code for each row in pd specified by pd_rows
#'
somo_make_pd_code = function(somo, pd_rows = seq_len(NROW(somo$pd))) {
  restore.point("somo_make_pd_code")
  pd = somo$pd[pd_rows,]
  pd$.ROW = pd_rows

  mod_df = somo_make_mod_df(somo)
  if (NROW(mod_df)==0) {
    return(pd$text)
  }

  library(data.table)
  # Find overlaps between pd and replacements
  dt_mod = mod_df %>%
    select(code_ind, repl_start= start, repl_end = end,
      mod_code = mod_code) %>%
    unique() %>%
    as.data.table()
  setkeyv(dt_mod, c("code_ind","repl_start","repl_end"))

  dt_pd = as.data.table(pd)
  setkeyv(dt_pd, c("code_ind","start","end"))

  ol_dt = foverlaps(dt_pd,dt_mod, mult="all", type="any", which=FALSE, nomatch=NA,by.x = c("code_ind","start","end"),by.y = c("code_ind","repl_start","repl_end"))

  ma_df = as_tibble(ol_dt) %>%
    mutate(
      rstart = repl_start-start+1,
      rend = rstart + repl_end - repl_start
    )

  # replace
  mod_df = ma_df %>% filter(!is.na(mod_code) & repl_start >= start & repl_end <= end) %>%
    group_by(id) %>%
    summarize(res_code = stringi::stri_sub_replace_all(first(text), from=rstart, to = rend, value = mod_code))

  pd = pd %>%
    left_join(mod_df, by="id") %>%
    mutate(res_code = ifelse(is.na(res_code), text, res_code))

  pd$res_code
}

#' Generate a data frame that contains information
#' for all code modifciations
somo_make_mod_df = function(somo) {
  restore.point("somo_make_mod_df")
  mod_df = bind_rows(somo$mod_li[1:somo$num_mod])
  if (NROW(mod_df)==0) return(NULL)
  mod_df$mod_code = rep("", NROW(mod_df))
  cid = 1
  codeinds = sort(unique(mod_df$code_ind))
  for (i in codeinds) {
    code = somo$code_df$code[i]
    rows = which(mod_df$code_ind==i)
    mod_df$mod_code[rows] = get.mod_df.value(mod_df[rows,], code)
  }
  mod_df
}

get.mod_df.value = function(mod_df, code, rows = seq_len(NROW(mod_df))) {
  pp = mod_df$type[rows]=="pp"
  num.pp = sum(pp)
  if (num.pp==0) return(mod_df$new[rows])

  value = rep("", length(rows))
  if (num.pp>0) {
    value[pp] = paste0(mod_df$pre[rows[pp]], substring(code, mod_df$start[rows[pp]], mod_df$end[rows[pp]]), mod_df$post[rows[pp]])
  }
  if (num.pp< length(rows)) {
    value[!pp] = mod_df$new[rows[!pp]]
  }
  value
}
