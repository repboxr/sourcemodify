#
# surround.one.fun.arg = function(code,fun, pkg=NULL, arg, pre, post, first.arg=FALSE) {
#   restore.point("surround.one.fun.arg")
#   ca = parse(text=code)[[1]]
#   if (is.null(names(ca)) & first.arg) {
#     ind = 2
#     ma = ca
#   } else {
#     fun.object = get(fun)
#     ma = match.call(fun.object, ca)
#     na = names(ma)
#     ind = which(na==arg)
#   }
#
#   arg_code = deparse1(ma[[ind]])
#   new.arg_code = paste0(pre,arg_code, post)
#   arg_call = parse(text=new.arg_code)[[1]]
#   ma[[ind]] = arg_call
#   deparse1(ma)
# }
#
# surround.fun.args = function(code, fun, pre, post, arg, first.arg=FALSE, pd=get.fun.calls.parse.data(code=code)) {
#   restore.point("surround.fun.args")
#   rows = which(pd$fun == fun)
#   if (length(rows)==0) {
#     return(NULL)
#   }
#   new.txt = sapply(rows, function(row) {
#     surround.one.fun.arg(pd$code[row],fun=fun,pre=pre, post=post, arg=arg, first.arg=first.arg )
#   })
#   tibble(start=pd$start[rows], end = pd$end[rows], type="n", new=new.txt)
# }
