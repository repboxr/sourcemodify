
debug.check.mod = function(somo) {
  mod_df = bind_rows(somo$mod_li[1:somo$num_mod]) %>%
    group_by(code_ind) %>%
    arrange(start) %>%
    mutate(
      end_cummax = cummax(end),
      is.nested = end < cummax(end),
      nest.level = 0
    ) %>%
    ungroup()
  temp = mod_df %>% filter(is.nested)

  i = 1
  somo$code_df$code[temp$code_ind[[i]]] %>% substring(temp$start[i]-10, temp$end[i]+10)

}
