library(dplyr)
load("experiments.Rda")

file_names = unlist(strsplit(list.files("profile-data"), split = ".Rda"))
experiments_done = experiments[experiments$hash %in% file_names,]

appendChanges = function (data, call_names = NULL, add_relative = FALSE, data_dir = "profile-data")
{
  if (is.null(call_names[1])) stop("Please specify names of the calls")
  ll = lapply(data$hash, FUN = function (h) {
    file = paste0(data_dir, "/", h,".Rda")
    load(file)
    mem_change = p_data[rownames(p_data) %in% call_names, "mem.diff"]
    time_change = p_data[rownames(p_data) %in% call_names, "time.diff"]

    mem_total = rep(max(p_data$mem.total), times = length(call_names))
    time_total = rep(max(p_data$total.time), times = length(call_names))

    if (add_relative) {
      mem_change_rel = p_data[rownames(p_data) %in% call_names, "mem.diff"]/max(p_data$mem.total)
      time_change_rel = p_data[rownames(p_data) %in% call_names, "time.diff"]/max(p_data$total.time)
    }
    out = data.frame(hash = h, call = call_names, mem_change = mem_change, time_change = time_change, mem_total = mem_total, time_total = time_total)

    if (add_relative) {
      out$mem_change_rel = mem_change_rel
      out$time_change_rel = time_change_rel
    }
    return (out)
  })
  df_changes = do.call(rbind, ll)
  return (merge(data, df_changes, by = "hash"))
}

bm = experiments_done %>% appendChanges(call_names = c("\"private$run.ale\"", "\"calculate.ale.num\""), add_relative = TRUE)

