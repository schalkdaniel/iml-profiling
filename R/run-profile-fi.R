library(mlr)
library(iml)
library(foreach)

source("setup.R")

data("Boston", package = "MASS")
data_raw = Boston[,c(1:9, which(colnames(Boston) == "medv"))]

cores = parallel::detectCores() - 1
cl = parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)


foreach::foreach(k = seq_len(nrow(experiments)), .packages = c("mlr", "iml", "foreach")) %dopar% {

  ## Setup:
  n     = experiments$n[k]
  p     = experiments$p[k]
  l     = experiments$learner[k]
  batch = experiments$batch[k]
  i     = experiments$rep[k]

  p_data = try(expr = {
    ## Sample as much rows as we want to have:
    idx = sample(x = seq_len(nrow(Boston)), size = n, replace = TRUE)

    if (p == 10L) {
      dat = data_raw[idx,]
    } else {
      dat = cbind(data_raw[idx,], as.data.frame(matrix(rnorm((p - 10L) * n), ncol = (p - 10L)))) #
    }

    task = makeRegrTask(data = dat, target = "medv")

    if (l == "regr.lm") {
      lrn = makeLearner(l)
    }
    if (l == "regr.randomForest") {
      lrn = makeLearner(l)
    }
    if (l == "regr.xgboost") {
      lrn = makeLearner(l)
    }
    mod = mlr::train(lrn, task)

    X = dat[which(names(dat) != "medv")]
    model = iml::Predictor$new(mod, data = X, y = dat$medv, batch)

    tmp_prof_filename = paste0("tmp-prof", k, ".out")
    Rprof(tmp_prof_filename, memory.profiling = TRUE)
    FeatureImp$new(model,loss="mse")
    Rprof(NULL)

    a = summaryRprof(tmp_prof_filename, memory = "both")$by.total
    file.remove(tmp_prof_filename)
    a$mem.diff = c(-diff(a$mem.total), 0)
    a$time.diff = c(-diff(a$total.time), 0)

    a
  }, silent = TRUE)

  hash = NULL
  if (class(p_data) == "try-error") {
    status = paste0(">> ERROR: ", attr(p_data, "condition")$message)
  } else {
    status = ">> DONE :-)"
    hash = experiments$hash[k]
    p_data$hash = hash
    save(p_data, file = paste0("profile-data-fi/", hash, ".Rda"))
  }
  msg_trace = paste0(as.character(Sys.time()), " ", k, "/", nrow(experiments), ": n=", n, " p=", p, " learner=", l, " batch_size=", batch, " rep=", i, "\t", status)

  log_file = "log.txt"
  if (file.exists(log_file)) {
    temp = readLines(log_file)
    temp[k] = msg_trace
    writeLines(temp, log_file)
  } else {
    file.create(log_file)
  }
  status
}

parallel::stopCluster(cl)

