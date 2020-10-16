ns         = c(100, 1000, 5000, 10000, 50000)
ps         = c(10, 20, 30, 40, 50)
batch_size = 10^seq_len(5L)
learners   = c("regr.lm", "regr.randomForest", "regr.xgboost")
rep        = seq_len(10L)

experiments = expand.grid(n = ns, p = ps, learner = learners, batch = batch_size, rep = rep, stringsAsFactors = FALSE)
experiments$hash = apply(X = experiments, MARGIN = 1L,  FUN = function (x) digest::sha1(x))


