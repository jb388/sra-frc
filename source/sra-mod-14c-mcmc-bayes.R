# MCMC script for Bayesian parameter optimization
# aut: J. Beem-Miller
# date: 16-Aug-2022
  
# set date for saving files
date <- Sys.Date()

## Markov Chain Monte Carlo parameter optimization
# Note that the model uses prior variance distribution from 'modFit' optimization

# Model iterations
iter <- 1000 # start with 1000

# for saving script output
save.iter <- paste0(iter, "iter", ".RData")
save.dir <- file.path(paste0("~/sra-frc/data/derived/bayes-par-fit-", date))
if(!dir.exists(save.dir)) dir.create(file.path(paste0("~/sra-frc/data/derived/bayes-par-fit-", date)))  

## Create list of MCMC fits (delayed rejection option, no adaptation)
# Notes: uses modFit var and covariance matrix, e.g.
# var0 = mod.fit[[i]][["var_ms_unweighted"]] (allows resp and bulkC to be scaled differently)
# jump = pars.fit.mod.sum[[i]][["cov.unscaled"]] (3x3)
# same upper/lower limits as in modFit, same cost fx
ix.10 <- seq(1, 27, 3) # start with just 0-10 cm depth increment subset
bayes.fit.fx <- function(mod, pars.fit, In.fit, sub, upper, lower, 
                         var0, var0_name = "var_ms_unweighted", jump = NULL, burninlength = 0, 
                         iter = iter, updatecov = niter) {
  lapply(seq_along(pars.fit[sub]), function(i) {
  
    start <- Sys.time()
    cat(paste0(names(pars.fit[sub])[i], " parameter fitting\n"))
    
    # define jump
    if (!is.null(jump)) {
      jump <- jump[sub][[i]][["cov.unscaled"]]
    }
    
    # define cost fx for current iteration
    # 14C costs only for now...
    mod.Cost <- function(pars) {
      modelOutput <- modFun(pars, In = In.fit[sub][[i]], pass = TRUE, mod = mod)
      cost1 <- modCost(model = modelOutput, obs = obs.bulk.14c[sub][[i]], scaleVar = TRUE)
      return(modCost(model = modelOutput, obs = obs.resp.14c[sub][[i]], scaleVar = TRUE, cost = cost1))
    }
    
    # run MCMC
    fit <- tryCatch(
      modMCMC(f = mod.Cost, 
              p = pars.fit[sub][[i]], 
              var0 = var0[sub][[i]][[var0_name]],
              jump = jump,
              upper = upper, 
              lower = lower,
              niter = iter,
              ntrydr = 2,
              burninlength = burninlength,
              updatecov = updatecov),
      error = function (e) {cat("ERROR :", conditionMessage(e), "\n")})
    end <- Sys.time()
    cat(paste0("time: ", end - start, "\n"))
    return(fit)
  })
}

# MCMC notes:
# 'ntrydr' can improve poor acceptance rate by adjusting the delayed rejection parameter
# e.g. ntrydr = 2 means upon first rejection, the next parameter candidate is tried
# (ntrydr = 1 means no delayed rejection steps)

# 'dr_steps' denotes the number of delayed rejection steps;
# 'Alfasteps' is the number of times the algorithm has entered the acceptance function for delayed rejection


## Run function
# 2pp
bayes_fit_2pp_0_10 <- bayes.fit.fx(mod = "2pp",
                                   pars.fit = pars.fit.2pp,
                                   In.fit = in.1.ls,
                                   var0 = mod.fits.2pp,
                                   jump = pars.fit.2pp.sum,
                                   sub = ix.10,
                                   upper = c(1, 1, 1),
                                   lower = c(0, 0 ,0))
names(bayes_fit_2pp_0_10) <- names(mod.fits.2pp[ix.10])
# save output 
# *WARNING* will overwrite if file from current date exist!
save(bayes_fit_2pp_0_10, file = paste0(save.dir, "/bayes_fit_", "2pp_", "0-10_", save.iter))

# 2ps
bayes_fit_2ps_0_10 <- bayes.fit.fx(mod = "2ps",
                                   pars.fit = pars.fit.2ps,
                                   In.fit = in.1.ls,
                                   var0 = mod.fits.2ps,
                                   jump = pars.fit.2ps.sum,
                                   sub = ix.10,
                                   upper = c(1, 1, 1),
                                   lower = c(0, 0 ,0))
names(bayes_fit_2ps_0_10) <- names(mod.fits.2ps)[ix.10]
# save output
# *WARNING* will overwrite if file from current date exist!
save(bayes_fit_2ps_0_10, file = paste0(save.dir, "/bayes_fit_", "2ps_", "0-10_", save.iter))

### Estimate parameter sensitivity and return timeseries distribution envelope
## 2pp
# calculate sensitivity
pred_uncert_2pp <- lapply(seq_along(bayes_fit_2pp_0_10), function(i) {
  cat(paste0("Estimating ", names(bayes_fit_2pp_0_10)[i], " sensitivity\n"))
  pars <- bayes_fit_2pp_0_10[[i]][["pars"]]
  sensRange(func = modFun, parInput = pars, mod = "2pp", In = 1, sensvar = c("bulkC", "resp"))
})
names(pred_uncert_2pp) <- names(bayes_fit_2pp_0_10)
save(pred_uncert_2pp, 
     file = paste0("../data/derived/bayes-par-sens/", "pred_uncert_2pp", "_", Sys.Date(), ".RData"))

# summarize
sens_sum.2pp.ls <- lapply(seq_along(pred_uncert_2pp), function(i) {
  n <- nrow(Datm)
  mat <- pred_uncert_2pp[[i]][4:ncol(pred_uncert_2pp[[i]])]
  ls <- list(mat[ , 1:n], mat[ , (n + 1):(n + n)])
  ls.sum <- lapply(ls, function(x) {
    df <- data.frame(t(apply(x, 2, summary)[c(2, 4:5), ]))
    colnames(df) <- c("q05", "mean", "q95")
    pool <- ifelse(grepl("bulkC", rownames(df)[1]), "bulkC", "respiration")
    cbind(pool = pool, df)
  })
  rbind(
    data.frame(cbind(years = Datm$Date, mean = Datm$d14c, pool = "atm", q05 = NA, q95 = NA),
               stringsAsFactors = FALSE), 
    cbind(years = Datm$Date, bind_rows(ls.sum)))
})
names(sens_sum.2pp.ls) <- names(pred_uncert_2pp)

## 2ps
# calculate sensitivity
pred_uncert_2ps <- lapply(seq_along(bayes_fit_2ps_0_10), function(i) {
  cat(paste0("Estimating ", names(bayes_fit_2ps_0_10)[i], " sensitivity\n"))
  pars <- bayes_fit_2ps_0_10[[i]][["pars"]]
  sensRange(modFun, parInput = pars, mod = "2ps", In = 1, sensvar = c("bulkC", "resp"))
})
names(pred_uncert_2ps) <- names(bayes_fit_2ps_0_10)
save(pred_uncert_2ps, 
     file = paste0("../data/derived/bayes-par-sens/", "pred_uncert_2ps", "_", Sys.Date(), ".RData"))

# ## SA and TT uncertainty
# # Function to calculate system age, pool ages, and transit time for all bayesian parameter combinations
# sa.tt.fx <- function(mod, pars, input, iter) {
#   
#   # initialize list
#   ls.nms <- c("SA.ls", "TT.ls", "fast.age.ls", "slow.age.ls")
#   SA.TT.ls <- lapply(ls.nms, function(ls) {
#     ls <- vector(mode = "list", length = iter)
#   })
#   names(SA.TT.ls) <- ls.nms
#   
#   # set progress bar
#   pb <- txtProgressBar(min = 0, max = iter, style = 3)
#   
#   for (i in 1:iter) {
#     
#     # model matrix and inputs
#     A <- -1 * diag(pars[i, 1:2])
#     if (mod == "2ps") {
#       In <- c(input, 0)
#       A[2, 1] <- pars[i, 3]
#     } else {
#       In <- c(input * pars[i, 3], input * (1 - pars[i, 3]))
#     }
#     
#     # System ages and transit times
#     SA <- systemAge(A = A, u = In)
#     TT <- transitTime(A = A, u = In)
#     
#     # Append to list
#     SA.TT.ls[["SA.ls"]][[i]] <- as.numeric(SA$meanSystemAge)
#     SA.TT.ls[["TT.ls"]][[i]] <- as.numeric(TT$meanTransitTime)
#     SA.TT.ls[["fast.age.ls"]][[i]] <- as.numeric(SA$meanPoolAge[1])
#     SA.TT.ls[["slow.age.ls"]][[i]] <- as.numeric(SA$meanPoolAge[2])
#     
#     # tracker
#     setTxtProgressBar(pb, i)
#   }
#   return(SA.TT.ls)
# }
# 
# # note that the following function calls are very time consuming...
# # 2pp
# SA.TT.2pp.ls <- lapply(seq_along(bayes_fit_2pp_0_10), function(j) {
#   sa.tt.fx(mod = "2pp", bayes_fit_2pp_0_10[[j]], in.1.ls[[j]], iter)
# })
# # 2ps
# SA.TT.2ps.ls <- lapply(seq_along(bayes_fit_2ps_0_10), function(j) {
#   sa.tt.fx(mod = "2ps", bayes_fit_2ps_0_10[[j]][["par"]], in.1.ls[[j]], iter)
# })
# 
# # save output
# # *WARNING* will overwrite if files from current date exist!
# save(SA.TT.2pp.ls, file = paste0(save.dir, "/bayes_fit_SA_TT_", "2pp_", save.iter))
# save(SA.TT.2ps.ls, file = paste0(save.dir, "/bayes_fit_SA_TT_", "2ps_", save.iter))