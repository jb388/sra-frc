# MCMC script for Bayesian parameter optimization
# aut: J. Beem-Miller
# date: 17-Nov-2022
  
# set date for saving files
date <- Sys.Date()

## Markov Chain Monte Carlo parameter optimization
# Note that the model uses prior variance distribution from 'modFit' optimization

# Model iterations
iter <- 10000 # start with 1,000; then 10,000... (good acceptance, therefore no need for adaptive/DR)

# for saving script output
save.iter <- paste0(iter, "iter", ".RData")
save.dir <- file.path(paste0("~/sra-frc/data/derived/bayes-par-fit-", date))
if(!dir.exists(save.dir)) dir.create(file.path(paste0("~/sra-frc/data/derived/bayes-par-fit-", date)))  

# Notes:
# 29-Nov-2022
## - removed BSwf 2009 bulk 14c data in case it helps with fits
## - GRrf 10-20, 20-30 have outliers removed

## Create list of MCMC fits (delayed rejection option, no adaptation)
# Notes: uses modFit var and covariance matrix, e.g.
# var0 = mod.fit[[i]][["var_ms_unweighted"]] (allows resp and bulkC to be scaled differently)
# jump = pars.fit.mod.sum[[i]][["cov.unscaled"]] (3x3)
# same upper/lower limits as in modFit, same cost fx
bayes.fit.fx <- function(mod, pars.fit, In.fit, sub, upper, lower, 
                         var0, var0_name = "var_ms_unweighted", jump = NULL, burninlength = 0, 
                         niter = iter, updatecov = iter, fit_lag = FALSE) {
  lapply(seq_along(pars.fit[sub]), function(i) {
  
    start <- Sys.time()
    cat(paste0(names(pars.fit[sub])[i], " parameter fitting\n"))
    
    # define jump
    if (!is.null(jump)) {
      jump <- jump[sub][[i]][["cov.unscaled"]]
    }
    
    # define cost fx for current iteration 
    # only 14C costs for now...
    mod.Cost <- function(pars) {
      modelOutput <- modFun(pars, In = In.fit[sub][[i]], pass = TRUE, mod = mod, fit_lag = fit_lag)
      cost1 <- modCost(model = modelOutput, obs = obs.bulk.14c[sub][[i]], scaleVar = TRUE)
      modCost(model = modelOutput, obs = obs.resp.14c[sub][[i]], scaleVar = TRUE, cost = cost1)
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
              updatecov = iter),
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
# default here is 2, i.e. delayed rejection toggeled ON

# 'dr_steps' denotes the number of delayed rejection steps;
# 'Alfasteps' is the number of times the algorithm has entered the acceptance function for delayed rejection


## Run function
# 2pp
bayes_fit_2pp <- bayes.fit.fx(mod = "2pp",
                              pars.fit = pars.fit.2pp,
                              In.fit = in.1.ls,
                              var0 = mod.fits.2pp,
                              jump = pars.fit.2pp.sum,
                              sub = 1:27,
                              upper = c(1, 1, 1),
                              lower = c(0, 0 ,0))
names(bayes_fit_2pp) <- names(mod.fits.2pp)
# save output 
# *WARNING* will overwrite if file from current date exist!
save(bayes_fit_2pp, file = paste0(save.dir, "/bayes_fit_", "2pp_", save.iter))


# 2ps
bayes_fit_2ps <- bayes.fit.fx(mod = "2ps",
                              pars.fit = pars.fit.2ps,
                              In.fit = in.1.ls,
                              var0 = mod.fits.2ps,
                              jump = pars.fit.2ps.sum,
                              sub = 1:27,
                              upper = c(1, 1, 1),
                              lower = c(0, 0 ,0))
names(bayes_fit_2ps) <- names(mod.fits.2ps)
# save output
# *WARNING* will overwrite if file from current date exist!
save(bayes_fit_2ps, file = paste0(save.dir, "/bayes_fit_", "2ps_", save.iter))

### Estimate parameter sensitivity and return timeseries distribution envelope
## 2pp
# calculate sensitivity
pred_uncert_2pp <- lapply(seq_along(bayes_fit_2pp), function(i) {
  cat(paste0("Estimating ", names(bayes_fit_2pp)[i], " sensitivity\n"))
  pars <- bayes_fit_2pp[[i]][["pars"]]
  sensRange(func = modFun, parInput = pars, mod = "2pp", In = 1, sensvar = c("bulkC", "resp"))
})
names(pred_uncert_2pp) <- names(bayes_fit_2pp)
save(pred_uncert_2pp, 
     file = paste0(save.dir, "/pred_uncert_", "2pp_", save.iter))

# # summarize
# sens_sum.2pp.ls <- lapply(seq_along(pred_uncert_2pp), function(i) {
#   n <- nrow(Datm)
#   mat <- pred_uncert_2pp[[i]][4:ncol(pred_uncert_2pp[[i]])]
#   ls <- list(mat[ , 1:n], mat[ , (n + 1):(n + n)])
#   ls.sum <- lapply(ls, function(x) {
#     df <- data.frame(t(apply(x, 2, summary)[c(2, 4:5), ]))
#     colnames(df) <- c("q05", "mean", "q95")
#     pool <- ifelse(grepl("bulkC", rownames(df)[1]), "bulkC", "respiration")
#     cbind(pool = pool, df)
#   })
#   rbind(
#     data.frame(cbind(years = Datm$Date, mean = Datm$d14c, pool = "atm", q05 = NA, q95 = NA),
#                stringsAsFactors = FALSE), 
#     cbind(years = Datm$Date, bind_rows(ls.sum)))
# })
# names(sens_sum.2pp.ls) <- names(pred_uncert_2pp)

## 2ps
# calculate sensitivity
pred_uncert_2ps <- lapply(seq_along(bayes_fit_2ps), function(i) {
  cat(paste0("Estimating ", names(bayes_fit_2ps)[i], " sensitivity\n"))
  pars <- bayes_fit_2ps[[i]][["pars"]]
  sensRange(modFun, parInput = pars, mod = "2ps", In = 1, sensvar = c("bulkC", "resp"))
})
names(pred_uncert_2ps) <- names(bayes_fit_2ps)
save(pred_uncert_2ps, 
     file = paste0(save.dir, "/pred_uncert_", "2ps_", save.iter))

## lagged models
# 1p w/ lag
bayes_fit_1p.lag <- bayes.fit.fx(mod = "1p",
                                 pars.fit = pars.fit.1p.lag,
                                 In.fit = in.1.ls,
                                 var0 = mod.fits.1p.lag,
                                 jump = pars.fit.1p.lag.sum,
                                 sub = 1:27,
                                 upper = c(1, 100),
                                 lower = c(0, 0),
                                 fit_lag = TRUE)
names(bayes_fit_1p.lag) <- names(mod.fits.1p.lag)
# save output 
# *WARNING* will overwrite if file from current date exist!
save(bayes_fit_1p.lag, file = paste0(save.dir, "/bayes_fit_", "1p.lag_", save.iter))

# calculate sensitivity
pred_uncert_1p.lag <- lapply(seq_along(bayes_fit_1p.lag), function(i) {
  cat(paste0("Estimating ", names(bayes_fit_1p.lag)[i], " sensitivity\n"))
  pars <- bayes_fit_1p.lag[[i]][["pars"]]
  sensRange(func = modFun, parInput = pars, mod = "1p", In = 1, sensvar = c("bulkC", "resp"), fit_lag = TRUE, sensR = TRUE)
})
names(pred_uncert_1p.lag) <- names(bayes_fit_1p.lag)
save(pred_uncert_1p.lag, 
     file = paste0(save.dir, "/pred_uncert_", "1p_lag_", save.iter))


# 2pp w/ lag
bayes_fit_2pp.lag <- bayes.fit.fx(mod = "2pp",
                                  pars.fit = pars.fit.2pp.lag,
                                  In.fit = in.1.ls,
                                  var0 = mod.fits.2pp.lag,
                                  jump = pars.fit.2pp.lag.sum,
                                  sub = 1:27,
                                  upper = c(1, 1, 1, 100),
                                  lower = c(0, 0, 0, 0),
                                  fit_lag = TRUE)
names(bayes_fit_2pp.lag) <- names(mod.fits.2pp.lag)
# save output 
# *WARNING* will overwrite if file from current date exist!
save(bayes_fit_2pp.lag, file = paste0(save.dir, "/bayes_fit_", "2pp.lag_", save.iter))

# calculate sensitivity
pred_uncert_2pp.lag <- lapply(seq_along(bayes_fit_2pp.lag), function(i) {
  cat(paste0("Estimating ", names(bayes_fit_2pp.lag)[i], " sensitivity\n"))
  pars <- bayes_fit_2pp.lag[[i]][["pars"]]
  sensRange(func = modFun, parInput = pars, mod = "2pp", In = 1, sensvar = c("bulkC", "resp"), fit_lag = TRUE, sensR = TRUE)
})
names(pred_uncert_2pp.lag) <- names(bayes_fit_2pp.lag)
save(pred_uncert_2pp.lag, 
     file = paste0(save.dir, "/pred_uncert_", "2pp_lag_", save.iter))

# # summarize
# sens_sum.2pp.lag.ls <- lapply(seq_along(pred_uncert_2pp.lag), function(i) {
#   n <- nrow(Datm)
#   mat <- pred_uncert_2pp.lag[[i]][4:ncol(pred_uncert_2pp.lag[[i]])]
#   ls <- list(mat[ , 1:n], mat[ , (n + 1):(n + n)])
#   ls.sum <- lapply(ls, function(x) {
#     df <- data.frame(t(apply(x, 2, summary)[c(2, 4:5), ]))
#     colnames(df) <- c("q05", "mean", "q95")
#     pool <- ifelse(grepl("bulkC", rownames(df)[1]), "bulkC", "respiration")
#     cbind(pool = pool, df)
#   })
#   rbind(
#     data.frame(cbind(years = Datm$Date, mean = Datm$d14c, pool = "atm", q05 = NA, q95 = NA),
#                stringsAsFactors = FALSE), 
#     cbind(years = Datm$Date, bind_rows(ls.sum)))
# })
# names(sens_sum.2pp.lag.ls) <- names(pred_uncert_2pp.lag)

## 2ps w/ lag
bayes_fit_2ps.lag <- bayes.fit.fx(mod = "2ps",
                                  pars.fit = pars.fit.2ps.lag,
                                  In.fit = in.1.ls,
                                  var0 = mod.fits.2ps.lag,
                                  jump = pars.fit.2ps.lag.sum,
                                  sub = 1:27,
                                  updatecov = 50, # adaptive Metropolix
                                  burninlength = 1000,
                                  upper = c(1, 1, 1, 100),
                                  lower = c(0, 0, 0, 0),
                                  fit_lag = TRUE)
names(bayes_fit_2ps.lag) <- names(mod.fits.2ps.lag)
# save output 
# *WARNING* will overwrite if file from current date exist!
save(bayes_fit_2ps.lag, file = paste0(save.dir, "/bayes_fit_", "2ps.lag_", save.iter))

# calculate sensitivity
pred_uncert_2ps.lag <- lapply(seq_along(bayes_fit_2ps.lag), function(i) {
  cat(paste0("Estimating ", names(bayes_fit_2ps.lag)[i], " sensitivity\n"))
  pars <- bayes_fit_2ps.lag[[i]][["pars"]]
  sensRange(func = modFun, parInput = pars, mod = "2ps", In = 1, sensvar = c("bulkC", "resp"), fit_lag = TRUE, sensR = TRUE)
})
names(pred_uncert_2ps.lag) <- names(bayes_fit_2ps.lag)
save(pred_uncert_2ps.lag, 
     file = paste0(save.dir, "/pred_uncert_", "2ps_lag_", save.iter))

# # summarize
# sens_sum.2ps.lag.ls <- lapply(seq_along(pred_uncert_2ps.lag), function(i) {
#   n <- nrow(Datm)
#   mat <- pred_uncert_2ps.lag[[i]][4:ncol(pred_uncert_2ps.lag[[i]])]
#   ls <- list(mat[ , 1:n], mat[ , (n + 1):(n + n)])
#   ls.sum <- lapply(ls, function(x) {
#     df <- data.frame(t(apply(x, 2, summary)[c(2, 4:5), ]))
#     colnames(df) <- c("q05", "mean", "q95")
#     pool <- ifelse(grepl("bulkC", rownames(df)[1]), "bulkC", "respiration")
#     cbind(pool = pool, df)
#   })
#   rbind(
#     data.frame(cbind(years = Datm$Date, mean = Datm$d14c, pool = "atm", q05 = NA, q95 = NA),
#                stringsAsFactors = FALSE), 
#     cbind(years = Datm$Date, bind_rows(ls.sum)))
# })
# names(sens_sum.2ps.lag.ls) <- names(pred_uncert_2ps.lag)

## SA and TT uncertainty
# Function to calculate system age, pool ages, and transit time for all bayesian parameter combinations
sa.tt.fx <- function(mod, par.ls, in.fit.ls, s, dlen, fit_lag = FALSE) {
  
  # get par length
  if (grepl("1p", mod)) {
    par.len <- 1
  } else if (grepl("2p", mod)) {
    par.len <- 3
  } else {
    par.len <- 5
  }
  
  # determine iterations
  iter <- nrow(par.ls[[1]][["pars"]])
  
  # start loop
  sa.tt.site.ls <- lapply(seq_along(par.ls), function(j) {
    
    cat(paste0(names(par.ls[j]), "\n"))
    
    # initialize list
    ls.nms <- c("sysAge", "transT")
    SA.TT.ls <- lapply(ls.nms, function(ls) {
      ls <- vector(mode = "list", length = s) 
    })
    names(SA.TT.ls) <- ls.nms
    
    if (fit_lag) {
      SA.TT.ls$lag <- vector(mode = "list", length = s)  
    }
    
    # get input
    IN <- in.fit.ls[[j]]
    
    # sample index
    s_ind <- sample(iter, s)
    
    # set progress bar
    pb <- txtProgressBar(min = 0, max = s, style = 3)
    
    for (i in seq_along(s_ind)) {
      
      setTxtProgressBar(pb, i)
      # cat(paste(s_ind[i], "\n"))
      
      # get pars
      PARS <- par.ls[[j]][["pars"]][s_ind[i], 1:par.len]
      
      # run soc.fx
      soc.out <- soc.fx(pars = PARS, mod = mod, In = IN, mod_mat = TRUE)
      
      # calc ages & transit times
      if (mod == "1p") {
        SA.TT.ls$transT <- NULL
        SA.TT.ls$sysAge[[i]] <- soc.out$ss_soc / soc.out$in_vector
      } else {
        # set index for distibutions
        a <- seq(1, dlen)
        sa <- tryCatch(
          systemAge(A = soc.out$A_mat, u = soc.out$in_vector, a = a),
          error = function (e) {cat("ERROR :", conditionMessage(e), "\n")})
        tt <- tryCatch(
          transitTime(soc.out$A_mat, u = soc.out$in_vector),
          error = function (e) {cat("ERROR :", conditionMessage(e), "\n")})
        
        # Append to list
        SA.TT.ls[["sysAge"]][[i]] <- sa
        SA.TT.ls[["transT"]][[i]] <- tt 
      }
      if (fit_lag) {
        SA.TT.ls$lag[[i]] <- par.ls[[j]][["pars"]][s_ind[i], par.len + 1]
      }
    }
    
    # linebreak    
    cat("\n")
    
    return(SA.TT.ls)
  })
  names(sa.tt.site.ls) <- names(par.ls)
  return(sa.tt.site.ls)
}

# 1p, w/ lag
SA.TT.1p.lag.ls <- sa.tt.fx("1p", bayes_fit_1p.lag, in.fit.1p.lag.mcmc, s = 200, dlen = 500, fit_lag = TRUE)
save(SA.TT.1p.lag.ls, file = paste0(save.dir, "/bayes_fit_SA_TT_", "1p.lag_", save.iter))

# note that the following function calls are very time consuming...
# 2pp, w/ lg
SA.TT.2pp.lag.ls <- sa.tt.fx("2pp", bayes_fit_2pp.lag, in.fit.2pp.lag.mcmc, s = 200, dlen = 500, fit_lag = TRUE)
# save output
# *WARNING* will overwrite if files from current date exist!
save(SA.TT.2pp.lag.ls, file = paste0(save.dir, "/bayes_fit_SA_TT_", "2pp.lag_", save.iter))

# 2ps, w/ lag
SA.TT.2ps.lag.ls <- sa.tt.fx("2ps", bayes_fit_2ps.lag, in.fit.2ps.lag.mcmc, s = 200, dlen = 500, fit_lag = TRUE)
# save output
# *WARNING* will overwrite if files from current date exist!
save(SA.TT.2ps.lag.ls, file = paste0(save.dir, "/bayes_fit_SA_TT_", "2ps.lag_", save.iter))