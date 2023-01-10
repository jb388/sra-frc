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
# 1-Dec-2022
## - ran 2ps lag model MCMC, used burnin = 1000, updatecov = 50
## - bestfit pars look great, sensrange looks awful
## - going to rerun w/o burnin and updatecov
# 8-Dec-2022
## - running SA/TT fx for mcmc lists for non-lagged models

## Create list of MCMC fits (delayed rejection option, no adaptation)
# Notes: uses modFit var and covariance matrix, e.g.
# var0 = mod.fit[[i]][["var_ms_unweighted"]] (allows resp and bulkC to be scaled differently)
# jump = pars.fit.mod.sum[[i]][["cov.unscaled"]] (3x3)
# same upper/lower limits as in modFit, same cost fx

# Parallel implementation
#####
library(doParallel)
library(foreach)

# Detect number of cores on machine
UseCores <- detectCores()

# redefine bayes fit fx w/ parallel syntax
bayes.fit.prll.fx <- function(mod, pars.fit, In.fit, obs.bulk.14c, obs.resp.14c, upper, lower, ntrydr = 2,
                              var0, var0_name = "var_ms_unweighted", jump = NULL, burninlength = 0, 
                              niter, updatecov, fit_lag = FALSE, nms) {
  # start timer
  start <- Sys.time()
    
  # define jump
  if (!is.null(jump)) {
    jump <- jump[["cov.unscaled"]]
  }
  
  # define lambda (true half-life of 14C)
  lambda <- 1 / 8267
  
  # load data
  NHZone2 <- data.frame(readxl::read_excel(
    "/Users/jeff/sra-frc/data/external/Hua_2021/S0033822221000953sup002.xls", sheet = 2, skip = 5,
    col_names = c("Year.AD", "mean.Delta14C", "sd.Delta14C", "mean.F14C", "sd.F14C")))
  
  # forcast data through 2021 (2 additional years)
  yrs <- seq(2000, 2019.25, by = 1/4) # A series of years by quarters
  nz2 <- spline(NHZone2[ , c(1, 4)], xout = yrs) # quarterly spline interpolation of fm data
  nhz2 <- ts(nz2$y, start = 2000, freq = 4) # Transformation into a time-series object
  m <- forecast::ets(nhz2) # Fits an exponential smoothing state space model to the time series
  f2 <- forecast::forecast(m, h = 2.75 * 4) # Uses the fitted model to forecast 2 years into the future
  atm14c <- rbind(
    SoilR::bind.C14curves(SoilR::IntCal20, NHZone2, "AD"),
    data.frame(Year.AD = seq(tsp(f2$mean)[1], tsp(f2$mean)[2], by = 1/tsp(f2$mean)[3]),
               Delta14C = suppressWarnings(
                 ISRaD::convert_fm_d14c(
                   fm = as.numeric(f2$mean), obs_date_y = seq(2019.75, 2022, by = .25), verbose = FALSE)),
               Sigma = NA)
  )
  
  # filter to 1900-2022 and calc annual averages
  Datm <- data.frame(Date = seq(1900.5, 2021.5), d14c = NA)
  for (i in seq_along(Datm$Date)) {
    ix <- which(atm14c$Year.AD >= Datm[i, "Date"] & atm14c$Year.AD < Datm[i, "Date"] + 1)
    Datm[i, "d14c"] <- mean(atm14c[ix, "Delta14C"], na.rm = TRUE)
  }
  
  # define fxs
  fm <- function (k){
    k/(k + lambda)
  }
  soc.fx <- function(pars, mod, In, out = "pools", mod_mat = FALSE) {
    
    # steady-state stock calc fx
    calc.soc <- function(A, in_vector) {
      (-1 * solve(A) %*% in_vector)
    }
    
    # model matrices
    if (mod == "1p") {
      
      # 1p mod matrix
      A <- pars
      
      # 1p steady-state stocks
      in_vector <- In
      ss.cstock <- pars^-1 * in_vector
      
      # pool names
      pnms <- "bulkC"
      
    } else if (grepl("2p", mod)) {
      
      # pool names
      pnms <- c("fast", "slow")
      
      # 2pp mod matrix
      A <- -diag(pars[1:2])
      
      # 2pp steady-state C stocks
      in_vector <- c(In * pars[3], In * (1 - pars[3]))
      ss.cstock <- calc.soc(A, in_vector)
      
      if (mod == "2ps") {
        
        # 2ps mod matrix
        A[2, 1] <- pars[3] * pars[1]
        
        # 2ps steady-state C stocks
        in_vector <- c(In, 0)
        ss.cstock <- calc.soc(A, in_vector)
      }
    } else if (grepl("3p", mod)) {
      
      # pool names
      pnms <- c("fast", "slow", "passive")
      
      # 3pp mod matrix
      A <- -diag(pars[1:3])
      
      # 3pp steady-state C stocks
      in_vector <- c(In * pars[4], In * pars[5], In * (1 - pars[4] - pars[5]))
      ss.cstock <- calc.soc(A, in_vector)
      
      if (mod == "3ps") {
        
        # 3ps mod matrix
        A[2, 1] <- pars[4] * pars[1]
        A[3, 2] <- pars[5] * pars[2] 
        
        # 3ps steady-state C stocks
        in_vector <- c(In, 0, 0)
        ss.cstock <- calc.soc(A, in_vector)
      }
    }
    
    if (out == "sum") {
      soc <- sum(ss.cstock)
      if (mod_mat) {
        list(A_mat = A, in_vector = in_vector, ss_soc = soc, par_names = pnms)
      } else {
        soc
      }
    } else {
      soc <- ss.cstock
      if (mod_mat) {
        list(A_mat = A, in_vector = in_vector, ss_soc = soc, par_names = pnms)
      } else {
        soc
      }
    }
  }
  modFun <- function(pars, mod, In, lag = 0, pass = TRUE, PMeco_depth = NULL, fit_lag = FALSE) {
    
    # get lag, set pars
    if (fit_lag) {
      if (grepl("2", mod)) {
        PARS <- pars[1:3]
        lag <- pars[4]
      } else if (grepl("3", mod)) {
        PARS <- pars[1:5]
        lag <- pars[6]
      } else {
        # 1p
        PARS <- pars[1]
        lag <- pars[2]
      } 
    } else {
      PARS <- pars
      lag <- lag
    }
    
    # run soc.fx to get: mod_mat [[1]], in_vector [[2]], steady-state C [[3]], and pool names [[4]]
    soc.fx_out <- soc.fx(PARS, mod, In, mod_mat = TRUE)
    ss.cstock <- soc.fx_out[[3]]
    
    # check for negative stocks
    if (any(ss.cstock <= 0)) {
      cat("pool ", which(ss.cstock <= 0), "< 0\n")
    }
    
    # time index
    ix.t <- c((lag + 1):nrow(Datm))
    
    # check for 1p mod
    if (mod == "1p") {
      
      # model
      model <- SoilR::OnepModel14(
        t = Datm$Date[ix.t],
        k = soc.fx_out[[1]],
        C0 = as.vector(ss.cstock),
        F0_Delta14C = SoilR::Delta14C_from_AbsoluteFractionModern(fm(soc.fx_out[[1]])),
        In = soc.fx_out[[2]],
        inputFc = Datm,
        lag = lag,
        pass = pass)
      
    } else {
      
      # calculate initial 14C
      F0_Delta14C <- unlist(
        lapply(-diag(soc.fx_out[[1]]), function(x) SoilR::Delta14C_from_AbsoluteFractionModern(fm(x))))
      
      # multipool model fx
      mod.fx <- function(A,
                         t,
                         in_vector,
                         C0,
                         F0_Delta14C, 
                         xi = 1, # timestep
                         inputFc, 
                         lag = lag,
                         pass = pass) {
        t_start = min(t)
        t_stop = max(t)
        inputFluxes = SoilR::BoundInFluxes(function(t) {
          matrix(nrow = length(in_vector), ncol = 1, in_vector)
        }, t_start, t_stop)
        if (length(xi) == 1) 
          fX = function(t) {
            xi
          }
        At = SoilR::BoundLinDecompOp(map = function(t) {
          fX(t) * A
        }, t_start, t_stop)
        Fc = SoilR::BoundFc(inputFc, lag = lag, format = "Delta14C")
        mod = SoilR::Model_14(t, At, ivList = C0, initialValF = SoilR::ConstFc(F0_Delta14C, "Delta14C"), 
                       inputFluxes = inputFluxes, inputFc = Fc, pass = pass)
      }
      
      # run model
      model <- mod.fx(
        A = soc.fx_out[[1]],
        t = Datm$Date[ix.t],
        in_vector = soc.fx_out[[2]],
        C0 = as.vector(ss.cstock),
        F0_Delta14C = F0_Delta14C,
        inputFc = Datm,
        lag = lag,
        pass = pass) 
    }
    
    # get mod values
    C14m <- SoilR::getF14C(model)
    C14p <- SoilR::getF14(model) 
    C14r <- SoilR::getF14R(model)
    Ctot <- SoilR::getC(model)
    
    # dataframe for modFit fx
    data.frame(
      time = Datm$Date[ix.t],
      resp = C14r,
      bulkC = C14m,
      cStock = sum(Ctot[1, ]))
  }
  
  # define cost fx for current iteration 
  # only 14C costs for now...
  mod.Cost <- function(pars) {
    modelOutput <- modFun(pars, In = In.fit, pass = TRUE, mod = mod, fit_lag = fit_lag)
    cost1 <- FME::modCost(model = modelOutput, obs = obs.bulk.14c, scaleVar = TRUE)
    FME::modCost(model = modelOutput, obs = obs.resp.14c, scaleVar = TRUE, cost = cost1)
  }
  
  # run MCMC
  fit <- tryCatch(
    FME::modMCMC(f = mod.Cost,
            p = pars.fit, 
            var0 = var0[[var0_name]],
            jump = jump,
            upper = upper, 
            lower = lower,
            niter = niter,
            ntrydr = ntrydr,
            burninlength = burninlength,
            updatecov = updatecov),
    error = function (e) {cat("ERROR :", conditionMessage(e), "\n")})
  end <- Sys.time()
  cat(paste0(nms, " time: ", end - start, "\n"))
  return(fit)
}

# Register the cluster using n - 1 cores, send output to console (outfile = "")
outfile <- file.path(save.dir, "bayes_fit_2pp.lag_10000itr.txt")
cl <- makeCluster(UseCores - 1, outfile = outfile)

registerDoParallel(cl)

## 2pp, lag
nms <- names(pars.fit.2pp.lag)
# Use foreach loop and %dopar% to compute in parallel
bayes_fit_2pp.lag <- foreach(i = seq_along(pars.fit.2pp.lag), .packages = c("SoilR", "ISRaD", "FME")) %dopar%
  (bayes.fit.prll.fx(mod = "2pp",
                     pars.fit = pars.fit.2pp.lag[[i]],
                     In.fit = in.1.ls[[i]],
                     obs.bulk.14c = obs.bulk.14c[[i]],
                     obs.resp.14c = obs.resp.14c[[i]],
                     upper = c(1, 1, 1, 100),
                     lower = c(0, 0, 0, 0),
                     var0 = mod.fits.2pp.lag[[i]],
                     jump = pars.fit.2pp.lag.sum[[i]],
                     niter = 10000,
                     updatecov = 10000,
                     fit_lag = TRUE, 
                     nms = nms[i]))
names(bayes_fit_2pp.lag) <- names(pars.fit.2pp.lag)
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

#stop the cluster
stopCluster(cl)


## 2ps, lag
# Register the cluster using n - 1 cores, send output to console (outfile = "")
outfile <- file.path(save.dir, "bayes_fit_2ps.lag_10000itr.txt")
cl <- makeCluster(UseCores - 1, outfile = outfile)

registerDoParallel(cl)

nms <- names(pars.fit.2ps.lag)
# Use foreach loop and %dopar% to compute in parallel
bayes_fit_2ps.lag <- foreach(i = seq_along(pars.fit.2ps.lag), .packages = c("SoilR", "ISRaD", "FME")) %dopar%
  (bayes.fit.prll.fx(mod = "2ps",
                     pars.fit = pars.fit.2ps.lag[[i]],
                     In.fit = in.1.ls[[i]],
                     obs.bulk.14c = obs.bulk.14c[[i]],
                     obs.resp.14c = obs.resp.14c[[i]],
                     upper = c(1, 1, 1, 100),
                     lower = c(0, 0, 0, 0),
                     var0 = mod.fits.2ps.lag[[i]],
                     jump = pars.fit.2ps.lag.sum[[i]],
                     niter = 10000,
                     updatecov = 10000,
                     fit_lag = TRUE, 
                     nms = nms[i]))
names(bayes_fit_2ps.lag) <- names(pars.fit.2ps.lag)
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

#stop the cluster
stopCluster(cl)

## 3ps, lag
nms <- names(pars.fit.3ps.lag)
# Use foreach loop and %dopar% to compute in parallel
bayes_fit_2ps.lag <- foreach(i = seq_along(pars.fit.3ps.lag), .packages = c("SoilR", "ISRaD", "FME")) %dopar%
  (bayes.fit.prll.fx(mod = "2ps",
                     pars.fit = pars.fit.3ps.lag[[i]],
                     In.fit = in.1.ls[[i]],
                     obs.bulk.14c = obs.bulk.14c[[i]],
                     obs.resp.14c = obs.resp.14c[[i]],
                     upper = c(1, 1, 1, 1, 1, 100),
                     lower = c(0, 0, 0, 0, 0, 0),
                     var0 = mod.fits.2ps.lag[[i]],
                     jump = pars.fit.2ps.lag.sum[[i]],
                     niter = 10000,
                     updatecov = 10000,
                     fit_lag = TRUE, 
                     nms = nms[i]))
names(bayes_fit_3ps.lag) <- names(pars.fit.3ps.lag)
# save output 
# *WARNING* will overwrite if file from current date exist!
save(bayes_fit_3ps.lag, file = paste0(save.dir, "/bayes_fit_", "3ps.lag_", save.iter))


# calculate sensitivity
pred_uncert_3ps.lag <- lapply(seq_along(bayes_fit_3ps.lag), function(i) {
  cat(paste0("Estimating ", names(bayes_fit_3ps.lag)[i], " sensitivity\n"))
  pars <- bayes_fit_3ps.lag[[i]][["pars"]]
  sensRange(func = modFun, parInput = pars, mod = "3ps", In = 1, sensvar = c("bulkC", "resp"), fit_lag = TRUE, sensR = TRUE)
})
names(pred_uncert_3ps.lag) <- names(bayes_fit_3ps.lag)
save(pred_uncert_3ps.lag, 
     file = paste0(save.dir, "/pred_uncert_", "3ps_lag_", save.iter))


#stop the cluster
stopCluster(cl)

## SA and TT uncertainty
# Function to calculate system age, pool ages, and transit time for all bayesian parameter combinations
sa.tt.prll.fx2 <- function(mod, bayes_pars, s, dlen, soc.meas, res, fit_lag = FALSE) {
  
  # get par length
  if (grepl("1p", mod)) {
    par.len <- 1
  } else if (grepl("2p", mod)) {
    par.len <- 3
  } else {
    par.len <- 5
  }
  
  # determine iterations
  iter <- nrow(bayes_pars[["pars"]])
    
  # initialize list
  ls.nms <- c("sysAge", "transT", "ins")
  SA.TT.ls <- lapply(ls.nms, function(ls) {
    ls <- vector(mode = "list", length = s) 
  })
  names(SA.TT.ls) <- ls.nms
  
  if (fit_lag) {
    SA.TT.ls$lag <- vector(mode = "list", length = s)  
  }
  
  # sample index
  s_ind <- sample(iter, s)
  
  # define fxs
  soc.fx <- function(pars, mod, In, out = "pools", mod_mat = FALSE) {
    
    # steady-state stock calc fx
    calc.soc <- function(A, in_vector) {
      (-1 * solve(A) %*% in_vector)
    }
    
    # model matrices
    if (mod == "1p") {
      
      # 1p mod matrix
      A <- pars
      
      # 1p steady-state stocks
      in_vector <- In
      ss.cstock <- pars^-1 * in_vector
      
      # pool names
      pnms <- "bulkC"
      
    } else if (grepl("2p", mod)) {
      
      # pool names
      pnms <- c("fast", "slow")
      
      # 2pp mod matrix
      A <- -diag(pars[1:2])
      
      # 2pp steady-state C stocks
      in_vector <- c(In * pars[3], In * (1 - pars[3]))
      ss.cstock <- calc.soc(A, in_vector)
      
      if (mod == "2ps") {
        
        # 2ps mod matrix
        A[2, 1] <- pars[3] * pars[1]
        
        # 2ps steady-state C stocks
        in_vector <- c(In, 0)
        ss.cstock <- calc.soc(A, in_vector)
      }
    } else if (grepl("3p", mod)) {
      
      # pool names
      pnms <- c("fast", "slow", "passive")
      
      # 3pp mod matrix
      A <- -diag(pars[1:3])
      
      # 3pp steady-state C stocks
      in_vector <- c(In * pars[4], In * pars[5], In * (1 - pars[4] - pars[5]))
      ss.cstock <- calc.soc(A, in_vector)
      
      if (mod == "3ps") {
        
        # 3ps mod matrix
        A[2, 1] <- pars[4] * pars[1]
        A[3, 2] <- pars[5] * pars[2] 
        
        # 3ps steady-state C stocks
        in_vector <- c(In, 0, 0)
        ss.cstock <- calc.soc(A, in_vector)
      }
    }
    
    if (out == "sum") {
      soc <- sum(ss.cstock)
      if (mod_mat) {
        list(A_mat = A, in_vector = in_vector, ss_soc = soc, par_names = pnms)
      } else {
        soc
      }
    } else {
      soc <- ss.cstock
      if (mod_mat) {
        list(A_mat = A, in_vector = in_vector, ss_soc = soc, par_names = pnms)
      } else {
        soc
      }
    }
  }
    
  # run loop
  for (j in seq_along(s_ind)) {
    
    # get pars
    PARS <- bayes_pars[["pars"]][s_ind[j], 1:par.len]
    
    # run in fit fx
    SOC <- soc.meas[["lyr_soc"]]
    IN <- 1 
    
    if  (SOC < soc.fx(PARS, mod, IN, "sum")) { 
      
      # by step; floor set at .001
      byStep <- (IN - .001) / res 
    
      # in vector
      ins <- seq(.001, IN, byStep)
      
    } else {
    
      # by step; ceiling set at SOC
      byStep <- (SOC - IN) / res 
      
      # in vector
      ins <- seq(IN, 
                 SOC, 
                 byStep)
    }
    
    # modeled stocks
    soc_mod <- lapply(seq_along(ins), function(j) soc.fx(PARS, mod, ins[j], "sum"))
    IN.FIT <- round(ins[which.min(abs(unlist(soc_mod) - SOC))], 3)
    
    # run soc.fx
    soc.out <- soc.fx(pars = PARS, mod = mod, In = IN.FIT, mod_mat = TRUE)
    
    # calc ages & transit times
    if (mod == "1p") {
      SA.TT.ls$transT <- NULL
      SA.TT.ls$sysAge[[i]] <- soc.out$ss_soc / soc.out$in_vector
      SA.TT.ls$ins[[i]] <- IN.FIT
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
      SA.TT.ls[["sysAge"]][[j]] <- sa
      SA.TT.ls[["transT"]][[j]] <- tt 
      SA.TT.ls[["ins"]][[j]] <- IN.FIT
    }
    if (fit_lag) {
      SA.TT.ls$lag[[j]] <- bayes_pars[["pars"]][s_ind[j], par.len + 1]
    }
  }
  return(SA.TT.ls)
}

## 2pp
# Register the cluster using n - 1 cores, set outfile
outfile <- file.path(save.dir, "sa.tt_2pp_10000itr.txt")
cl <- makeCluster(UseCores - 1, outfile = outfile)
registerDoParallel(cl)

# calc SA/TT
SA.TT.2pp.ls <- foreach(i = seq_along(pars.fit.2pp.mcmc), .packages = c("SoilR")) %dopar%
  (sa.tt.prll.fx2(
    mod = "2pp", 
    bayes_pars = bayes_fit_2pp[[i]], 
    s = 200, 
    dlen = 500,
    soc.meas = csoc.19.0_30[[i]],
    res = 500,
    fit_lag = FALSE))
names(SA.TT.2pp.ls) <- names(pars.fit.2pp)
save(SA.TT.2pp.ls, file = paste0(save.dir, "/bayes_fit_SA_TT_", "2pp_", save.iter))

# stop cluster
stopCluster(cl)


# 2pp, lag
# Register the cluster using n - 1 cores, set outfile
outfile <- file.path(save.dir, "sa.tt_2pp.lag_1000itr.txt")
cl <- makeCluster(UseCores - 1, outfile = outfile)
registerDoParallel(cl)

SA.TT.2pp.lag.ls <- foreach(i = seq_along(pars.fit.2pp.lag.mcmc), .packages = c("SoilR")) %dopar%
  (sa.tt.prll.fx2(
    mod = "2pp", 
    bayes_pars = bayes_fit_2pp.lag[[i]], 
    s = 200, 
    dlen = 500,
    soc.meas = csoc.19.0_30[[i]],
    res = 500,
    fit_lag = TRUE))
names(SA.TT.2pp.lag.ls) <- names(pars.fit.2pp)
save(SA.TT.2pp.lag.ls, file = paste0(save.dir, "/bayes_fit_SA_TT_", "2pp.lag_", save.iter))

#stop the cluster
stopCluster(cl)


# Register the cluster using n - 1 cores, set outfile
outfile <- file.path(save.dir, "sa.tt_2ps_1000itr.txt")
cl <- makeCluster(UseCores - 1, outfile = outfile)
registerDoParallel(cl)

# calc SA/TT
SA.TT.2ps.ls <- foreach(i = seq_along(pars.fit.2ps.mcmc), .packages = c("SoilR")) %dopar%
  (sa.tt.prll.fx(
    mod = "2ps", 
    bayes_pars = bayes_fit_2ps[[i]], 
    in.fit = in.fit.2ps.mcmc[[i]], 
    s = 200, 
    dlen = 500, 
    fit_lag = FALSE))
names(SA.TT.2ps.ls) <- names(pars.fit.2ps)
save(SA.TT.2ps.ls, file = paste0(save.dir, "/bayes_fit_SA_TT_", "2ps_", save.iter))

# stop cluster
stopCluster(cl)


## 2ps, lag
# Register the cluster using n - 1 cores, send output to console (outfile = "")
outfile <- file.path(save.dir, "sa.tt_2ps.lag_10000itr.txt")
cl <- makeCluster(UseCores - 1, outfile = outfile)

# 2ps, lag
SA.TT.2ps.lag.ls <- foreach(i = seq_along(pars.fit.2ps.lag.mcmc), .packages = c("SoilR")) %dopar%
  (sa.tt.prll.fx2(
    mod = "2ps", 
    bayes_pars = bayes_fit_2ps.lag[[i]], 
    s = 200, 
    dlen = 500,
    soc.meas = csoc.19.0_30[[i]],
    res = 500,
    fit_lag = TRUE))
names(SA.TT.2ps.lag.ls) <- names(pars.fit.2ps)
save(SA.TT.2ps.lag.ls, file = paste0(save.dir, "/bayes_fit_SA_TT_", "2ps.lag_", save.iter))

#stop the cluster
stopCluster(cl)
