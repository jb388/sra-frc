# generic function for running SoilR models (1p, 2pp, 2ps, 3pp, 3ps) on lists of parameters and inputs

# mod fun
modFun <- function(pars, mod, In, lag = 0, pass = TRUE, verbose = TRUE, out = "modFit", PMeco_depth = NULL) {
  
  # model matrix
  if (grepl("2p", mod)) {
    A <- -diag(pars[1:2])
    pnms <- c("fast", "slow")
    if (mod == "2pp") {
      in_vector <- c(In * pars[3], In * (1 - pars[3]))
    } else {
      A[2, 1] <- pars[3]
      in_vector <- c(In, 0)  
    }
  } else if (grepl("3p", mod)) {
    A <- -diag(pars[1:3])
    pnms <- c("fast", "slow", "passive")
    if (mod == "3pp") {
      in_vector <- c(In * pars[4], In * pars[5], In * (1 - pars[4] - pars[5]))
    } else {
      A[2, 1] <- pars[4] * pars[1]
      A[3, 2] <- pars[5] * pars[2] 
      in_vector <- c(In, 0, 0)
    }
  }
  
  # calculate steady-state stocks and initial 14C
  if (mod == "1p") {
    in_vector <- In
    ss.cstock <- pars^-1 * in_vector
    F0_Delta14C <- Delta14C_from_AbsoluteFractionModern(fm(pars))
    pnms <- "bulkC"
  } else {
    ss.cstock <- (-1 * solve(A) %*% in_vector)
    F0_Delta14C <- unlist(lapply(-diag(A), function(x) Delta14C_from_AbsoluteFractionModern(fm(x))))
  }
  
  # check for negative stocks
  if (any(ss.cstock <= 0)) {
    cat("pool ", which(ss.cstock <= 0), "< 0")
  }
  
  # time index
  ix.t <- c((lag + 1):nrow(Datm))
  
  # model
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
    inputFluxes = BoundInFluxes(function(t) {
      matrix(nrow = length(in_vector), ncol = 1, in_vector)
    }, t_start, t_stop)
    if (length(xi) == 1) 
      fX = function(t) {
        xi
      }
    At = BoundLinDecompOp(map = function(t) {
      fX(t) * A
    }, t_start, t_stop)
    Fc = BoundFc(inputFc, lag = lag, format = "Delta14C")
    mod = Model_14(t, At, ivList = C0, initialValF = ConstFc(F0_Delta14C, "Delta14C"), 
                   inputFluxes = inputFluxes, inputFc = Fc, pass = pass)
  }
  
  # run mod
  if (mod == "1p") {
    model <- OnepModel14(
      t = Datm$Date[ix.t],
      k = as.vector(pars),
      C0 = as.vector(ss.cstock),
      F0_Delta14C = F0_Delta14C,
      In = in_vector,
      inputFc = Datm,
      lag = lag,
      pass = pass)
  } else {
    model <- mod.fx(
      A = A,
      t = Datm$Date[ix.t],
      in_vector = in_vector,
      C0 = as.vector(ss.cstock),
      F0_Delta14C = F0_Delta14C,
      inputFc = Datm,
      lag = lag,
      pass = pass) 
  }
  
  # get mod values
  C14m <- getF14C(model)
  C14p <- getF14(model) 
  C14r <- getF14R(model)
  Ctot <- getC(model)
  
  if (out == "modFit") {
    # dataframe for modFit fx
    data.frame(
      time = Datm$Date[ix.t],
      resp = C14r,
      bulkC = rowSums(Ctot[1, ] / sum(Ctot[1, ]) * C14p),
      cStock = sum(Ctot[1, ]))
  } else {
    
    # sum c stocks
    ss.cstock <- round(ss.cstock, 2)
    cstock.sum <- ifelse(is.null(dim(ss.cstock)), ss.cstock, colSums(ss.cstock))
    
    if (verbose) {
      # print site and steady-state stocks
      if (!is.null(PMeco_depth)) cat(paste0(PMeco_depth, "\n"))
      for (i in seq_along(ss.cstock)) {
        cat(paste("pool", i, ss.cstock[i], "\n"))
      }
      cat(cstock.sum, " (modeled total C stock)\n")
      if (!is.null(PMeco_depth)) {
        cat(round(csoc.19.0_30[[PMeco_depth]][ , "lyr_soc"], 1), " (measured total C stock)\n")
        if (cstock.sum < csoc.19.0_30[[PMeco_depth]][ , "lyr_soc_lwr"]) cat("Modeled stocks too low w/ current inputs\n") else
          if (cstock.sum > csoc.19.0_30[[PMeco_depth]][ , "lyr_soc_upr"]) cat("Modeled stocks too high w/ current inputs\n")
      }
    }
    
    # data frame for plotting
    data.frame(
      years = rep(Datm$Date[ix.t], (ncol(C14p) + 3)),
      d14C = c(c(C14p),
               C14m,
               C14r,
               Datm$d14c[ix.t]),
      pool = rep(c(pnms, "bulkC", "respiration", "atm"), 
                 each = nrow(C14p))) %>%
      distinct
  }
}