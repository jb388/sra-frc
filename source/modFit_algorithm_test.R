# modFit algorithm comparison
# Example model structure: 2pp, w/ lag

# load initial pars and add 10 year lag as default
load("/Users/jeff/sra-frc/data/derived/modFit_pars/pars.i.2pp.RData")
pars.i.2pp.lag <- lapply(pars.i.2pp, function(x) c(x, 10)) # start w/ 10 yr

# load Nelder-Mead results
load("/Users/jeff/sra-frc/data/derived/modFit_pars/mod.sens.fits.2pp.lag_2022-09-06.RData")

# degeneracy check:
cvg10 <- which(unlist(lapply(lapply(mod.sens.fits.2pp.lag, "[[", 1), "[[", "convergence")) == 10)

# Note: methods "Pseudo", "Port", "L-BFGS-B" have singular matrix issues; collinear pars?

# Marq
cvg10.2pp.lag.redo.Marq <- mod.fits.fx(
  pars = pars.i.2pp.lag,
  mod = "2pp",
  In = in.1.ls,
  sub = cvg10,
  upper = c(1, 1, 1, 100), # max lag = 30 y (cf. 40 y for Koarashi et al. 2012 w/ depth max = 60 cm)
  lower = c(0, 0, 0, 0),
  method = "Marq",
  cost = "14C",
  fit_lag = TRUE
)
save(cvg10.2pp.lag.redo.Marq, file = paste0("/Users/jeff/sra-frc/data/derived/modFit_tests/", "cvg10.2pp.lag.redo.Marq", "_", Sys.Date(), ".RData"))
mod.fits.2pp.lag.redo.Marq <- lapply(cvg10.2pp.lag.redo.Marq, function(x) x[[1]])
names(mod.fits.2pp.lag.redo.Marq) <- names(cvg10)
# check convergence
lapply(mod.fits.2pp.lag.redo.Marq, "[[", "info") # info = 2 or 3 (good)

# Newton
cvg10.2pp.lag.redo.Newton <- mod.fits.fx(
  pars = pars.i.2pp.lag,
  mod = "2pp",
  In = in.1.ls,
  sub = cvg10,
  upper = c(1, 1, 1, 100), # max lag = 30 y (cf. 40 y for Koarashi et al. 2012 w/ depth max = 60 cm)
  lower = c(0, 0, 0, 0),
  method = "Newton",
  cost = "14C",
  fit_lag = TRUE
)
save(cvg10.2pp.lag.redo.Newton, file = paste0("/Users/jeff/sra-frc/data/derived/modFit_tests/", "cvg10.2pp.lag.redo.Newton", "_", Sys.Date(), ".RData"))
mod.fits.2pp.lag.redo.Newton <- lapply(cvg10.2pp.lag.redo.Newton, function(x) x[[1]])
names(mod.fits.2pp.lag.redo.Newton) <- names(cvg10)
# check convergence
lapply(mod.fits.2pp.lag.redo.Newton, "[[", "code") # code = 1 or 2 (good); 3 = bad [tried reducing steptol to no avail]
ix.Newton <- which(unlist(lapply(mod.fits.2pp.lag.redo.Newton, "[[", "code")) < 3)
mod.fits.2pp.lag.redo.Newton2 <- mod.fits.2pp.lag.redo.Newton[ix.Newton]

# CG
cvg10.2pp.lag.redo.CG <- mod.fits.fx(
  pars = pars.i.2pp.lag,
  mod = "2pp",
  In = in.1.ls,
  sub = cvg10,
  upper = c(1, 1, 1, 100), # max lag = 30 y (cf. 40 y for Koarashi et al. 2012 w/ depth max = 60 cm)
  lower = c(0, 0, 0, 0),
  method = "CG",
  cost = "14C",
  fit_lag = TRUE
)
save(cvg10.2pp.lag.redo.CG, file = paste0("/Users/jeff/sra-frc/data/derived/modFit_tests/", "cvg10.2pp.lag.redo.CG", "_", Sys.Date(), ".RData"))
mod.fits.2pp.lag.redo.CG <- lapply(cvg10.2pp.lag.redo.CG, function(x) x[[1]])
names(mod.fits.2pp.lag.redo.CG) <- names(cvg10)
# check convergence
lapply(mod.fits.2pp.lag.redo.CG, "[[", "convergence") # 0 = good; 1 = maxit reached


# BFGS
cvg10.2pp.lag.redo.BFGS <- mod.fits.fx(
  pars = pars.i.2pp.lag,
  mod = "2pp",
  In = in.1.ls,
  sub = cvg10,
  upper = c(1, 1, 1, 100), # max lag = 30 y (cf. 40 y for Koarashi et al. 2012 w/ depth max = 60 cm)
  lower = c(0, 0, 0, 0),
  method = "BFGS",
  cost = "14C",
  fit_lag = TRUE
)
save(cvg10.2pp.lag.redo.BFGS, file = paste0("/Users/jeff/sra-frc/data/derived/modFit_tests/", "cvg10.2pp.lag.redo.BFGS", "_", Sys.Date(), ".RData"))
mod.fits.2pp.lag.redo.BFGS <- lapply(cvg10.2pp.lag.redo.BFGS, function(x) x[[1]])
names(mod.fits.2pp.lag.redo.BFGS) <- names(cvg10)
# check convergence
lapply(mod.fits.2pp.lag.redo.BFGS, "[[", "convergence") # 0 = good; 1 = maxit reached

# get pars
pars.fit.Marq <- lapply(mod.fits.2pp.lag.redo.Marq, "[[", "par")
pars.fit.Newton <- lapply(mod.fits.2pp.lag.redo.Newton, "[[", "par")
pars.fit.BFGS <- lapply(mod.fits.2pp.lag.redo.BFGS, "[[", 1)
pars.fit.CG <- lapply(mod.fits.2pp.lag.redo.CG, "[[", 1)

# load mod.fits as needed

  
# compare ssr
get.ssr.fx <- function(mod.fit.ls, mod) {
  data.frame(bind_rows(lapply(mod.fit.ls, "[", "ssr"), .id = "PMeco_depth")) %>%
    mutate(mod = mod,
           depth = sapply(strsplit(PMeco_depth, "_"), "[[", 2))
}

# Marq
ssr.Marq <- get.ssr.fx(mod.fits.2pp.lag.redo.Marq, "2pp") # really high SSR, but fits (below) look good?
ssr.Newton <- get.ssr.fx(mod.fits.2pp.lag.redo.Newton, "2pp") # OK fits
ssr.CG <- get.ssr.fx(mod.fits.2pp.lag.redo.CG, "2pp") # better fits
ssr.BFGS <- get.ssr.fx(mod.fits.2pp.lag.redo.BFGS, "2pp") # marginal improvement on CG fits

# fit models
## model fx
modFun <- function(pars, mod, In, lag = 0, pass = TRUE, verbose = TRUE, out = "modFit", PMeco_depth = NULL, var_14c = "d14c", fit_lag = FALSE) {
  
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
    model <- OnepModel14(
      t = Datm$Date[ix.t],
      k = soc.fx_out[[1]],
      C0 = as.vector(ss.cstock),
      F0_Delta14C = Delta14C_from_AbsoluteFractionModern(fm(soc.fx_out[[1]])),
      In = soc.fx_out[[2]],
      inputFc = Datm,
      lag = lag,
      pass = pass)
  } else {
    
    # calculate initial 14C
    F0_Delta14C <- unlist(
      lapply(-diag(soc.fx_out[[1]]), function(x) Delta14C_from_AbsoluteFractionModern(fm(x))))
    
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
  C14m <- getF14C(model)
  C14p <- getF14(model) 
  C14r <- getF14R(model)
  Ctot <- getC(model)
  
  if (var_14c == "fm") {
    dates <- Datm$Date[ix.t]
    for (i in seq_along(dates)) {
      C14m[i] <- convert_fm_d14c(d14c = C14m[i], obs_date_y = dates[i], verbose = FALSE) 
      C14p[i] <- convert_fm_d14c(d14c = C14p[i], obs_date_y = dates[i], verbose = FALSE) 
      C14r[i] <- convert_fm_d14c(d14c = C14r[i], obs_date_y = dates[i], verbose = FALSE)
    }
  }
  
  if (out == "modFit") {
    # dataframe for modFit fx
    data.frame(
      time = Datm$Date[ix.t],
      resp = C14r,
      bulkC = C14m,
      cStock = sum(Ctot[1, ]))
  } else {
    
    # sum c stocks
    ss.cstock <- round(ss.cstock, 2)
    cstock.sum <- ifelse(is.null(dim(ss.cstock)), ss.cstock, colSums(ss.cstock))
    
    if (verbose) {
      # print site and steady-state stocks
      if (!is.null(PMeco_depth)) cat(paste0(PMeco_depth, "\n"))
      for (i in seq_along(ss.cstock)) {
        cat(paste(soc.fx_out[[4]][i], ss.cstock[i], "\n"))
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
      pool = rep(c(soc.fx_out[[4]], "bulkC", "respiration", "atm"), 
                 each = nrow(C14p))) %>%
      distinct
  }
}

## fits
mod.fitted.Marq <- lapply(seq_along(pars.fit.Marq), function(i) {
  modFun(pars = pars.fit.Marq[[i]], In = in.1.ls[cvg10][[i]], mod = "2pp", 
         verbose = FALSE, out = "plot.df")
})
names(mod.fitted.Marq) <- names(cvg10)

mod.fitted.Newton <- lapply(seq_along(pars.fit.Newton), function(i) {
  modFun(pars = pars.fit.Newton[[i]], In = in.1.ls[cvg10][[i]], mod = "2pp", 
         verbose = FALSE, out = "plot.df")
})
names(mod.fitted.Newton) <- names(cvg10)

mod.fitted.BFGS <- lapply(seq_along(pars.fit.BFGS), function(i) {
  modFun(pars = pars.fit.BFGS[[i]], In = in.1.ls[cvg10][[i]], mod = "2pp", 
         verbose = FALSE, out = "plot.df")
})
names(mod.fitted.BFGS) <- names(cvg10)

mod.fitted.CG <- lapply(seq_along(pars.fit.CG), function(i) {
  modFun(pars = pars.fit.CG[[i]], In = in.1.ls[cvg10][[i]], mod = "2pp", 
         verbose = FALSE, out = "plot.df")
})
names(mod.fitted.CG) <- names(cvg10)


# plot fits
## plot fx
multiMod.fit.plot.fx <- function(fit1, fit1.name, 
                                 fit2 = NULL, fit2.name = NULL, 
                                 fit3 = NULL, fit3.name = NULL, 
                                 fit4 = NULL, fit4.name = NULL, 
                                 sensrange = FALSE, avals_sr = NULL) {
  
  lapply(seq_along(fit1), function(i) {
    PMeco_depth <- names(fit1)[i]
    con.df <- con.df.fx(PMeco_depth)
    plot.df <- rbind(fit1[[i]],
                     fit2[[i]],
                     fit3[[i]],
                     fit4[[i]])
    plot.df$Model <- factor(c(rep(fit1.name, nrow(fit1[[i]])),
                              rep(fit2.name, nrow(fit2[[i]])),
                              rep(fit3.name, nrow(fit3[[i]])),
                              rep(fit4.name, nrow(fit4[[i]]))),
                            levels = c(fit1.name, fit2.name, fit3.name, fit4.name))
    
    p <- plot.df %>%
      filter(pool == "bulkC" | pool == "respiration" | pool == "atm") %>%
      ggplot(., aes(years, d14C)) +
      geom_path(aes(linetype = Model, color = pool)) +
      geom_point(data = con.df, aes(Year, d14c, color = pool), size = 3) +
      scale_color_manual(
        name = "Model pool",
        values = c("atm" = 8,
                   "bulkC" = "black",
                   "respiration" = "#e47b1f")) +
      scale_x_continuous(limits = c(1950, 2022)) +
      ggtitle(PMeco_depth) +
      xlab("Year") +
      ylab(expression(''*Delta*''^14*'C (‰)')) +
      theme_bw() +
      theme(panel.grid = element_blank())
    
    if (sensrange) {
      if (is.null(avals_sr)) stop("alpha values must be specified when sensrange = TRUE")
      p + 
        geom_ribbon(aes(ymin = q95, ymax = q05, fill = pool, alpha = Model)) +
        scale_fill_manual(
          name = "Model pool",
          values = c("bulkC" = "black",
                     "respiration" = "#e47b1f")) +
        scale_alpha_manual(
          name = "Model",
          values = avals_sr)
    } else {
      p
    }
  })
}

## plots
multiMod.fit.plot.fx(mod.fitted.2pp.lag100[cvg10], "Nelder-Mead", mod.fitted.Marq, "Marq", mod.fitted.Newton, "Newton", mod.fitted.BFGS, "BFGS")

# Nelder-Mead fits still seem best...