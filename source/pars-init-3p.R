## 3-pool model initial parameter search script
# Requires the following functions: con.df.fx; par.3p.fx; C14.3p.plot.fx
# Requires the following input: obs.bulk.14c, obs.resp.14c
# These functions and inputs are in Rmd file...

# Initialize list (9 sites by 3 depths = 27 elements)
pars.i.3p <- lapply(seq_len(27), function(x) {
  data.frame(kP = NA,
             kM = NA,
             alph = NA)
})
names(pars.i.3p) <- names(obs.bulk.14c)

# initial pars for 3ps mod
pars.i.3ps <- lapply(seq_len(27), function(x) {
  data.frame(kP = NA,
             kM = NA,
             a21 = NA,
             a32 = NA)
})
names(pars.i.3ps) <- names(obs.bulk.14c)

# ANpp_0-10
#####
# Site/depth info
PMeco <- "ANpp"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
soc <- csoc.19.0_30[[PMeco_depth]][ , "lyr_soc"]
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.1, .01)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars
ini.3p.C14.df <- modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df")

# plot
C14.3p.plot.fx(ini.3p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# ANpp-10-20
#####
# Site/depth info
PMeco <- "ANpp"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
soc <- csoc.19.0_30[[PMeco_depth]][ , "lyr_soc"]
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.2, .02)
alpha <- 0.005 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# ANpp-20-30
#####
# Site/depth info
PMeco <- "ANpp"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.5, .03)
alpha <- 0.005 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# ANrf-0-10
#####
# Site/depth info
PMeco <- "ANrf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.15, .02)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# ANrf-10-20
#####
# Site/depth info
PMeco <- "ANrf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.5, .03)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# ANrf-20-30
#####
# Site/depth info
PMeco <- "ANrf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.5, .03)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# ANwf-0-10
#####
# Site/depth info
PMeco <- "ANwf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.14, .01)
alpha <- 0.006 # transfer coefs
pars <- c(ks, alpha)

# pars for 

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# ANwf-10-20
#####
# Site/depth info
PMeco <- "ANwf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.5, .03)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# ANwf-20-30
#####
# Site/depth info
PMeco <- "ANwf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.5, .03)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# initial pars 3ps
In <- in.est[[PMeco_depth]]
ks <- c(.1, .008)
a21 <- .05
a32 <- .005
pars <- c(ks, a21, a32)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, mod = "3ps", In = In, PM = substr(PMeco, 1, 2), lag = 20, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# BSpp-0-10
#####
# Site/depth info
PMeco <- "BSpp"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.1, .014)
alpha <- 0.005 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# BSpp-10-20
#####
# Site/depth info
PMeco <- "BSpp"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.5, .03)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# BSpp-20-30
#####
# Site/depth info
PMeco <- "BSpp"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.5, .03)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# BSrf-0-10
#####
# Site/depth info
PMeco <- "BSrf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.2, .01)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# BSrf-10-20
#####
# Site/depth info
PMeco <- "BSrf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.5, .03)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# BSrf-20-30
#####
# Site/depth info
PMeco <- "BSrf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.5, .03)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# BSwf-0-10
#####
# Site/depth info
PMeco <- "BSwf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.2, .02)
alpha <- 0.005 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# BSwf-10-20
#####
# Site/depth info
PMeco <- "BSwf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.5, .03)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# BSwf-20-30
#####
# Site/depth info
PMeco <- "BSwf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.5, .03)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# GRpp-0-10
#####
# Site/depth info
PMeco <- "GRpp"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.05, .05)
alpha <- 0.005 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# GRpp-10-20
#####
# Site/depth info
PMeco <- "GRpp"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.5, .03)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# GRpp-20-30
#####
# Site/depth info
PMeco <- "GRpp"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.5, .03)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# GRrf-0-10
#####
# Site/depth info
PMeco <- "GRrf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.01, .002)
alpha <- 0.005 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# GRrf-10-20
#####
# Site/depth info
PMeco <- "GRrf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.5, .03)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# GRrf-20-30
#####
# Site/depth info
PMeco <- "GRrf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.5, .03)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# GRwf-0-10
#####
# Site/depth info
PMeco <- "GRwf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.085, .05)
alpha <- 0.003 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# GRwf-10-20
#####
# Site/depth info
PMeco <- "GRwf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.5, .03)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars
#####

# GRwf-20-30
#####
# Site/depth info
PMeco <- "GRwf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.5, .03)
alpha <- 0.002 # transfer coefs
pars <- c(ks, alpha)

# evaluate pars and plot
C14.3p.plot.fx(
  modFun_3p(pars = pars, gam = gam, In = In, PM = substr(PMeco, 1, 2), lag = 0, out = "plot.df"), 
  con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.3p[[PMeco_depth]] <- pars


#####

# Save output
save(pars.i.3p, file = paste0("~/sra-frc/data/derived/modFit_pars/", "pars.i.3ps", "_", Sys.Date(), ".Rdata"))
