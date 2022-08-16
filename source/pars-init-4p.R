## 4-pool model initial parameter search script
# Requires the following functions: con.df.fx; par.4p.fx; C14.4p.plot.fx
# Requires the following input: obs.bulk.14c, obs.resp.14c
# These functions and inputs are in Rmd file...

# Initialize list (9 sites by 3 depths = 27 elements)
pars.i.4p <- lapply(seq_len(27), function(x) {
  data.frame(kpF = NA,
             kpS = NA,
             kmF = NA,
             kmS = NA,
             alfP = NA,
             alfM = NA)
})
names(pars.i.4p) <- names(obs.bulk.14c)

# ANpp_0-10
#####
# Site/depth info
PMeco <- "ANpp"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.frc.df.fx(PMeco_depth)

# initial pars
soc <- csoc.19.0_30[[PMeco_depth]][ , "lyr_soc"]
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.08, .005, .01, .0005)
alphs <- c(.01, .005) # transfer coefs
pars <- c(ks, alphs)

# evaluate pars
ini.4p.C14.df <- par.4p.fx(pars = pars, gam = gam, In = In, lag = 0)

# plot
C14.4p.plot.fx(ini.4p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.4p[[PMeco_depth]] <- pars
#####

# ANpp-10-20
#####
# Site/depth info
PMeco <- "ANpp"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.frc.df.fx(PMeco_depth)

# initial pars
soc <- csoc.19.0_30[[PMeco_depth]][ , "lyr_soc"]
In <- in.est[[PMeco_depth]]
gam <- .87
ks <- c(.04, .0015, .01, .0005)
alphs <- c(.01, .005) # transfer coefs
pars <- c(ks, alphs)

# evaluate pars
ini.4p.C14.df <- par.4p.fx(pars = pars, gam = gam, In = In, lag = 0)

# plot
C14.4p.plot.fx(ini.4p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.4p[[PMeco_depth]] <- pars
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
ks <- c(.02, .003) # fast, slow
tc <- .6 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
ks <- c(.15, .005) # fast, slow
tc <- .3 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)
# C14.2p.plot.fx(par.fx(pars = mod.fits.2ps5.10$`ANrf_0-10`$par, In = In, mod = "2ps"), con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
ks <- c(.02, .003) # fast, slow
tc <- .5 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
ks <- c(.02, .0015) # fast, slow
tc <- .4 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
ks <- c(.02, .001) # fast, slow
tc <- .1 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)
# C14.2p.plot.fx(par.fx(mod.fits.2ps5.10$`ANwf_0-10`$par,
#                       In = In,
#                       mod = "2ps"), con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
ks <- c(.02, .001) # fast, slow
tc <- .2 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
ks <- c(.02, .001) # fast, slow
tc <- .25 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
ks <- c(.1, .008) # fast, slow
tc <- .55 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
ks <- c(.02, .006) # fast, slow
tc <- .5 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
ks <- c(.02, .003) # fast, slow
tc <- .7 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
ks <- c(.24, .0045) # fast, slow
tc <- .2 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
ks <- c(.2, .0045) # fast, slow
tc <- .2 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
ks <- c(.24, .0034) # fast, slow
tc <- .2 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
ks <- c(.3, .0045) # fast, slow
tc <- .2 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
ks <- c(.4, .004) # fast, slow
tc <- .15 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
ks <- c(.4, .004) # fast, slow
tc <- .1 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
ks <- c(.1, .01) # fast, slow
tc <- .18 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
ks <- c(.03, .01) # fast, slow
tc <- .8 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
ks <- c(.02, .0035) # fast, slow
tc <- .6 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
ks <- c(.02, .005) # fast, slow
tc <- .7 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)
# C14.2p.plot.fx(par.fx(pars = mod.fits.2ps5.10$`GRrf_0-10`$par, In = In, mod = "2ps"), con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
ks <- c(.02, .002) # fast, slow
tc <- .2 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
ks <- c(.02, .002) # fast, slow
tc <- .3 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
ks <- c(.15, .008) # fast, slow
tc <- .1 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)
# C14.2p.plot.fx(par.fx(pars = mod.fits.2ps5.10$`GRwf_0-10`$par, In = In, mod = "2ps"), con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
ks <- c(.35, .0035) # fast, slow
tc <- .4 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
ks <- c(.4, .004) # fast, slow
tc <- .9 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


#####

# Save output
save(pars.i.2ps, file = paste0("~/sra-ts/data/derived/modFit_pars/", "pars.i.2ps", "_", Sys.Date(), ".Rdata"))
