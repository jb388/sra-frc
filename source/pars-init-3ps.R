## 3-pool series model initial parameter search script
# Requires the following functions: modFun; soc.fx; con.df.fx; C14.plot.fx
# Requires the following input: obs.bulk.14c, obs.resp.14c, obs.Cstock
# These functions and inputs are in Rmd file "sra-frc.RMD" (this directory)

# Initialize lists (9 sites by 3 depths = 27 elements)
pars.i.3ps <- lapply(seq_len(27), function(df) {
  data.frame(kf = NA, ks = NA, kp = NA, a21 = NA, a32 = NA)
})
names(pars.i.3ps) <- names(obs.bulk.14c)

# ANpp
##### 
## 0-10
PMeco_depth <- names(pars.i.3ps)[1]
k1 <- .1
k2 <- .01
k3 <- .0005
a21 <- .25
a32 <- .008
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3ps)[2]
k1 <- .05
k2 <- .006
k3 <- .0005
a21 <- .5
a32 <- .03
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.3ps)[3]
k1 <- .03
k2 <- .004
k3 <- .0009
a21 <- .7
a32 <- .05
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, lag = 30, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars
#####

# ANrf
#####
## 0-10
PMeco_depth <- names(pars.i.3ps)[4]
k1 <- .08
k2 <- .02
k3 <- .0008
a21 <- .6
a32 <- .05
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3ps)[5]
k1 <- .02
k2 <- .005
k3 <- .0007
a21 <- .5
a32 <- .05
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, lag = 10, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.3ps)[6]
k1 <- .01
k2 <- .005
k3 <- .0005
a21 <- .5
a32 <- .05
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, lag = 30, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars
#####

# ANwf
#####
## 0-10
PMeco_depth <- names(pars.i.3ps)[7]
k1 <- .02
k2 <- .003
k3 <- .0005
a21 <- .6
a32 <- .05
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, lag = 20, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3ps)[8]
k1 <- .02
k2 <- .008
k3 <- .0005
a21 <- .5
a32 <- .08
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.3ps)[9]
k1 <- .02
k2 <- .008
k3 <- .0004
a21 <- .5
a32 <- .08
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars
#####

# BSpp
#####
## 0-10
PMeco_depth <- names(pars.i.3ps)[10]
k1 <- .08
k2 <- .01
k3 <- .002
a21 <- .5
a32 <- .05
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3ps)[11]
k1 <- .02
k2 <- .008
k3 <- .001
a21 <- .5
a32 <- .05
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.3ps)[12]
k1 <- .01
k2 <- .005
k3 <- .001
a21 <- .5
a32 <- .05
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars
#####

# BSrf
#####
## 0-10
PMeco_depth <- names(pars.i.3ps)[13]
k1 <- .1
k2 <- .01
k3 <- .001
a21 <- .5
a32 <- .05
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3ps)[14]
k1 <- .04
k2 <- .01
k3 <- .001
a21 <- .5
a32 <- .05
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.3ps)[15]
k1 <- .1
k2 <- .008
k3 <- .0009
a21 <- .7
a32 <- .05
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, lag = 20, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars
#####

# BSwf
#####
## 0-10
PMeco_depth <- names(pars.i.3ps)[16]
k1 <- .2
k2 <- .02
k3 <- .001
a21 <- .5
a32 <- .04
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3ps)[17]
k1 <- .3
k2 <- .05
k3 <- .001
a21 <- .4
a32 <- .05
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.3ps)[18]
k1 <- .25
k2 <- .04
k3 <- .0007
a21 <- .2
a32 <- .02
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars
#####

# GRpp
#####
## 0-10
PMeco_depth <- names(pars.i.3ps)[19]
k1 <- .15
k2 <- .04
k3 <- .002
a21 <- .6
a32 <- .02
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3ps)[20]
k1 <- .03
k2 <- .01
k3 <- .001
a21 <- .6
a32 <- .02
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, lag = 30, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars

## 20-30
PMeco_depth <- names(pars.i.3ps)[21]
k1 <- .01
k2 <- .005
k3 <- .001
a21 <- .5
a32 <- .05
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars
#####

# GRrf
#####
## 0-10
PMeco_depth <- names(pars.i.3ps)[22]
k1 <- .01
k2 <- .005
k3 <- .0009
a21 <- .5
a32 <- .04
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3ps)[23]
k1 <- .01
k2 <- .005
k3 <- .001
a21 <- .5
a32 <- .05
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.3ps)[24]
k1 <- .01
k2 <- .005
k3 <- .0005
a21 <- .5
a32 <- .06
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars
#####

# GRwf
#####
## 0-10
PMeco_depth <- names(pars.i.3ps)[25]
k1 <- .06
k2 <- .05
k3 <- .001
a21 <- .2
a32 <- .02
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3ps)[26]
k1 <- .3
k2 <- .05
k3 <- .0006
a21 <- .2
a32 <- .01
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.3ps)[27]
k1 <- .01
k2 <- .01
k3 <- .0004
a21 <- .2
a32 <- .1
pars <- c(k1, k2, k3, a21, a32)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3ps", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3ps[[PMeco_depth]] <- pars
#####

save(pars.i.3ps, file = "/Users/jeff/sra-frc/data/derived/modFit_pars/pars.i.3ps.RData")
