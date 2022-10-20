## 2-pool parallel model initial parameter search script
# Requires the following functions: modFun; soc.fx; con.df.fx; C14.plot.fx
# Requires the following input: obs.bulk.14c, obs.resp.14c, obs.Cstock
# These functions and inputs are in Rmd file "mod-strs.RMD" (this directory)

# Initialize lists (9 sites by 3 depths = 27 elements)
pars.i.2pp <- vector(mode = "list", length = 27)
names(pars.i.2pp) <- names(obs.bulk.14c)

# ANpp
##### 
## 0-10
PMeco_depth <- names(pars.i.2pp)[1]
kf <- .1
ks <- .003
gam <- .98
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars

## 10-20
PMeco_depth <- names(pars.i.2pp)[2]
kf <- .013
ks <- .0018
gam <- .8
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.2pp)[3]
kf <- .008
ks <- .0012
gam <- .8
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars
#####

# ANrf
#####
## 0-10
PMeco_depth <- names(pars.i.2pp)[4]
kf <- .015
ks <- .003
gam <- .9
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.2pp)[5]
kf <- .01
ks <- .0018
gam <- .8
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.2pp)[6]
kf <- .01
ks <- .0014
gam <- .6
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars
#####

# ANwf
#####
## 0-10
PMeco_depth <- names(pars.i.2pp)[7]
kf <- .01
ks <- .001
gam <- .9
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.2pp)[8]
kf <- .011
ks <- .00058
gam <- .95
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.2pp)[9]
kf <- .011
ks <- .00046
gam <- .95
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars
#####

# BSpp
#####
## 0-10
PMeco_depth <- names(pars.i.2pp)[10]
kf <- .02
ks <- .004
gam <- .85
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.2pp)[11]
kf <- .01
ks <- .0015
gam <- .95
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.2pp)[12]
kf <- .009
ks <- .002
gam <- .6
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars
#####

# BSrf
#####
## 0-10
PMeco_depth <- names(pars.i.2pp)[13]
kf <- .12
ks <- .005
gam <- .7
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.2pp)[14]
kf <- .12
ks <- .005
gam <- .7
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.2pp)[15]
kf <- .08
ks <- .003
gam <- .7
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, lag = lag, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars
#####

# BSwf
#####
## 0-10
PMeco_depth <- names(pars.i.2pp)[16]
kf <- .03
ks <- .004
gam <- .5
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.2pp)[17]
kf <- .1
ks <- .004
gam <- .6
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.2pp)[18]
kf <- .15
ks <- .002
gam <- .7
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars
#####

# GRpp
#####
## 0-10
PMeco_depth <- names(pars.i.2pp)[19]
kf <- .05
ks <- .008
gam <- .9
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.2pp)[20]
kf <- .01
ks <- .008
gam <- .8
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.2pp)[21]
kf <- .008
ks <- .004
gam <- .6
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars
#####

# GRrf
#####
## 0-10
PMeco_depth <- names(pars.i.2pp)[22]
kf <- .008
ks <- .004
gam <- .6
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.2pp)[23]
kf <- .008
ks <- .002
gam <- .6
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.2pp)[24]
kf <- .008
ks <- .002
gam <- .6
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars
#####

# GRwf
#####
## 0-10
PMeco_depth <- names(pars.i.2pp)[25]
kf <- .02
ks <- .004
gam <- .9
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.2pp)[26]
kf <- .01
ks <- .003
gam <- .6
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars

## 20-30
PMeco_depth <- names(pars.i.2pp)[27]
kf <- .3
ks <- .0035
gam <- .1
pars <- c(kf, ks, gam)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2pp", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2pp[[PMeco_depth]] <- pars
#####

# save
save(pars.i.2pp, file = "/Users/jeff/sra-frc/data/derived/modFit_pars/pars.i.2pp.RData")