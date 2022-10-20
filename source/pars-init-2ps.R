## 2-pool series model initial parameter search script
# Requires the following functions: modFun; soc.fx; con.df.fx; C14.plot.fx
# Requires the following input: obs.bulk.14c, obs.resp.14c, obs.Cstock
# These functions and inputs are in Rmd file "mod-strs.RMD" (this directory)

# Initialize lists (9 sites by 3 depths = 27 elements)
pars.i.2ps <- vector(mode = "list", length = 27)
names(pars.i.2ps) <- names(obs.bulk.14c)

# ANpp
##### 
## 0-10
PMeco_depth <- names(pars.i.2ps)[1]
kf <- .1
ks <- .002
a21 <- .01
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.2ps)[2]
kf <- .015
ks <- .0015
a21 <- .2
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.2ps)[3]
kf <- .005
ks <- .0015
a21 <- .2
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
#####

# ANrf
#####
## 0-10
PMeco_depth <- names(pars.i.2ps)[4]
kf <- .02
ks <- .004
a21 <- .2
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.2ps)[5]
kf <- .01
ks <- .002
a21 <- .2
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.2ps)[6]
kf <- .008
ks <- .0011
a21 <- .2
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
#####

# ANwf
#####
## 0-10
PMeco_depth <- names(pars.i.2ps)[7]
kf <- .014
ks <- .001
a21 <- .1
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.2ps)[8]
kf <- .013
ks <- .0008
a21 <- .1
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.2ps)[9]
kf <- .02
ks <- .001
a21 <- .25
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
#####

# BSpp
#####
## 0-10
PMeco_depth <- names(pars.i.2ps)[10]
kf <- .05
ks <- .008
a21 <- .4
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.2ps)[11]
kf <- .02
ks <- .006
a21 <- .5
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.2ps)[12]
kf <- .02
ks <- .003
a21 <- .7
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
#####

# BSrf
#####
## 0-10
PMeco_depth <- names(pars.i.2ps)[13]
kf <- .08
ks <- .005
a21 <- .4
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.2ps)[14]
kf <- .1
ks <- .0045
a21 <- .2
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.2ps)[15]
kf <- .08
ks <- .0024
a21 <- .3
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
#####

# BSwf
#####
## 0-10
PMeco_depth <- names(pars.i.2ps)[16]
kf <- .15
ks <- .0045
a21 <- .2
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.2ps)[17]
kf <- .2
ks <- .003
a21 <- .05
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.2ps)[18]
kf <- .2
ks <- .003
a21 <- .05
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
#####

# GRpp
#####
## 0-10
PMeco_depth <- names(pars.i.2ps)[19]
kf <- .05
ks <- .008
a21 <- .1
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.2ps)[20]
kf <- .03
ks <- .01
a21 <- .8
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.2ps)[21]
kf <- .02
ks <- .0035
a21 <- .6
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
#####

# GRrf
#####
## 0-10
PMeco_depth <- names(pars.i.2ps)[22]
kf <- .02
ks <- .005
a21 <- .7
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.2ps)[23]
kf <- .005
ks <- .005
a21 <- .5
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.2ps)[24]
kf <- .005
ks <- .005
a21 <- .5
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
#####

# GRwf
#####
## 0-10
PMeco_depth <- names(pars.i.2ps)[25]
kf <- .02
ks <- .0009
a21 <- .01
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.2ps)[26]
kf <- .35
ks <- .004
a21 <- .05
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.2ps)[27]
kf <- .005
ks <- .001
a21 <- .05
pars <- c(kf, ks, a21)

# plot
C14.plot.fx(plot.df = modFun(pars = pars, In = 1, mod = "2ps", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df = con.df.fx(PMeco_depth), 
            mod = "2ps", 
            # pool_filter = NULL,
            PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
#####

# save
save(pars.i.2ps, file = "/Users/jeff/sra-frc/data/derived/modFit_pars/pars.i.2ps.RData")
