## 1-pool model initial parameter search script
# Requires the following functions: modFun; soc.fx; con.df.fx; C14.plot.fx
# Requires the following input: obs.bulk.14c, obs.resp.14c, obs.Cstock
# These functions and inputs are in Rmd file "mod-strs.RMD" (this directory)

# Initialize lists (9 sites by 3 depths = 27 elements)
pars.i.1p <- vector(mode = "list", length = 27)
names(pars.i.1p) <- names(obs.bulk.14c)

# ANpp
##### 
## 0-10
PMeco_depth <- names(pars.i.1p)[1]
pars <- .2

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.1p)[2]
pars <- .003

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.1p)[3]
pars <- .0025

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars
#####

# ANrf
#####
## 0-10
PMeco_depth <- names(pars.i.1p)[4]
pars <- .2

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.1p)[5]
pars <- .0032

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.1p)[6]
pars <- .002

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars
#####

# ANwf
#####
## 0-10
PMeco_depth <- names(pars.i.1p)[7]
pars <- .003

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.1p)[8]
pars <- .0018

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.1p)[9]
pars <- .0013

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars
#####

# BSpp
#####
## 0-10
PMeco_depth <- names(pars.i.1p)[10]
pars <- .01

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.1p)[11]
pars <- .007

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.1p)[12]
pars <- .003

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars
#####

# BSrf
#####
## 0-10
PMeco_depth <- names(pars.i.1p)[13]
pars <- .005

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.1p)[14]
pars <- .005

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.1p)[15]
pars <- .004

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars
#####

# BSwf
#####
## 0-10
PMeco_depth <- names(pars.i.1p)[16]
pars <- .005

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.1p)[17]
pars <- .003

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.1p)[18]
pars <- .004
mod.i <- modFun(pars = pars, In = 1, mod = "1p", out = "", verbose = TRUE, PMeco_depth = PMeco_depth)

# plot
C14.plot.fx(mod.i, con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars
#####

# GRpp
#####
## 0-10
PMeco_depth <- names(pars.i.1p)[19]
pars <- .014

# plot
C14.plot.fx(modFun(pars = pars, mod = "1p", In = 1, out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.1p)[20]
pars <- .009

# plot
C14.plot.fx(modFun(pars = pars, mod = "1p", In = 1, out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth = PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.1p)[21]
pars <- .005

# plot
C14.plot.fx(modFun(pars = pars, mod = "1p", In = 1, out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth = PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars
#####

# GRrf
#####
## 0-10
PMeco_depth <- names(pars.i.1p)[22]
pars <- .006

# plot
C14.plot.fx(modFun(pars = pars, mod = "1p", In = 1, out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.1p)[23]
pars <- .0032

# plot
C14.plot.fx(modFun(pars = pars, mod = "1p", In = 1, out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth = PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.1p)[24]
pars <- .0028

# plot
C14.plot.fx(modFun(pars = pars, mod = "1p", In = 1, out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth = PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars
#####

# GRwf
#####
## 0-10
PMeco_depth <- names(pars.i.1p)[25]
pars <- .2

# plot
C14.plot.fx(modFun(pars = pars, mod = "1p", In = 1, out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.1p)[26]
pars <- .005

# plot
C14.plot.fx(modFun(pars = pars, mod = "1p", In = 1, out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth = PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.1p)[27]
pars <- .0032

# plot
C14.plot.fx(modFun(pars = pars, mod = "1p", In = 1, out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "1p", pool_filter = c("bulkC", "atm"), PMeco_depth = PMeco_depth)

# save pars
pars.i.1p[[PMeco_depth]] <- pars
#####

# save
save(pars.i.1p, file = "/Users/jeff/sra-frc/data/derived/modFit_pars/pars.i.1p.RData")
