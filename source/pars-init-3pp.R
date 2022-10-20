## 3-pool parallel model initial parameter search script
# Requires the following functions: modFun; soc.fx; con.df.fx; C14.plot.fx
# Requires the following input: obs.bulk.14c, obs.resp.14c, obs.Cstock, frc.14c.df
# These functions and inputs are in Rmd file "mod-strs.RMD" (this directory)

load("/Users/jeff/sra-frc/source/frc.14c.df.RData")

# Initialize lists (9 sites by 3 depths = 27 elements)
pars.i.3pp <- lapply(seq_len(27), function(df) {
  data.frame(kf = NA, ks = NA, kp = NA, gam1 = NA, gam2 = NA)
})
names(pars.i.3pp) <- names(obs.bulk.14c)

# function for deriving k3 by site
k3.fx <- function(PMeco_depth) {
  
  # fit kp  w/ thermal data
  if (grepl("AN", PMeco_depth)) {
    j <- 1
  } else if (grepl("BS", PMeco_depth)) {
    j <- 2
  } else {
    j <- 3
  }
  SOMslow_d14c <- tapply(frc.14c.df$d14c, frc.14c.df$PM, min)[[j]]
  k(convert_fm_d14c(d14c = SOMslow_d14c, obs_date_y = 2019, verbose = FALSE))
}

# ANpp
##### 
## 0-10
PMeco_depth <- names(pars.i.3pp)[1]
k1 <- .4
k2 <- .05
k3 <- .001
gam1 <- .19
gam2 <- .8
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3pp)[2]
k1 <- .05
k2 <- .01
k3 <- .0009
gam1 <- .1
gam2 <- .85
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.3pp)[3]
k1 <- .03
k2 <- .005
k3 <- .0009
gam1 <- .09
gam2 <- .83
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, lag = 30, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars
#####

# ANrf
#####
## 0-10
PMeco_depth <- names(pars.i.3pp)[4]
k1 <- .08
k2 <- .01
k3 <- .001
gam1 <- .5
gam2 <- .49
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3pp)[5]
k1 <- .05
k2 <- .005
k3 <- .0007
gam1 <- .1
gam2 <- .87
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, lag = 10, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.3pp)[6]
k1 <- .05
k2 <- .005
k3 <- .0007
gam1 <- .1
gam2 <- .87
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, lag = 30, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars
#####

# ANwf
#####
## 0-10
PMeco_depth <- names(pars.i.3pp)[7]
k1 <- .02
k2 <- .008
k3 <- .001
gam1 <- .48
gam2 <- .45
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3pp)[8]
k1 <- .02
k2 <- .008
k3 <- .001
gam1 <- .48
gam2 <- .4
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.3pp)[9]
k1 <- .02
k2 <- .008
k3 <- .0008
gam1 <- .47
gam2 <- .4
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars
#####

# BSpp
#####
## 0-10
PMeco_depth <- names(pars.i.3pp)[10]
k1 <- .08
k2 <- .01
k3 <- .001
gam1 <- .5
gam2 <- .49
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3pp)[11]
k1 <- .02
k2 <- .008
k3 <- .001
gam1 <- .48
gam2 <- .47
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.3pp)[12]
k1 <- .01
k2 <- .005
k3 <- .001
gam1 <- .1
gam2 <- .85
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars
#####

# BSrf
#####
## 0-10
PMeco_depth <- names(pars.i.3pp)[13]
k1 <- .1
k2 <- .01
k3 <- .001
gam1 <- .6
gam2 <- .38
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3pp)[14]
k1 <- .1
k2 <- .018
k3 <- .0007
gam1 <- .19
gam2 <- .79
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.3pp)[15]
k1 <- .25
k2 <- .01
k3 <- .0008
gam1 <- .18
gam2 <- .77
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, lag = 20, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars
#####

# BSwf
#####
## 0-10
PMeco_depth <- names(pars.i.3pp)[16]
k1 <- .2
k2 <- .02
k3 <- .001
gam1 <- .75
gam2 <- .24
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3pp)[17]
k1 <- .1
k2 <- .07
k3 <- .002
gam1 <- .29
gam2 <- .65
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.3pp)[18]
k1 <- .1
k2 <- .01
k3 <- .001
gam1 <- .29
gam2 <- .65
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars
#####

# GRpp
#####
## 0-10
PMeco_depth <- names(pars.i.3pp)[19]
k1 <- .15
k2 <- .04
k3 <- .002
gam1 <- .6
gam2 <- .39
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3pp)[20]
k1 <- .1
k2 <- .01
k3 <- .005
gam1 <- .1
gam2 <- .85
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, lag = 30, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars

## 20-30
PMeco_depth <- names(pars.i.3pp)[21]
k1 <- .01
k2 <- .005
k3 <- .001
gam1 <- .1
gam2 <- .85
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars
#####

# GRrf
#####
## 0-10
PMeco_depth <- names(pars.i.3pp)[22]
k1 <- .01
k2 <- .005
k3 <- .0009
gam1 <- .5
gam2 <- .48
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3pp)[23]
k1 <- .01
k2 <- .005
k3 <- .001
gam1 <- .1
gam2 <- .85
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.3pp)[24]
k1 <- .01
k2 <- .005
k3 <- .001
gam1 <- .1
gam2 <- .85
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars
#####

# GRwf
#####
## 0-10
PMeco_depth <- names(pars.i.3pp)[25]
k1 <- .07
k2 <- .04
k3 <- .001
gam1 <- .55
gam2 <- .44
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars


## 10-20
PMeco_depth <- names(pars.i.3pp)[26]
k1 <- .01
k2 <- .005
k3 <- .001
gam1 <- .1
gam2 <- .85
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars


## 20-30
PMeco_depth <- names(pars.i.3pp)[27]
k1 <- .01
k2 <- .005
k3 <- .001
gam1 <- .1
gam2 <- .85
pars <- c(k1, k2, k3, gam1, gam2)

# plot
C14.plot.fx(modFun(pars = pars, In = 1, mod = "3pp", out = "", verbose = TRUE, PMeco_depth = PMeco_depth), 
            con.df.fx(PMeco_depth), "3pp", pool_filter = c("bulkC", "respiration", "atm"), PMeco_depth)

# save pars
pars.i.3pp[[PMeco_depth]] <- pars
#####

save(pars.i.3pp, file = "/Users/jeff/sra-frc/data/derived/modFit_pars/pars.i.3pp.RData")
