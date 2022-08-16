FmDist <- function(sample, ramp, bulkFm = bulkFm, FmTemp = FmTemp, dfn = 3, reso = 0.1, mp_temps = NA, all.plots = FALSE){
  library(splines2)
  library(splines)
  library(zoo)
  library(DescTools)
  library(ggplot2)
  library(plotrix)
  library(weights)
  library(mpspline2)
  library(ISRaD)
  library(grid)
  library(gridExtra)
  library(dplyr)

  len_ramp <- which(ramp$temp == 775)

  ### Grab only relevant data with new RPO smoothing outputs
  ramp <- ramp[1:len_ramp,]
  ramp$CO2_prop = ramp$Moving / max(ramp$Moving, na.rm = TRUE)

  par(mfrow = c(1,1))
  ###Thermogram
  #Temperature ramp, with intervals for interpolation = reso (default = 0.1)
  ht = seq(100, 775, by = reso)
  rsp <- spline(ramp$temp, ramp$CO2_prop, xout = ht)

  ### Radiocarbon
  #B spline
  summary(B_fm1 <- lm(bulkFm ~ bs(FmTemp)))
  #Natural cubic spline
  summary(NCub_fm <- lm(bulkFm ~ ns(FmTemp)))

  #Cubic Spline
  CubFit <- interpSpline(bulkFm ~ FmTemp)
  #Linear Interpolation
  FmTempl = c(100, FmTemp, max(ramp$temp)) #Assuming flat 14C change past constraint points
  bulkFml = c(bulkFm[1], bulkFm, bulkFm[length(bulkFm)]) #Build out Fm vector to create flat lines
  Lfit <- approx(FmTempl, bulkFml, n = length(ht))
  #Mass Preserving Spline
  # Fitting mass-preserving spline all the way to the end of the data series weights the first and last fraction unnaturally,
  # so we'll find the temp where 1% & 99% of the C has been released and constrain there
  maxtemp <- max(ramp$temp)

  frac1 = AUC(ramp$temp, ramp$Moving, from = 100, to = mp_temps$Thigh[1], method = 'spline', subdivisions = 2000) / 1000
  tRange = seq(80, mp_temps$Thigh[1])
  a = 0
  for(i in tRange){
    if(a < frac1){
      a <- AUC(ramp$temp, ramp$Moving, from = 100, to = i, method = 'spline', subdivisions = 500)
      temp1 = i
    }
  }

  frac99 =(AUC(ramp$temp, ramp$Moving, from = mp_temps$Tlow[length(mp_temps$Tlow)], to = mp_temps$Thigh[length(mp_temps$Thigh)],
               method = 'spline', subdivisions = 2000)) - (AUC(ramp$temp, ramp$Moving,
                                                               from = mp_temps$Tlow[length(mp_temps$Tlow)], to = mp_temps$Thigh[length(mp_temps$Thigh)],
                                                               method = 'spline', subdivisions = 2000) / 1000)
  tRange = seq(mp_temps$Tlow[length(mp_temps$Tlow)], to = mp_temps$Thigh[length(mp_temps$Thigh)])
  a = 0
  for(i in tRange){
    if(a < frac99){
      a <- AUC(ramp$temp, ramp$Moving, from = mp_temps$Tlow[length(mp_temps$Tlow)], to = i, method = 'spline', subdivisions = 500)
      temp99 = i
    }
  }

  if(temp1 < 100) temp1 = 100
  if(temp99 > 675) temp99 = 675

  mp <- data.frame(Sample = "sample",
                   ltemp = c(temp1, mp_temps[,1][2:length(mp_temps[,1])]),
                   htemp = c(mp_temps[1:length(mp_temps[,2])-1,2], temp99),
                   bulkFm)

  # Old mp, with max and min as default (poor fit of 1st and last fractions with MPspline)
  # mp <- data.frame(Sample = "sample",
  #                  ltemp = c(100, mp_temps[,1][2:length(mp_temps[,1])]),
  #                  htemp = mp_temps[,2],
  #                  bulkFm)

  #Initialize dataframe
  frame_sp <- data.frame(ht, ht, ht, ht, ht, ht, ht)
  colnames(frame_sp) <- c('Temp', 'Area', 'B_Fm', 'Cub_Fm', "L_Fm", "NCub_Fm", "MPS_Fm")
  #Calculate total area under curve
  totAUC <- AUC(ht, rsp$y, method = 'spline', subdivisions = length(ht))

  Area <- ht
  n = 1
  cond = c(rep(TRUE, length(ht)-1), FALSE)
  for(t in ht){
    if(cond[n]){
      Area[n] <- AUC(ht, rsp$y, from = t, to = ht[n+1], method = 'spline') / totAUC
    } else {
      Area[n] <- AUC(ht, rsp$y, from = t, to = ht[n], method = 'spline') / totAUC
    }

    #frame_sp$Cub_Fm[n] <- predict(CubFit, t)$y
    #frame_sp$L_Fm[n] <- Lfit$y[n]
    n = n+1
    if(n %% 2000 == 0) print(paste("Interpolation", round(n/length(ht) * 100, 1), "percent finished."))
  }


  # for(t in ht){
  #   if(n < l_ht){
  #     Area[n] <- AUC(ht, rsp$y, from = t, to = ht[n+1], method = 'spline') / totAUC
  #   } else {
  #     Area[n] <- AUC(ht, rsp$y, from = t, to = ht[n], method = 'spline') / totAUC
  #   }
  #
  #   #frame_sp$Cub_Fm[n] <- predict(CubFit, t)$y
  #   #frame_sp$L_Fm[n] <- Lfit$y[n]
  #   n = n+1
  #   if(n %% 2000 == 0) print(paste("Interpolation", round(n/length(ht) * 100, 1), "percent finished."))
  # }

  frame_sp$Area <- Area

  #frame_sp$Area[length(frame_sp$Area)] = 0
  frame_sp$Cub_Fm <- spline(y = bulkFm, x= FmTemp, method = 'fmm', xout = frame_sp$Temp)$y #Doesn't make much sense for extrapolation but here it is
  frame_sp$L_Fm <- Lfit$y
  frame_sp$B_Fm = predict(B_fm1, data.frame(FmTemp=frame_sp$Temp)) #B spline fit
  #frame_sp$NCub_Fm = predict(NCub_fm, data.frame(FmTemp=frame_sp$Temp)) #Natural Cubic spline fit
  frame_sp$NCub_Fm = spline(y = bulkFm, x= FmTemp, method = 'natural', xout = frame_sp$Temp)$y

  # mpspline can only use integers, so we'll fit a finer spline to it
  mp.sp <- mpspline(mp, d = seq(99, 775))
  #frame_sp$MPS_Fm = approx(seq(100,len_ramp), mp.sp$sample$est_dcm, xout = ht)$y
  #frame_sp$MPS_Fm <- zoo::na.fill(frame_sp$MPS_Fm, fill = frame_sp$MPS_Fm[101]) ### Fill 99-100 with 100-101 values

  frame_sp$MPS_Fm <- na.approx(mp.sp$sample$est_1cm, xout = seq(100, 775,by=reso), rule = 2)

  # plot(seq(1,len_ramp), mp.sp$sample$est_1cm, type = 'l', lwd =3)
  # lines(frame_sp$Temp, frame_sp$MPS_Fm, lwd = 2, col = 'red')

  par(mfrow = c(1,1))

  #Plot 14C interpolations and thermograms
  plot(frame_sp$Temp, frame_sp$Area, type = 'l', lwd = 3, col = 'steelblue',
       xlab = 'Temperature', ylab = 'Total Area',
       main = sample)
  par(new = T)
  plot(frame_sp$Temp, frame_sp$B_Fm, type = 'l', lwd = 1.5, col = 'coral', yaxt = 'n',
       ylab = '', xlab = '',
       ylim = c(0, 1.5))
  lines(frame_sp$Temp, frame_sp$Cub_Fm, col = 'cyan4', lwd = 1.5)
  lines(Lfit, col = 'purple')
  lines(frame_sp$Temp, frame_sp$NCub_Fm, col = 'darkgreen', lwd = 1.5)
  lines(frame_sp$Temp, frame_sp$MPS_Fm, col = "black", lty = 2, lwd =1.5)
  points(FmTemp, bulkFm, pch = 21, bg = 'blue')
  axis(side = 4)
  mtext(side = 4, 'Fm')
  legend('bottomleft', legend = c('B Spline', 'Cubic Spline', 'Linear', "Nat. Cubic", "MP Spline", 'Thermogram',"14C Data"),
         lty = c(1,1,1,1,1,2,NA), col = c('coral', 'cyan4', 'purple','darkgreen', 'black', 'steelblue','black'),
         #bg = c(1,1,1),
         pch = c(NA, NA,NA,NA,NA, NA,21),
         lwd = 2, cex = 0.7)

  #Histograms for outputs
  par(mfrow = c(2,1))
  breaks <- seq(from = 0.0, to = 2, length.out = 1000)

  if(all.plots) hist(frame_sp$L_Fm, main ='Linear Interpolation, Unweighted', freq = FALSE)
  LinHist <- weighted.hist(frame_sp$L_Fm, frame_sp$Area, freq = FALSE, xlim = c(0.5, 1.5),
                           main = 'Linear Interpolation, Weighted', breaks = breaks, plot = ifelse(all.plots, TRUE, FALSE))

  if(all.plots) hist(frame_sp$B_Fm, main = 'B Spline, Unweighted', freq = FALSE)
  BHist <- weighted.hist(frame_sp$B_Fm, frame_sp$Area, freq= FALSE, xlim = c(0.5, 1.5),
                         main = 'B Spline, Weighted', breaks = breaks, plot = ifelse(all.plots, TRUE, FALSE))

  if(all.plots) hist(frame_sp$Cub_Fm, main ='Cubic Spline, Unweighted', freq = FALSE)
  CubHist <- weighted.hist(frame_sp$Cub_Fm, frame_sp$Area, freq = FALSE, xlim = c(0.5, 1.5),
                           main = 'Cubic Spline, Weighted', breaks = breaks, plot = ifelse(all.plots, TRUE, FALSE))

  if(all.plots) hist(frame_sp$NCub_Fm, main = 'Natural Cubic Spline, Unweighted', freq=FALSE)
  NCubHist <- weighted.hist(frame_sp$NCub_Fm, frame_sp$Area, freq = FALSE, xlim = c(0.5, 1.5),
                            main = 'Natural Cubic Spline, Weighted', breaks = breaks, plot = ifelse(all.plots, TRUE, FALSE))

  if(all.plots) hist(frame_sp$MPS_Fm, main = 'Mass-Preserving Spline, Unweighted', freq=FALSE)
  MPHist <- weighted.hist(frame_sp$MPS_Fm, frame_sp$Area, freq = FALSE, xlim = c(0.5, 1.5),
                          main = 'Mass-Preserving Spline, Weighted', breaks = breaks, plot = ifelse(all.plots, TRUE, FALSE))

  if(all.plots){
    par(mfrow = c(1,1))
    plot(ramp$temp, ramp$Moving, type = 'l', lwd = 3, xlim = c(150, len_ramp), main = paste(sample, ":: Mass-preserving spline fit"))
    par(new = T)
    plot(FmTemp, bulkFm, ylim = c(.6, 1.12), axes = F, ann = F, xlim = c(150, len_ramp), pch = 16, col = 'steelblue',cex = 1.5)
    for(i in 1:6){
      x0v = mp_temps[,1][i]
      y0h = 0
      x1b = mp_temps[,2][i]
      y1b = mp[,4][i]
      if(i == 1 ){
        segments(x0 = x1b, y0 = y0h, x1 = x1b, y1 = y1b, col = 'brown2', lwd = 4)
        segments(x0 = 100, y0 = y1b, x1 = x1b, y1 = y1b, col = 'brown2', lwd = 4)
      } else {
        x0h = mp[,3][i-1]
        segments(x0 = x1b, y0 = y0h, x1 = x1b, y1 = y1b, col = 'brown2', lwd = 4)
        segments(x0 = x0h, y0 = y1b, x1 = x1b, y1 = y1b, col = 'brown2', lwd = 4)
        segments(x0 = x0h, y0 = y1b, x1 = x0v, y1 = y0h, col = 'brown2', lwd = 4)
      }
    }
    lines(frame_sp$Temp, frame_sp$MPS_Fm, lty = 2, lwd = 3, col = 'darkgreen')
  }

  ##Plot those distributions
  Cubstats <- c(wtd.quantile(frame_sp$Cub_Fm, weights = frame_sp$Area,
                             normwt = TRUE, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)),
                wmean = wtd.mean(frame_sp$Cub_Fm, weight = frame_sp$Area))
  Linstats <- c(wtd.quantile(frame_sp$L_Fm, weights = frame_sp$Area,
                             normwt = TRUE, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)),
                wmean = wtd.mean(frame_sp$L_Fm, weight = frame_sp$Area))
  Bstats <- c(wtd.quantile(frame_sp$B_Fm, weights = frame_sp$Area,
                           normwt = TRUE, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)),
              wmean = wtd.mean(frame_sp$B_Fm, weight = frame_sp$Area))
  NCubstats <- c(wtd.quantile(frame_sp$NCub_Fm, weights = frame_sp$Area,
                              normwt = TRUE, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)),
                 wmean = wtd.mean(frame_sp$NCub_Fm, weight = frame_sp$Area))
  MPstats <- c(wtd.quantile(frame_sp$MPS_Fm, weights = frame_sp$Area,
                            normwt = TRUE, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)),
               wmean = wtd.mean(frame_sp$MPS_Fm, weight = frame_sp$Area))

  xlim = c(max(min(Bstats, Linstats, Cubstats, NCubstats, MPstats),0.5) - 0.015,
           min(max(Bstats, Linstats, Cubstats, NCubstats, MPstats), 1.5) + 0.015)
  #Linear Interpolation
  colz = c('#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026','#4a1486',"black")
  if(all.plots){
    print(ggplot(data = frame_sp) +
            geom_vline(xintercept = Linstats, lty = 2) + theme_bw() +
            geom_freqpoly(bins = 200, aes(x = L_Fm, y=..density.., weight = Area),
                          colour="black", fill="white") +
            geom_density(aes(x = L_Fm, y=..density.., weight = abs(frame_sp$Area),fill="#FF6666"),alpha = 0.2) +
            xlim(xlim) +
            ggtitle(paste(sample, ':: Linear Spline Distribution')) + xlab('Fraction Modern') +
            geom_label(aes(x = Linstats, y = 0.7), data = data.frame(names(Linstats)), label =  names(Linstats), size = 2))

    #B Spline
    print(ggplot(data = frame_sp) +
            geom_vline(xintercept = Bstats, lty = 2) + theme_bw() +
            geom_freqpoly(bins = 200, aes(x = B_Fm, y=..density.., weight = Area),
                          colour="black", fill="white") +
            geom_density(aes(x = B_Fm,y=..density.., weight = abs(frame_sp$Area),fill="#FFFF66"),alpha = 0.2) +
            xlim(xlim) +
            ggtitle(paste(sample, ':: B Spline Distribution')) + xlab('Fraction Modern') +
            geom_label(aes(x = Bstats, y = 0.7), data = data.frame(names(Bstats)), label =  names(Bstats), size = 2))

    #Cubic Spline
    print(ggplot(data = frame_sp) +
            geom_vline(xintercept = Cubstats, lty = 2) + theme_bw() +
            geom_freqpoly(bins = 200, aes(x = Cub_Fm, y=..density.., weight = Area),
                          colour="black", fill="white") +
            geom_density(aes(x = Cub_Fm,y=..density.., weight = abs(frame_sp$Area),fill="#FF6666"),alpha = 0.2) +
            xlim(xlim) +
            ggtitle(paste(sample, ':: Cubic Spline Distribution')) + xlab('Fraction Modern') +
            geom_label(aes(x = Cubstats, y = 0.7), data = data.frame(names(Cubstats)), label =  names(Cubstats), size = 2))

    #Natural Cubic Spline
    print(ggplot(data = frame_sp) +
            geom_vline(xintercept = NCubstats, lty = 2) + theme_bw() +
            geom_freqpoly(bins = 200, aes(x = NCub_Fm, y=..density.., weight = Area),
                          colour="black", fill="white") +
            geom_density(aes(x = NCub_Fm,y=..density.., weight = abs(frame_sp$Area),fill="#FF6666"),alpha = 0.2) +
            xlim(xlim) + theme_bw() +
            ggtitle(paste(sample, ':: Natural Cubic Spline Distribution')) + xlab('Fraction Modern') +
            geom_label(aes(x = NCubstats, y = 0.7), data = data.frame(names(NCubstats)), label =  names(Cubstats), size = 2))

    #Mass-preserving Spline
    print(ggplot(data = frame_sp) +
            geom_vline(xintercept = MPstats, lty = 2) + theme_bw() +
            geom_freqpoly(bins = 200, aes(x = MPS_Fm, y=..density.., weight = Area),
                          colour="black", fill="white") +
            geom_density(aes(x = MPS_Fm,y=..density.., weight = abs(frame_sp$Area),fill="#FF6666"),alpha = 0.2) +
            xlim(xlim) + theme_bw() +
            ggtitle(paste(sample, ':: Mass-Preserving Spline Distribution')) + xlab('Fraction Modern') +
            geom_label(aes(x = MPstats, y = 0.7), data = data.frame(names(MPstats)), label =  names(MPstats), size = 2))
  }
  #All splines overlain (Density)
  xlim = c(xlim[1] - 0.005, xlim[2] + 0.005)
  bins = 200

  print(ggplot(data=frame_sp) +
          geom_freqpoly(bins = bins, aes(x = L_Fm, y = ..density.., weight = abs(Area),color="#FF6666"),alpha = 1) +
          geom_freqpoly(bins = bins, aes(x = B_Fm, y = ..density.., weight = abs(Area),color="#FFFF66"),alpha = 1) +
          geom_freqpoly(bins = bins, aes(x = Cub_Fm, y = ..density.., weight = abs(Area),color="#FF66FF"),alpha = 1) +
          geom_freqpoly(bins = bins, aes(x = NCub_Fm, y = ..density.., weight = abs(Area),color="#6666FF"),alpha = 1) +
          geom_freqpoly(bins = bins, aes(x = MPS_Fm, y = ..density.., weight = abs(Area),color="steelblue2"),alpha = 1) +
          xlim(xlim) + theme_bw() +
          ggtitle(paste(sample, ':: Weighted Histogram Polygons')) +
          scale_color_discrete(name = "Fit", labels = c("Linear", "Cubic", "B Spline", "Nat. Cubic", "Mass-Preserving")))

  #All splines overlain (Histograms)
  # par(mfrow = c(1,1))
  # plot(FmTemp, bulkFm, pch = 21, bg = 'blue', ylab = 'Fm',
  #      ylim = c(0.5, max(bulkFm, frame_sp$B_Fm, frame_sp$Cub_Fm, frame_sp$L_Fm)),
  #      xlim = c(min(frame_sp$Temp), max(frame_sp$Temp)))
  # par(new = T)
  # plot(frame_sp$Temp, frame_sp$Area, type = 'l', lwd = 3, col = 'steelblue', xlab = '', ylab = '',
  #      main = sample, yaxt = 'n')
  # par(new = T)
  # plot(frame_sp$Temp, frame_sp$B_Fm, type = 'l', lwd = 1.5, col = 'coral', yaxt = 'n', ylab = '', xlab = '',
  #      ylim = c(0.5, max(bulkFm, frame_sp$B_Fm, frame_sp$Cub_Fm, frame_sp$L_Fm)))
  # lines(frame_sp$Temp, frame_sp$Cub_Fm, col = 'cyan4', lwd = 1.5)
  # lines(Lfit, col = 'purple', lwd = 1.5)
  # lines(frame_sp$Temp, frame_sp$NCub_Fm, col = 'darkgreen', lwd = 1.5)
  # lines(frame_sp$Temp, frame_sp$MPS_Fm, col = 'black', lty = 2, lwd = 1.5)
  # axis(side = 4)
  # mtext(side = 2, 'Fm')
  # legend('topright', legend = c('B Spline', 'Cubic Spline', 'Linear', "Nat. Cubic", "MP Spline", 'Thermogram',"14C Data"),
  #        lty = c(1,1,1,1,2,1,NA), col = c('coral', 'cyan4', 'purple','darkgreen', 'black', 'steelblue','black'),
  #        #bg = c(1,1,1),
  #        pch = c(NA, NA,NA,NA,NA, NA,21),
  #        lwd = 2, cex = 0.7)

  print(ggplot(data=frame_sp) +
          geom_density(aes(x = L_Fm, y = ..density.., weight = abs(Area),color="#FF6666"),alpha = 0.5, lwd =1.2) +
          geom_density(aes(x = B_Fm, y = ..density.., weight = abs(Area),color="#FFFF66"),alpha = 0.5, lwd =1.2) +
          geom_density(aes(x = Cub_Fm, y = ..density.., weight = abs(Area),color="#FF66FF"),alpha = 0.5, lwd = 1.2) +
          geom_density(aes(x = NCub_Fm, y = ..density.., weight = abs(Area),color="#6666FF"),alpha = 0.5, lwd = 1.2) +
          geom_density(aes(x = MPS_Fm, y = ..density.., weight = abs(Area),color="steelblue2"),alpha = 0.5, lwd = 1.2) +
          xlim(xlim) + xlab("Fm") + theme_bw() +
          ggtitle(paste(sample, ':: Weighted Densities')) +
          scale_color_discrete(name = "Fit", labels = c("Linear", "Cubic", "B Spline", "Nat. Cubic", "MP Spline")))

  # print("Linear, B, Cubic")
  # print(Linstats)
  # print(Bstats)
  # print(Cubstats)
  return(list(data.frame(sample, Linstats, Bstats, Cubstats, NCubstats, MPstats),
              data.frame(sample, MidFm = breaks,
                         Lin = c(0,LinHist$density),
                         B = c(0,BHist$density),
                         Cub = c(0,CubHist$density),
                         NCub = c(0,NCubHist$density),
                         MPS = c(0,MPHist$density),
                         LinDens = density(LinHist$density, n = 1000)$y,
                         BDens = density(BHist$density, n = 1000)$y,
                         CubDens = density(CubHist$density, n = 1000)$y,
                         NCubDens = density(NCubHist$density, n = 1000)$y,
                         MPSDens = density(MPHist$density, n = 1000)$y
              ),
              data.frame(Area = frame_sp$Area,
                         L_Fm = frame_sp$L_Fm,
                         B_Fm = frame_sp$B_Fm,
                         Cub_Fm = frame_sp$Cub_Fm,
                         NCub_Fm = frame_sp$NCub_Fm,
                         MPS_Fm = frame_sp$MPS_Fm)))
}
