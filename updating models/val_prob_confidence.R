#' Calibration performance
#'
#' The function val_prob_confidence is an adaptation of val.prob from Frank Harrell's rms package and the val.prob.ci.2 function
#' from Van Calster et al's CalibrationCurves package. Most of the code and the description are therefore copied from these
#' functions. Parameter definitions are also similar to those in the two existing functions.
#' 
val_prob_confidence <- function(p, y, logit, group, weights = rep(1, length(y)), normwt = F, pl = T,
                                smooth = c("loess","rcs",F), CL.smooth="fill",CL.BT=F,lty.smooth=1,col.smooth="black",lwd.smooth=1,
                                nr.knots=5,logistic.cal = F,lty.log=1,col.log="black",lwd.log=1, xlab = "Predicted probability", ylab =
                                  "Observed proportion", xlim = c(-0.02, 1),ylim = c(-0.15,1), m, g, cuts, emax.lim = c(0, 1),
                                legendloc =  c(0.50 , 0.27), statloc = c(0,.85),dostats=T,cl.level=0.95,roundstats=2,
                                riskdist = "predicted", cex=0.75,cex.leg = 0.75, connect.group =
                                  F, connect.smooth = T, g.group = 4, evaluate = 100, nmin = 0, d0lab="0", d1lab="1", cex.d01=0.7,
                                dist.label=0.04, line.bins=-.05, dist.label2=.03, cutoff, las=1, length.seg=1,
                                y.intersp=1,lty.ideal=1,col.ideal="red",lwd.ideal=1,...)
{
  if(smooth[1]==F){smooth <- "F"}
  smooth <- match.arg(smooth)
  if(!missing(p))
    if(any(!(p>=0 | p<=1))){stop("Probabilities can not be > 1 or < 0.")}
  if(missing(p))
    p <- plogis(logit)
  else logit <- qlogis(p)
  if(!all(y%in%0:1)){stop("The vector with the binary outcome can only contain the values 0 and 1.")}
  if(length(p) != length(y))
    stop("lengths of p or logit and y do not agree")
  names(p) <- names(y) <- names(logit) <- NULL
  if(!missing(group)) {
    if(length(group) == 1 && is.logical(group) && group)
      group <- rep("", length(y))
    if(!is.factor(group))
      group <- if(is.logical(group) || is.character(group))
        as.factor(group) else cut2(group, g =
                                     g.group)
    names(group) <- NULL
    nma <- !(is.na(p + y + weights) | is.na(group))
    ng <- length(levels(group))
  }
  else {
    nma <- !is.na(p + y + weights)
    ng <- 0
  }
  logit <- logit[nma]
  y <- y[nma]
  p <- p[nma]
  if(ng > 0) {
    group <- group[nma]
    weights <- weights[nma]
    return(val.probg(p, y, group, evaluate, weights, normwt, nmin)
    )
  }
  
  # Sort vector with probabilities
  y     <- y[order(p)]
  logit <- logit[order(p)]
  p     <- p[order(p)]
  
  
  if(length(p)>5000 & smooth=="loess"){warning("Number of observations > 5000, RCS is recommended.",immediate. = T)}
  if(length(p)>1000 & CL.BT==T){warning("Number of observations is > 1000, this could take a while...",immediate. = T)}
  
  
  if(length(unique(p)) == 1) {
    P <- mean(y)
    Intc <- qlogis(P)
    n <- length(y)
    D <- -1/n
    L01 <- -2 * sum(y * logit - log(1 + exp(logit)), na.rm = T)
    L.cal <- -2 * sum(y * Intc - log(1 + exp(Intc)), na.rm = T)
    U.chisq <- L01 - L.cal
    U.p <- 1 - pchisq(U.chisq, 1)
    U <- (U.chisq - 1)/n
    Q <- D - U
    
    stats <- c(0, 0.5, 0, D, 0, 1, U, U.chisq, U.p, Q, mean((y - p[
      1])^2), Intc, 0, 0, 0, rep(abs(p[1] - P), 2))
    names(stats) <- c("Dxy", "C (ROC)", "R2", "D", "D:Chi-sq",
                      "D:p", "U", "U:Chi-sq", "U:p", "Q", "Brier",
                      "Intercept", "SE(intercept)", "Slope", "SE(slope)", "Emax", "Eavg", "ECI")
    return(stats)
  }
  i <- !is.infinite(logit)
  nm <- sum(!i)
  if(nm > 0)
    warning(paste(nm, "observations deleted from logistic calibration due to probs. of 0 or 1"))
  f.fixed <- lrm.fit(logit[i], y[i], initial=c(0., 1.), maxit=1L)
  f.recal <- lrm.fit(logit[i], y[i])
  
  se.slope <- sqrt(vcov(f.recal)[2,2])
  se.intc <- sqrt(vcov(f.recal)[1,1])
  
  cl.slope <- confint(f.recal, level = cl.level)[2,]
  cl.interc <- confint(f.recal, level = cl.level)[1,]
  
  stats <- f.fixed$stats
  n <- stats["Obs"]
  predprob <- seq(emax.lim[1], emax.lim[2], by = 0.0005)
  Sm <- lowess(p, y, iter=0)
  cal.smooth <- approx(Sm, xout=p, ties=mean)$y
  er   <- abs(p - cal.smooth)
  eavg <- mean(er)
  emax <- max(er)
  e90  <- unname(quantile(er, 0.9))
  
  if (pl) {
    plot(0.5, 0.5, xlim = xlim, ylim = ylim, type = "n", xlab = xlab,
         ylab = ylab, las=las,...)
    clip(0,1,0,1)
    abline(0, 1, lty = lty.ideal,col=col.ideal,lwd=lwd.ideal)
    do.call("clip", as.list(par()$usr))
    
    
    lt <- lty.ideal
    lw.d <- lwd.ideal
    all.col <- col.ideal
    leg <- "Ideal"
    marks <- -1
    if (logistic.cal) {
      lt <- c(lt, lty.log)
      lw.d <- c(lw.d,lwd.log)
      all.col <- c(all.col,col.log)
      leg <- c(leg, "Logistic calibration")
      marks <- c(marks, -1)
    }
    if(smooth!="F"){all.col <- c(all.col,col.smooth)}
    if (smooth=="loess") {
      #Sm <- lowess(p,y,iter=0)
      Sm <- loess(y~p,degree=2)
      Sm <- data.frame(Sm$x,Sm$fitted); Sm.01 <- Sm
      
      if (connect.smooth==T & CL.smooth!="fill") {
        clip(0,1,0,1)
        lines(Sm, lty = lty.smooth,lwd=lwd.smooth,col=col.smooth)
        do.call("clip", as.list(par()$usr))
        lt <- c(lt, lty.smooth)
        lw.d <- c(lw.d,lwd.smooth)
        marks <- c(marks, -1)
      }else if(connect.smooth==F & CL.smooth!="fill"){
        clip(0,1,0,1)
        points(Sm,col=col.smooth)
        do.call("clip", as.list(par()$usr))
        lt <- c(lt, 0)
        lw.d <- c(lw.d,1)
        marks <- c(marks, 1)
      }
      if(CL.smooth==T | CL.smooth=="fill"){
        to.pred <- seq(min(p),max(p),length=200)
        if(CL.BT==T){
          cat("Bootstrap samples are being generated.\n\n\n")
          
          replicate(2000,BT.samples(y,p,to.pred)) -> res.BT
          apply(res.BT,1,quantile,c(0.025,0.975)) -> CL.BT
          colnames(CL.BT) <- to.pred
          
          if(CL.smooth=="fill"){
            clip(0,1,0,1)
            polygon(x = c(to.pred, rev(to.pred)), y = c(CL.BT[2,],
                                                        rev(CL.BT[1,])),
                    col = rgb(177, 177, 177, 177, maxColorValue = 255), border = NA)
            if (connect.smooth==T) {
              lines(Sm, lty = lty.smooth,lwd=lwd.smooth,col=col.smooth)
              lt <- c(lt, lty.smooth)
              lw.d <- c(lw.d,lwd.smooth)
              marks <- c(marks, -1)
            }else if(connect.smooth==F){
              points(Sm,col=col.smooth)
              lt <- c(lt, 0)
              lw.d <- c(lw.d,1)
              marks <- c(marks, 1)
            }
            do.call("clip", as.list(par()$usr))
            leg <- c(leg, "Flexible calibration (Loess)")
          }else{
            
            clip(0,1,0,1)
            lines(to.pred,CL.BT[1,],lty=2,lwd=1,col=col.smooth);clip(0,1,0,1);lines(to.pred,CL.BT[2,],lty=2,lwd=1,col=col.smooth)
            do.call("clip", as.list(par()$usr))
            leg <- c(leg,"Flexible calibration (Loess)","CL flexible")
            lt <- c(lt,2)
            lw.d <- c(lw.d,1)
            all.col <- c(all.col,col.smooth)
            marks <- c(marks,-1)
          }
          
        }else{
          Sm.0 <- loess(y~p,degree=2)
          predict(Sm.0,type="fitted",se=T) -> cl.loess
          clip(0,1,0,1)
          if(CL.smooth=="fill"){
            polygon(x = c(Sm.0$x, rev(Sm.0$x)), y = c(cl.loess$fit+cl.loess$se.fit*1.96,
                                                      rev(cl.loess$fit-cl.loess$se.fit*1.96)),
                    col = rgb(177, 177, 177, 177, maxColorValue = 255), border = NA)
            if (connect.smooth==T) {
              lines(Sm, lty = lty.smooth,lwd=lwd.smooth,col=col.smooth)
              lt <- c(lt, lty.smooth)
              lw.d <- c(lw.d,lwd.smooth)
              marks <- c(marks, -1)
            }else if(connect.smooth==F){
              points(Sm,col=col.smooth)
              lt <- c(lt, 0)
              lw.d <- c(lw.d,1)
              marks <- c(marks, 1)
            }
            do.call("clip", as.list(par()$usr))
            leg <- c(leg, "Flexible calibration (Loess)")
          }else{
            lines(Sm.0$x,cl.loess$fit+cl.loess$se.fit*1.96,lty=2,lwd=1,col=col.smooth)
            lines(Sm.0$x,cl.loess$fit-cl.loess$se.fit*1.96,lty=2,lwd=1,col=col.smooth)
            do.call("clip", as.list(par()$usr))
            leg <- c(leg,"Flexible calibration (Loess)","CL flexible")
            lt <- c(lt,2)
            lw.d <- c(lw.d,1)
            all.col <- c(all.col,col.smooth)
            marks <- c(marks,-1)
          }
          
        }
        
      }else{
        leg <- c(leg, "Flexible calibration (Loess)")}
      cal.smooth <- approx(Sm.01, xout = p)$y
      eavg <- mean(abs(p - cal.smooth))
      ECI <- mean((p-cal.smooth)^2)*100
    }
    if(smooth=="rcs"){
      par(lwd=lwd.smooth,bty="n",col=col.smooth)
      if(!is.numeric(nr.knots)){stop("Nr.knots must be numeric.")}
      if(nr.knots==5){
        tryCatch(rcspline.plot(p,y,model="logistic",nk=5,show="prob", statloc = "none"
                               ,add=T,showknots=F,xrange=c(min(na.omit(p)),max(na.omit(p))),lty=lty.smooth),error=function(e){
                                 warning("The number of knots led to estimation problems, nk will be set to 4.",immediate. = T)
                                 tryCatch(rcspline.plot(p,y,model="logistic",nk=4,show="prob", statloc = "none"
                                                        ,add=T,showknots=F,xrange=c(min(na.omit(p)),max(na.omit(p))),lty=lty.smooth)
                                          ,error=function(e){
                                            warning("Nk 4 also led to estimation problems, nk will be set to 3.",immediate.=T)
                                            rcspline.plot(p,y,model="logistic",nk=3,show="prob", statloc = "none"
                                                          ,add=T,showknots=F,xrange=c(min(na.omit(p)),max(na.omit(p)))
                                                          ,lty=lty.smooth)
                                          })
                               })
      }else if(nr.knots==4){
        tryCatch(rcspline.plot(p,y,model="logistic",nk=4,show="prob", statloc = "none"
                               ,add=T,showknots=F,xrange=c(min(na.omit(p)),max(na.omit(p))),lty=lty.smooth),error=function(e){
                                 warning("The number of knots led to estimation problems, nk will be set to 3.",immediate.=T)
                                 rcspline.plot(p,y,model="logistic",nk=3,show="prob", statloc = "none"
                                               ,add=T,showknots=F,xrange=c(min(na.omit(p)),max(na.omit(p))),lty=lty.smooth)
                               })
      }else if(nr.knots==3){
        tryCatch(rcspline.plot(p,y,model="logistic",nk=3,show="prob", statloc = "none"
                               ,add=T,showknots=F,xrange=c(min(na.omit(p)),max(na.omit(p))),lty=lty.smooth),
                 error=function(e){
                   stop("Nk = 3 led to estimation problems.")
                 })
      }else{stop(paste("Number of knots = ",nr.knots,sep="", ", only 5 >= nk >=3 is allowed."))}
      
      par(lwd=1,bty="o",col="black")
      leg <- c(leg,"Flexible calibration (RCS)","CL flexible")
      lt <- c(lt,lty.smooth,2)
      lw.d <- c(lw.d,rep(lwd.smooth,2))
      all.col <- c(all.col,col.smooth)
      marks <- c(marks,-1,-1)
    }
    if(!missing(m) | !missing(g) | !missing(cuts)) {
      if(!missing(m))
        q <- cut2(p, m = m, levels.mean = T, digits = 7)
      else if(!missing(g))
        q <- cut2(p, g = g, levels.mean = T, digits = 7)
      else if(!missing(cuts))
        q <- cut2(p, cuts = cuts, levels.mean = T, digits = 7)
      means <- as.single(levels(q))
      prop <- tapply(y, q, function(x)mean(x, na.rm = T))
      points(means, prop, pch = 2, cex=1)
      ng	<-tapply(y, q, length)
      og	<-tapply(y, q, sum)
      ob	<-og/ng
      se.ob	<-sqrt(ob*(1-ob)/ng)
      g		<- length(as.single(levels(q)))
      
      for (i in 1:g) lines(c(means[i], means[i]), c(prop[i],min(1,prop[i]+1.96*se.ob[i])), type="l")
      for (i in 1:g) lines(c(means[i], means[i]), c(prop[i],max(0,prop[i]-1.96*se.ob[i])), type="l")
      
      if(connect.group) {
        lines(means, prop)
        lt <- c(lt, 1)
        lw.d <- c(lw.d,1)
      }
      else {
        lt <- c(lt, 0)
        lw.d <- c(lw.d, 0)
      }
      leg <- c(leg, "Grouped observations")
      all.col <- c(all.col, col.smooth)
      marks <- c(marks, 2)
    }
  }
  lr <- stats["Model L.R."]
  p.lr <- stats["P"]
  D <- (lr - 1)/n
  L01 <- -2 * sum(y * logit - logb(1 + exp(logit)), na.rm = TRUE)
  #U.chisq <- L01 - f$deviance[2]
  U.chisq <- L01 - f.recal$deviance[2]
  p.U <- 1 - pchisq(U.chisq, 2)
  U <- (U.chisq - 2)/n
  Q <- D - U
  Dxy <- stats["Dxy"]
  C <- stats["C"]
  R2 <- stats["R2"]
  B <- sum((p - y)^2)/n
  Bmax  <- mean(y) * (1-mean(y))^2 + (1-mean(y)) * mean(y)^2
  Bscaled <- 1 - B/Bmax
  stats <- c(Dxy, C, R2, D, lr, p.lr, U, U.chisq, p.U, Q, B,
             f.recal$coef[1], se.intc, f.recal$coef[2], se.slope, emax, Bscaled)
  names(stats) <- c("Dxy", "C (ROC)", "R2", "D", "D:Chi-sq",
                    "D:p", "U", "U:Chi-sq", "U:p", "Q", "Brier", "Intercept", "SE(intercept)",
                    "Slope", "SE(slope)", "Emax", "Brier scaled")
  if(smooth=="loess")
    stats <- c(stats, c(Eavg = eavg),c(ECI = ECI))
  
  # Cut off definition
  if(!missing(cutoff)) {
    arrows(x0=cutoff,y0=.1,x1=cutoff,y1=-0.025,length=.15)
  }
  if(pl) {
    prob <- 1/(1 + exp( - logit))
    pred.prob <- f.recal$coef[1] + f.recal$coef[2] * logit
    pred.prob <- 1/(1 + exp( - pred.prob))
    if(logistic.cal) lines(prob, pred.prob, lty=lty.log,lwd=lwd.log,col=col.log)
    }
    lp <- legendloc
    if (!is.logical(lp)) {
      if (!is.list(lp))
        lp <- list(x = lp[1], y = lp[2])
      legend(lp, leg, lty = lt, pch = marks, cex = cex.leg, bty = "n",lwd=lw.d,
             col=all.col,y.intersp = y.intersp)
    }
    if(!is.logical(statloc)) {
      if(dostats[1]==T){
        stats.2 <- paste('Calibration\n',
                         '...intercept: '
                         , sprintf(paste("%.",roundstats,"f",sep=""), stats["Intercept"]), " (",
                         sprintf(paste("%.",roundstats,"f",sep=""),cl.interc[1])," to ",
                         sprintf(paste("%.",roundstats,"f",sep=""),cl.interc[2]),")",'\n',
                         '...slope: '
                         , sprintf(paste("%.",roundstats,"f",sep=""), stats["Slope"]), " (",
                         sprintf(paste("%.",roundstats,"f",sep=""),cl.slope[1])," to ",
                         sprintf(paste("%.",roundstats,"f",sep=""),cl.slope[2]),")",'\n',
                         sep = ''
                         )
        text(statloc[1], statloc[2],stats.2,pos=4,cex=cex)
        
      }else{
        dostats <- dostats
        leg <- format(names(stats)[dostats])	#constant length
        leg <- paste(leg, ":", format(stats[dostats], digits=roundstats), sep =
                       "")
        if(!is.list(statloc))
          statloc <- list(x = statloc[1], y = statloc[2])
        text(statloc, paste(format(names(stats[dostats])),
                            collapse = "\n"), adj = 0, cex = cex)
        text(statloc$x + (xlim[2]-xlim[1])/3 , statloc$y, paste(
          format(round(stats[dostats], digits=roundstats)), collapse =
            "\n"), adj = 1, cex = cex)
      }
    }
    if(is.character(riskdist)) {
      if(riskdist == "calibrated") {
        x <- f.recal$coef[1] + f.recal$coef[2] * log(p/(1 - p))
        x <- 1/(1 + exp( - x))
        x[p == 0] <- 0
        x[p == 1] <- 1
      }
      else x <- p
      bins <- seq(0, min(1,max(xlim)), length = 101)
      x <- x[x >= 0 & x <= 1]
      f0	<-table(cut(x[y==0],bins))
      f1	<-table(cut(x[y==1],bins))
      j0	<-f0 > 0
      j1	<-f1 > 0
      bins0 <-(bins[-101])[j0]
      bins1 <-(bins[-101])[j1]
      f0	<-f0[j0]
      f1	<-f1[j1]
      maxf <-max(f0,f1)
      f0	<-(0.1*f0)/maxf
      f1	<-(0.1*f1)/maxf
      
      segments(bins1,line.bins,bins1,length.seg*f1+line.bins)
      segments(bins0,line.bins,bins0,length.seg*-f0+line.bins)
      lines(c(min(bins0,bins1)-0.01,max(bins0,bins1)+0.01),c(line.bins,line.bins))
      text(max(bins0,bins1)+dist.label,line.bins+dist.label2,d1lab,cex=cex.d01)
      text(max(bins0,bins1)+dist.label,line.bins-dist.label2,d0lab,cex=cex.d01)
      
    }
  stats
}
