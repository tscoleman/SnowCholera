#author: Thomas Coleman
# Difference in Differences Regressions - Error Analysis and Graphs for data from Snow 1855

# See "Causality in the Time of Cholera" working paper at https://papers.ssrn.com/abstract=3262234 
# and my John Snow project website, http://www.hilerun.org/econ/papers/snow

# This code is licensed under the BSD 2-Clause License, https://opensource.org/licenses/BSD-2-Clause

# This collect together some simple functions that produce a standard format graph, and are 'source'd 
# into various notebooks

# **preperrdata** Before graphing, to prepare the data

#  * Takes in *fittedmodel* - a regression that has been already run. From this it extracts the necessary 
#  parameters. Also *single*, a string which for "single" says that there is a single treatment effect. 
#  * Calculates the 1849 and 1854 predicted counts and rates
#  * Calculates *approximate* 95% error bars around the predicted rates, based on whether the fitted model 
#  is Poisson or Negative Binomial
#  * Produces an adjusted 1854 predicted rate, adjusting for the 1854 time effect and treatment effect, so 
#  that it is comparable to the 1849 predicted rate (for purposes of plotting with error bars)

#  This function changes global data (the x1849 & x1854 dataframes) using the "<<-" instead of "<-" 
#  assignment. This is poor programming style but I could not find another easy way of doing what I wanted. 

# **plot2_worker** Plots actual vs predicted, with error bars around the predicted

#  * Actually does the plotting, given all the data as input arguments (sequence no. for sub-districts; 
#  the actual mean or rate; predicted; the 2.5% and 97.5% points; title)
#  * btw, the hack for plotting error bars is from 
#  https://stackoverflow.com/questions/13032777/scatter-plot-with-error-bars

# **plot2** Is a cover function which unpacks the actual versus predicted mean from the appropriate dataframe

# **plot3** Plots actual 1849, 1854 (adjusted for time & treatment effects), predicted, with error bars

# **plotcomp** Plots actual 1849 versus 1854, with error bars around actual 1849

# **ploterrbars** is NOT a function you should use - I use it to print out .pdf versions of the graphs I want to use



# 22-jul-2020 modify to create "adjusted rates" for both 1849 and 1854 

preperrdata <- function(fittedmodel,single = "single",link="log",conf=.95,population="population") {       # This is not a good way to do this because I am 
	                                                  # changing globals from within the function (using <<- 
	                                                  # instead of <-)
# Function to prepare the data (error bars) for graphing
# The 2.5% and 97.5% confidence bands are calculated assuming either Poisson or Negative Binomial
# The rates are calculated by generating the counts up and down from the "expected" (from the fitted model)

	# Set the confidence levels (inputting 0.95 means lower leve .025, upper .975, total .05 / .95)
	xconfidl <- (1-conf)/2
	xconfidu <- 1-xconfidl
	expected <- predict(fittedmodel)
	xfamily <- family(fittedmodel)$family
	if (link == "log") {
		expected <- exp(expected)       # expected values
	}
	if (substr(xfamily,1,8) == "gaussian") {   # For OLS "rate" model need to convert from rates to counts
		expected[1:28] <- expected[1:28] * x1849[,population]
		expected[29:56] <- expected[29:56] * x1854[,population]
	}
	theta <- fittedmodel$theta
	x1849$predcount <<- expected[1:28]
	x1854$predcount <<- expected[29:56]
	x1849$predrate <<- 10000 * expected[1:28] / x1849[,population]
	x1854$predrate <<- 10000 * expected[29:56] / x1854[,population]
	if (xfamily == "poisson") {         # Get 95% confidence bands depending on model used (Poiss vs Neg Binom)
		x1849$limdn <<- 10000 * qpois(xconfidl,lambda=x1849$predcount) / x1849[,population]
		x1849$limup <<- 10000 * qpois(xconfidu,lambda=x1849$predcount) / x1849[,population]
		x1854$limdn <<- 10000 * qpois(xconfidl,lambda=x1854$predcount) / x1854[,population]
		x1854$limup <<- 10000 * qpois(xconfidu,lambda=x1854$predcount) / x1854[,population]
		x1849$limdnact <<- 10000 * qpois(xconfidl,lambda=x1849$deaths) / x1849[,population]
		x1849$limupact <<- 10000 * qpois(xconfidu,lambda=x1849$deaths) / x1849[,population]
	} else if (substr(xfamily,1,8) == "Negative"){
		x1849$limdn <<- 10000 * qnbinom(xconfidl,size=theta,mu=x1849$predcount) / x1849[,population]
		x1849$limup <<- 10000 * qnbinom(xconfidu,size=theta,mu=x1849$predcount) / x1849[,population]
		x1854$limdn <<- 10000 * qnbinom(xconfidl,size=theta,mu=x1854$predcount) / x1854[,population]
		x1854$limup <<- 10000 * qnbinom(xconfidu,size=theta,mu=x1854$predcount) / x1854[,population]
		x1849$limdnact <<- 10000 * qnbinom(xconfidl,size=theta,mu=x1849$deaths) / x1849[,population]
		x1849$limupact <<- 10000 * qnbinom(xconfidu,size=theta,mu=x1849$deaths) / x1849[,population]
	} else if (substr(xfamily,1,8) == "gaussian"){
		x3 <- predict(fittedmodel,interval="prediction")   # I think "confidence" is what I want and not "prediction"
		x1849$limdn <<- x3[1:28,2]
		x1849$limup <<- x3[1:28,3]
		x1854$limdn <<- x3[29:56,2]
		x1854$limup <<- x3[29:56,3]
		if (link == "log") {    # There is no "link" item for lm, so the only way I can tell if this model is run
		                      # in level or log form is to look at the predicted, and if it is negative presume it's in logs
			x1849$limdn <<- exp(x1849$limdn)
			x1849$limup <<- exp(x1849$limup)
			x1854$limdn <<- exp(x1854$limdn)
			x1854$limup <<- exp(x1854$limup)
		}
		x1849$limdn <<- 10000 * x1849$limdn
		x1849$limup <<- 10000 * x1849$limup
		x1854$limdn <<- 10000 * x1854$limdn
		x1854$limup <<- 10000 * x1854$limup
	}


	# Now adjust the 1854 predicted counts (and rates) for the time and treatment fixed effects -
	# effectively netting out the 1854 effect and making it 1849-equivalent
	# Adjust for the year effect only - this should make 1849 & 1854 comparable net of time effect
	if (link == "log") {
		xyr1854 <- (coef(fittedmodel)["year1854"])
		x49 <- exp(xyr1854)
		x54 <- exp(-xyr1854)
		x1849$rateadjyr <<- 10000 * (x1849$deaths*x49) / x1849[,population]
		x1854$rateadjyr <<- 10000 * (x1854$deaths*x54) / x1854[,population]
	} else {
		x49 <- (coef(fittedmodel)["year1854"])
		x54 <- -(coef(fittedmodel)["year1854"])
		x1849$rateadjyr <<- x1849$rate + x49
		x1854$rateadjyr <<- x1854$rate + x54
	}
	# Adjust the 1854 actual for the estimated Treatment Effect and the estimated Year Effect
	if (single == "single")  {   # model with single treatment effect
		xdegless <- (coef(fittedmodel)["supplierSouthwarkVauxhall_Lambeth:year1854"])
		xdegmore <- xdegless               # Make both treatment effects the same
		if (link == "log") {
			x49 <- x49 * exp((x1849$lambethdegree == "less_Lambeth")*xdegless) 
			x49 <- x49 * exp((x1849$lambethdegree == "more_Lambeth")*xdegmore) 
			x54 <- x54 * exp(-(x1854$lambethdegree == "less_Lambeth")*xdegless) 
			x54 <- x54 * exp(-(x1854$lambethdegree == "more_Lambeth")*xdegmore)
			x1849$rateadj <<- 10000 * (x1849$deaths*x49) / x1849[,population]
			x1854$rateadj <<- 10000 * (x1854$deaths*x54) / x1854[,population]
		} else {
			x49 <- x49 + (x1849$lambethdegree == "less_Lambeth")*xdegless
			x54 <- x49 + (x1849$lambethdegree == "more_Lambeth")*xdegmore 
			x54 <- x54 -(x1854$lambethdegree == "less_Lambeth")*xdegless
			x54 <- x54 -(x1854$lambethdegree == "more_Lambeth")*xdegmore
			x1849$rateadj <<- x1849$rate + x49
			x1854$rateadj <<- x1854$rate + x54
		}
	} else if (single == "two") {      # model with two treatment effects          		
		xdegless <- coef(fittedmodel)["lambethdegreeless_Lambeth:year1854"]
		xdegmore <- coef(fittedmodel)["lambethdegreemore_Lambeth:year1854"]
		if (link == "log") {
			x49 <- x49 * exp((x1849$lambethdegree == "less_Lambeth")*xdegless) 
			x49 <- x49 * exp((x1849$lambethdegree == "more_Lambeth")*xdegmore) 
			x54 <- x54 * exp(-(x1854$lambethdegree == "less_Lambeth")*xdegless) 
			x54 <- x54 * exp(-(x1854$lambethdegree == "more_Lambeth")*xdegmore)
			x1849$rateadj <<- 10000 * (x1849$deaths*x49) / x1849[,population]
			x1854$rateadj <<- 10000 * (x1854$deaths*x54) / x1854[,population]
		} else {
			x49 <- x49 + (x1849$lambethdegree == "less_Lambeth")*xdegless
			x54 <- x49 + (x1849$lambethdegree == "more_Lambeth")*xdegmore 
			x54 <- x54 -(x1854$lambethdegree == "less_Lambeth")*xdegless
			x54 <- x54 -(x1854$lambethdegree == "more_Lambeth")*xdegmore
			x1849$rateadj <<- x1849$rate + x49
			x1854$rateadj <<- x1854$rate + x54
		}
	} else { # model with continuous treatment (population proportions)
		xperc_lambeth54 <- coef(fittedmodel)["perc_lambeth54"]
		if (link == "log") {
			x49 <-  exp((x1849$perc_lambeth * xperc_lambeth54)) / x49
			x54 <- x54 * exp(-(x1854$perc_lambeth * xperc_lambeth54))
			x1849$rateadj <<- 10000 * (x1849$deaths*x49) / x1849[,population]
			x1854$rateadj <<- 10000 * (x1854$deaths*x54) / x1854[,population]
		} else {
			x49 <- (x1849$perc_lambeth * xperc_lambeth54) - x49
			x54 <- x54 -(x1854$perc_lambeth * xperc_lambeth54)
			x1849$rateadj <<- x1849$rate + x49
			x1854$rateadj <<- x1854$rate + x54
		}
	}

	return(xfamily)
}


# "Worker" function to plot mean, predicted, and error bars
plot2_worker <- function(yseq, xmean,xlimdn,xpred,xlimup,title,legposition="bottomright") {            
	xplot <- plot(xmean, yseq,
	    xlim=range(c(xmean, xpred,xlimdn,xlimup)),
	    ylim=rev(range(yseq)), col="red",
	    main=title,xlab="Mortality rate actual (red filled) vs predicted (empty circle)",ylab="sub-district",
	    pch=19)
	lines(xpred, yseq, type="p",
	    xlim=range(c(xmean, xpred,xlimdn,xlimup)),
	    ylim=rev(range(yseq)), 
	    pch=1)
	# horizontal error bars
	xplot <- arrows(xlimdn, yseq, xlimup, yseq, length=0.05, angle=90, code=3,lty=3)
# Ooops - this legend is not for "plot2" but for "plotcomp"
#	xplot <- legend(legposition, c("1849 red circle", "1854 blue triangle"), col = c(2, 4),
#       text.col = c("red","blue"), lty = c(0, 0), pch = c(19, 17),
#       merge = TRUE,bty="o",bg="white")    # The bty="o" should overwrite the background


	xplot
}
# "Cover" function that takes in the dataframe and unpacks
plot2 <- function(confidata,xsupplier,title,legposition="bottomright") {            
	xmean <- subset(confidata,supplier==xsupplier)[,"rate"]
	xlimdn <- subset(confidata,supplier==xsupplier)[,"limdn"]
	xlimup <- subset(confidata,supplier==xsupplier)[,"limup"]
	xpred <- subset(confidata,supplier==xsupplier)[,"predrate"]
	yseq <- subset(confidata,supplier==xsupplier)[,"seq"]
#	xtitle <- paste(xsupplier,title)
	xplot <- plot2_worker(yseq,xmean,xlimdn,xpred,xlimup,title,legposition)
}


# 29-jul-20 Split out "plot3_worker"
plot3_worker <- function(yseq,xmean, xpred,xlimdn,xlimup,x1854adj,title,legposition="bottomright") {
	xplot <- plot(xmean, yseq,
	    xlim=range(c(xmean, xpred,xlimdn,xlimup,x1854adj)),
	    ylim=rev(range(yseq)), col="red", 
	    main=title,xlab="Mortality per 10,000 population",ylab="sub-district",
	    pch=19)
	lines(xpred, yseq, type="p",
	    xlim=range(c(xmean, xpred,xlimdn,xlimup)),
	    ylim=rev(range(yseq)), 
	    pch=1)
	lines(x1854adj, yseq, type="p",
	    xlim=range(c(xmean, xpred,xlimdn,xlimup)),
	    ylim=rev(range(yseq)), col="blue", 
	    pch=17)

	# horizontal error bars
	xplot <- arrows(xlimdn, yseq, xlimup, yseq, length=0.05, angle=90, code=3,lty=3)
	xplot <- legend(legposition, c("1849 red circle", "1854 blue triangle", "predicted"), col = c(2, 4, 1),
       text.col = c("red","blue","black"), lty = c(0, 0, 3), pch = c(19, 17, 1),
       merge = TRUE,bty="o",bg="white")    # The bty="o" should overwrite the background
}


# 26-feb-20 add (optional) argument "rateadj" which can be set to either
#  - "rateadj" = 1854 rate adjusted for both year effect and treatment effect
#  - "rateadjyr" = 1854 adjusted only for year effect (not treaement effect)
# 22-jul-20 add (optional) argument "yearadj" which can be set to either 1849 or 1854
#  - "1849" - display base 1849, adjust 1854 back to 1849 by year effect and treatment
#  - "1854" - display base 1854, adjust 1849 forward to 1854by year (no adjustment) and treatment)
plot3 <- function(confidata1849,confidata1854,xsupplier,title,legposition="bottomright",rateadj="rateadj",yearadj="1849") {            
	if (yearadj == "1849") {
		xmean <- subset(confidata1849,supplier==xsupplier)[,"rate"]
		xlimdn <- subset(confidata1849,supplier==xsupplier)[,"limdn"]
		xlimup <- subset(confidata1849,supplier==xsupplier)[,"limup"]
		xpred <- subset(confidata1849,supplier==xsupplier)[,"predrate"]
		yseq <- subset(confidata1849,supplier==xsupplier)[,"seq"]
		x1854adj <- subset(confidata1854,supplier==xsupplier)[,rateadj]
	}
	else {
		xmean <- subset(confidata1849,supplier==xsupplier)[,rateadj]
		xlimdn <- subset(confidata1854,supplier==xsupplier)[,"limdn"]
		xlimup <- subset(confidata1854,supplier==xsupplier)[,"limup"]
		xpred <- subset(confidata1854,supplier==xsupplier)[,"predrate"]
		yseq <- subset(confidata1854,supplier==xsupplier)[,"seq"]
		x1854adj <- subset(confidata1854,supplier==xsupplier)[,"rate"]
	}
	xplot <- plot3_worker(yseq,xmean, xpred,xlimdn,xlimup,x1854adj,title,legposition)
	# xplot <- plot(xmean, y,
	#     xlim=range(c(xmean, xpred,xlimdn,xlimup,x1854adj)),
	#     ylim=rev(range(y)), col="red", 
	#     main=title,xlab="Mortality per 10,000 population",ylab="sub-district",
	#     pch=19)
	# lines(xpred, y, type="p",
	#     xlim=range(c(xmean, xpred,xlimdn,xlimup)),
	#     ylim=rev(range(y)), 
	#     pch=1)
	# lines(x1854adj, y, type="p",
	#     xlim=range(c(xmean, xpred,xlimdn,xlimup)),
	#     ylim=rev(range(y)), col="blue", 
	#     pch=17)

	# # horizontal error bars
	# xplot <- arrows(xlimdn, y, xlimup, y, length=0.05, angle=90, code=3,lty=3)
	# xplot <- legend(legposition, c("1849 red circle", "1854 blue triangle", "predicted"), col = c(2, 4, 1),
 #       text.col = c("red","blue","black"), lty = c(0, 0, 3), pch = c(19, 17, 1),
 #       merge = TRUE,bty="o",bg="white")    # The bty="o" should overwrite the background

}

# Plotting joint region for 1849 & 1854 with error bars for each
plotcomp <- function(confidata1849,confidata1854,xsupplier,title,legposition="bottomright") {            
	xmean <- subset(confidata1849,supplier==xsupplier)[,"rate"]
	xlimdn <- subset(confidata1849,supplier==xsupplier)[,"limdnact"]
	xlimup <- subset(confidata1849,supplier==xsupplier)[,"limupact"]
	xpred <- subset(confidata1849,supplier==xsupplier)[,"predrate"]
	y <- subset(confidata1849,supplier==xsupplier)[,"seq"]
	xmean1854 <- subset(confidata1854,supplier==xsupplier)[,"rateadjyr"]
#	xlimdn1854 <- subset(confidata1854,supplier==xsupplier)[,"limdn"]
#	xlimup1854 <- subset(confidata1854,supplier==xsupplier)[,"limup"]
	xplot <- plot(xmean, y,
	    xlim=range(c(xmean, xmean1854,xlimdn,xlimup)),
	    ylim=rev(range(y)), col="red",
	    main=title,xlab="Mortality: 1849 red circle, 1854 blue diamond",ylab="sub-district",
	    pch=19)
	lines(xmean1854, y, type="p",
	    xlim=range(c(xmean, xmean1854,xlimdn,xlimup)),
	    ylim=rev(range(y)), col="blue",
	    pch=17)

	# horizontal error bars
	xplot <- arrows(xlimdn, y, xlimup, y, length=0.05, angle=90, code=3,,lty=3)
	xplot <- legend(legposition, c("1849 red circle", "1854 blue triangle"), col = c(2, 4),
       text.col = c("red","blue"), lty = c(0, 0), pch = c(19, 17),
       bty="o",bg="white")    # The bty="o" should overwrite the background

#	xplot <- arrows(xlimdn1854, y, xlimup1854, y, length=0.05, angle=90, code=3)
}


ploterrbars <- function(fittedmodel,plotname,single = "single") {       # This is not a good way to do this because I am 
	                                                  # changing globals from within the function (using <<- 
	                                                  # instead of <-)
	xfamily <- preperrdata(fittedmodel,single)  # this function modifies global data

	pdf(paste("../paper/figures/errbar_",plotname,"a.pdf",sep=""))
		plot2(x1849,"SouthwarkVauxhall",paste("First-12 Southwark-only ",xfamily," 1849 "))
	dev.off()
	pdf(paste("../paper/figures/errbar_",plotname,"b.pdf",sep=""))
		plot2(x1849,"SouthwarkVauxhall_Lambeth",paste("Next-16 Jointly-Supplied ",xfamily," 1849 "))
	dev.off()
	pdf(paste("../paper/figures/errbar_",plotname,"c.pdf",sep=""))
		plot3(x1849,x1854,"SouthwarkVauxhall",paste("First-12 Southwark-only ",xfamily," 1849vs1854 "))
	dev.off()
	pdf(paste("../paper/figures/errbar_",plotname,"d.pdf",sep=""))
		plot3(x1849,x1854,"SouthwarkVauxhall_Lambeth",paste("Next-16 Jointly-Supplied ",xfamily," 1849vs1854 "))
	dev.off()
	if (xfamily != "poisson") {         # Plot comparison for joint region only for Negative Binomial
		pdf(paste("../paper/figures/errbar_",plotname,"e.pdf",sep=""))
			plotcomp(x1849,x1854,"SouthwarkVauxhall",paste("First-12 Southwark-only ",xfamily," 1849vs1854 "))
		dev.off()
		pdf(paste("../paper/figures/errbar_",plotname,"f.pdf",sep=""))
			plotcomp(x1849,x1854,"SouthwarkVauxhall_Lambeth",paste("Next-16 Jointly-Supplied ",xfamily," 1849vs1854 "))
		dev.off()
	}

}



