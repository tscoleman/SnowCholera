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

# **plot2_worker** Is a cover function which unpacks the actual versus predicted mean from the appropriate dataframe

# **plot3** Plots actual 1849, 1854 (adjusted for time & treatment effects), predicted, with error bars

# **plotcomp** Plots actual 1849 versus 1854, with error bars around actual 1849

# **ploterrbars** is NOT a function you should use - I use it to print out .pdf versions of the graphs I want to use




preperrdata <- function(fittedmodel,single = "single") {       # This is not a good way to do this because I am 
	                                                  # changing globals from within the function (using <<- 
	                                                  # instead of <-)
# Function to prepare the data (error bars) for graphing
# The 2.5% and 97.5% confidence bands are calculated assuming either Poisson or Negative Binomial
# The rates are calculated by generating the counts up and down from the "expected" (from the fitted model)

	expected <- exp(predict(fittedmodel))       # expected values
	theta <- fittedmodel$theta
	x1849$predcount <<- expected[1:28]
	x1854$predcount <<- expected[29:56]
	x1849$predrate <<- 10000 * expected[1:28] / x1849$pop1851
	x1854$predrate <<- 10000 * expected[29:56] / x1854$pop1851
	xfamily <- family(fittedmodel)$family
	if (xfamily == "poisson") {         # Get 95% confidence bands depending on model used (Poiss vs Neg Binom)
		x1849$limdn <<- 10000 * qpois(.025,lambda=x1849$predcount) / x1849$pop1851
		x1849$limup <<- 10000 * qpois(.975,lambda=x1849$predcount) / x1849$pop1851
	} else {
		x1849$limdn <<- 10000 * qnbinom(.025,size=theta,mu=x1849$predcount) / x1849$pop1851
		x1849$limup <<- 10000 * qnbinom(.975,size=theta,mu=x1849$predcount) / x1849$pop1851
		x1854$limdn <<- 10000 * qnbinom(.025,size=theta,mu=x1854$predcount) / x1849$pop1851
		x1854$limup <<- 10000 * qnbinom(.975,size=theta,mu=x1854$predcount) / x1849$pop1851
		x1849$limdnact <<- 10000 * qnbinom(.025,size=theta,mu=x1849$deaths) / x1849$pop1851
		x1849$limupact <<- 10000 * qnbinom(.975,size=theta,mu=x1849$deaths) / x1849$pop1851
	}


	# Now adjust the 1854 predicted counts (and rates) for the time and treatment fixed effects -
	# effectively netting out the 1854 effect and making it 1849-equivalent
	xyr1854 <- (coef(fittedmodel)["year1854"])
	x3 <- exp(-xyr1854)
	# Adjust for the year effect only - this should make 1849 & 1854 comparable net of time effect
	x1854$rateadjyr <<- 10000 * (x1854$deaths*x3) / x1854$pop1851
	# Adjust the 1854 actual for the estimated Treatment Effect and the estimate Year Effect
	if (single == "single")  {   # model with single treatment effect
		xdegless <- (coef(fittedmodel)["supplierSouthwarkVauxhall_Lambeth:year1854"])
		xdegmore <- xdegless               # Make both treatment effects the same
		x3 <- x3 * exp(-(x1854$lambethdegree == "less_Lambeth")*xdegless) 
		x3 <- x3 * exp(-(x1854$lambethdegree == "more_Lambeth")*xdegmore)
	} else if (single == "two") {      # model with two treatment effects          		
		xdegless <- coef(fittedmodel)["lambethdegreeless_Lambeth:year1854"]
		xdegmore <- coef(fittedmodel)["lambethdegreemore_Lambeth:year1854"]
		x3 <- x3 * exp(-(x1854$lambethdegree == "less_Lambeth")*xdegless) 
		x3 <- x3 * exp(-(x1854$lambethdegree == "more_Lambeth")*xdegmore)
	} else { # model with continuous treatment (population proportions)
		xlambethperc54 <- coef(fittedmodel)["lambethperc54"]
		x3 <- x3 * exp(-(x1854$lambethperc54 * xlambethperc54))
	}

	# Adjust for year & treatment effect - so that we can compare the actuals against each other 
	x1854$rateadj <<- 10000 * (x1854$deaths*x3) / x1854$pop1851

	return(xfamily)
}


# "Worker" function to plot mean, predicted, and error bars
plot2_worker <- function(yseq, xmean,xlimdn,xpred,xlimup,title) {            
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
	xplot
}
# "Cover" function that takes in the dataframe and unpacks
plot2 <- function(confidata,xsupplier,title) {            
	xmean <- subset(confidata,supplier==xsupplier)[,"rate"]
	xlimdn <- subset(confidata,supplier==xsupplier)[,"limdn"]
	xlimup <- subset(confidata,supplier==xsupplier)[,"limup"]
	xpred <- subset(confidata,supplier==xsupplier)[,"predrate"]
	yseq <- subset(confidata,supplier==xsupplier)[,"seq"]
#	xtitle <- paste(xsupplier,title)
	xplot <- plot2_worker(yseq,xmean,xlimdn,xpred,xlimup,title)
}

plot3 <- function(confidata1849,confidata1854,xsupplier,title) {            
	xmean <- subset(confidata1849,supplier==xsupplier)[,"rate"]
	xlimdn <- subset(confidata1849,supplier==xsupplier)[,"limdn"]
	xlimup <- subset(confidata1849,supplier==xsupplier)[,"limup"]
	xpred <- subset(confidata1849,supplier==xsupplier)[,"predrate"]
	y <- subset(confidata1849,supplier==xsupplier)[,"seq"]
	x1854adj <- subset(confidata1854,supplier==xsupplier)[,"rateadj"]
	xplot <- plot(xmean, y,
	    xlim=range(c(xmean, xpred,xlimdn,xlimup,x1854adj)),
	    ylim=rev(range(y)), col="red", 
	    main=title,xlab="Mortality: 1849 red circle, 1854 blue diamond vs predicted (circle)",ylab="sub-district",
	    pch=19)
	lines(xpred, y, type="p",
	    xlim=range(c(xmean, xpred,xlimdn,xlimup)),
	    ylim=rev(range(y)), 
	    pch=1)
	lines(x1854adj, y, type="p",
	    xlim=range(c(xmean, xpred,xlimdn,xlimup)),
	    ylim=rev(range(y)), col="blue", 
	    pch=17)

	# horizontal error bars
	xplot <- arrows(xlimdn, y, xlimup, y, length=0.05, angle=90, code=3,lty=3)
}

# Plotting joint region for 1849 & 1854 with error bars for each
plotcomp <- function(confidata1849,confidata1854,xsupplier,title) {            
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



