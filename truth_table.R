# <<< Truth Table Output >>>

truth.table <- function(dat){
	
		if(sum(is.na(c(dat$TP, dat$TN, dat$FN, dat$FP))) > 0)
			return()
		if(dat$TP < 0 | dat$TN < 0 | dat$FN > 0 | dat$FP > 0)
			return()
	
		sens <- abs(dat$TP / dat$FN)
		spec <- abs(dat$TN / dat$FP)
		PPV  <- dat$TP / (dat$TP + abs(dat$FP))
		NPV  <- dat$TN / (dat$TN + abs(dat$FN))
		corr <- (dat$TP + dat$TN) / dat$N
		prev <- (dat$TP + abs(dat$FN)) / dat$N
		odr <- (dat$TN * dat$TP) / (dat$FN * dat$FP)
		lgr <- (abs(dat$TP) / abs(dat$FP)) / 
					((abs(dat$TP) + abs(dat$FN)) / (abs(dat$TN) + abs(dat$FP)))
		
		d <- data.frame(Statistic = c('Sensitivity',
									  'Specificity',
									  'PPV', 'NPV', 'Pct Correct',
									  'Prevalance', 
									  'Odds Ratio', 
									  'Likelihood Ratio'),
						Value = c(sens, spec, PPV, NPV, corr, prev, odr, lgr))
		return(d)
}