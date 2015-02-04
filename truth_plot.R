# <<< Truth Plot >>>

# d is a dataframe of the following form
# d  <-  data.frame(FN = -number_false_negatives,
# 					TN = number_true_negatives,
# 					FP = -number_false_positives,
# 					TP =  number_true_positives)

# stat.type is one of the following strings
#	none
#	lr			Likelihood ratio
#	or			Odds ratio
#	sens		Sensitivity
#	spec		Specificity
#	ppv			Positive predictive value
#	npt			Negative predictive value
#	pct			Percent positive
#	prev		Prevalence
#	chisq_stat	Chisq test statistic
#	chisq_stat	Chisq test p-value

# square is a boolean argument that determines whether
# we force a square coordinate system on the plots.

truth <- function(d, stat.type, square, is.pdf = F){
	
    frame.fill <- 'black'
    
	if(sum(is.na(d)) > 0) {return.error(); return()}
	if(d$FP > 0 | d$FN > 0 | d$TN < 0 | d$TP < 0) {return.error();return()}
		
	q <- ggplot(d) + geom_rect(aes(xmin=FP, xmax=TN, ymin=FN, ymax=TP),
				   		      colour = 'black',
				   		      fill = frame.fill,
				   		      alpha = .05, 
				   		      size = 2)
    
  if(is.null(stat.type))
    return(NULL)
    
		if (stat.type == 'chisq_stat' | stat.type == 'chisq_p'){
		  
		  with(abs(d), {
		    r1 <- TP + FP; r2 <- FN + TN
		    c1 <- TP + FN; c2 <- FP + TN
		    
		    expected <<- data.frame(FP = -r1*c2, 
		                            TN = r2*c2, 
		                            FN = -c1*r2, 
		                            TP = r1*c1)
		    expected <<- expected / (FP + TN + FN + TP)
		  })
		  q <- add.stat(q, d, stat.type, expected)
		  q <- truth.aesthetics(q, d, square, is.pdf, expected) 
		  return(q)
		} else {
		  q <- truth.aesthetics(q, d, square, is.pdf) 
		  q <- add.stat(q, d, stat.type)
		  return(q)
		}

}
		
truth.aesthetics <- function(p, d, square, is.pdf, expected = NULL){
  
  if(!is.null(expected)){
    d$FP <- sign(d$FP)*max(abs(c(d$FP, expected$FP)))
    d$FN <- sign(d$FN)*max(abs(c(d$FN, expected$FN)))
    d$TN <- sign(d$TN)*max(abs(c(d$TN, expected$TN)))
    d$TP <- sign(d$TP)*max(abs(c(d$TP, expected$TP)))
  }
  
	if(square == F){
		xmar <- max(abs(c(d$FP, d$TN))) + .15 * (d$TN - d$FP)
		ymar <- max(abs(c(d$FN, d$TP))) + .15 * (d$TP - d$FN)

		xmin <- -xmar; xmax <- xmar
		ymin <- -ymar; ymax <- ymar

		center.x <- center.y <- 0

	} else if(square == T){
		
		center.x <- mean(c(d$FP, d$TN))
		center.y <- mean(c(d$FN, d$TP))
		max.dev  <- max(abs(c(d$FP - center.x, d$TN - center.x, 
								d$FN - center.y, d$TP - center.y))) * 1.25
				
		xmin <- center.x - 2*max.dev; xmax <- center.x + 2*max.dev
		ymin <- center.y - max.dev; ymax <- center.y + max.dev
	}
		
	
	p <- p + coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
			annotate("text", x = (xmax - xmin)*.025, y = ymax - (ymax - ymin)*.05, 
						label = "TP", colour = 'red', size = 7) + 
			annotate("text", x = xmax - (xmax - xmin)*.025, y = (ymax - ymin)*.05, 
						label = "TN", colour = 'red', size = 7) + 
						
			annotate("text", x = xmin + (xmax - xmin)*.025, y = -(ymax - ymin)*.05, 
						label = "FP", colour = 'red', size = 7) + 
			annotate("text", x = -(xmax - xmin)*.025, y = ymin + (ymax - ymin)*.05, 
						label = "FN", colour = 'red', size = 7) + 
			xlab("Normals") +
			ylab("Abnormals") +
			theme(
			 	axis.text.x  = element_text(colour="black",size=15,angle=0),
			 	axis.text.y  = element_text(colour="black",size=15,angle=0),
			 	axis.title   = element_text(colour="black",size=20,angle=0, 
			 								vjust = -.5, face="plain")
			 ) +
			 geom_hline(aes(y = 0), colour = 'grey30', size = 1) + 
			 geom_vline(aes(x = 0), colour = 'grey30', size = 1) + 
			 theme(plot.margin = unit(rep(0, 4), "cm")) +
			 theme(aspect.ratio = .5, panel.grid.minor = element_blank()) +
			 theme(panel.background = element_rect(fill="white"), 
			 		panel.grid.major = element_line(colour = 'grey70'),
			 		panel.grid.minor = element_blank(),
			 		plot.margin = unit(c(1, 1, 1, 1), 'cm')) +
			 scale_x_continuous(labels = abs) +
			 scale_y_continuous(labels = abs)

	if(is.pdf == T)
		p <- p + theme(plot.margin = unit(c(3, 3, 3, 3), 'cm'))
		
	return(p)
}

add.stat <- function(p, d, stat.type, expected = NULL){
	
		num.col   		<- 'orange'
		denom.col 		<- 'green'
		expected.col 	<- denom.col
    
		op.op <- 1
		np.op <- .5
		lwd   <- 3
		
		if(!exists('stat.type') | length(stat.type) == 0){
			return(p)
		} else if (stat.type == 'lr') {
			p <- p + geom_segment(data=d, 
					 aes(x = FP, y = FN, xend = TN, yend = TP), 
			 		 color = denom.col,
			 		 size = lwd,
			 		 alpha = np.op) +
				 	 
				 	 geom_segment(data=d, aes(x = FP, y = 0, xend = 0, yend = TP), 
			 		 color = num.col,
			 		 size = lwd,
			 		 alpha = np.op)
			 		 	
		} else if(stat.type == 'or'){
			
#       Alternative Explanation
# 			p <- p + geom_rect(data = d, 
# 					   aes(xmin=FP, xmax=0, ymin=FN, ymax=0),
# 					   colour = denom.col,
# 					   fill = denom.col,
# 					   alpha = np.op) +
# 					   
# 					   geom_rect(data = d, 
# 					   aes(xmin=0, xmax=TN, ymin=0, ymax=TP),
# 					   colour = num.col,
# 					   fill = num.col,
# 					   alpha = np.op)
      
		  p <- p + geom_segment(data=d, aes(x = FP, y = 0, xend = 0, yend = TP), 
		                               color = num.col,
		                               size = lwd,
		                               alpha = np.op) +
                geom_segment(data=d, 
		                        aes(x = 0, y = FN, xend = TN, yend = 0), 
		                        color = denom.col,
		                        size = lwd,
		                        alpha = np.op)
      
		} else if (stat.type == 'sens'){
			p <- p + geom_segment(data = d, 
									aes(x = 0, y = 0, xend = 0, yend = TP),
									colour = num.col,
									size = lwd,
									alpha = op.op) + 
					geom_segment(data = d, 
									aes(x = 0, y = 0, xend = 0, yend = FN),
									colour = denom.col,
									size = lwd,
									alpha = op.op) 
		} else if (stat.type == 'spec'){
			p <- p + geom_segment(data = d, 
									aes(x = 0, y = 0, xend = TN, yend = 0),
									colour = num.col,
									size = lwd,
									alpha = op.op) + 
					geom_segment(data = d, 
									aes(x = 0, y = 0, xend = FP, yend = 0),
									colour = denom.col,
									size = lwd,
									alpha = op.op) 
		} else if (stat.type == 'ppv'){
			p <- p + geom_segment(data = d, 
									aes(x = FP, y = 0, xend = FP, yend = TP),
									colour = num.col,
									size = lwd,
									alpha = op.op) + 
					geom_segment(data = d, 
									aes(x = FP, y = TP, xend = 0, yend = TP),
									colour = denom.col,
									size = lwd,
									alpha = op.op) 
		} else if (stat.type == 'npv'){
			p <- p + geom_segment(data = d, 
									aes(x = 0, y = FN, xend = TN, yend = FN),
									colour = num.col,
									size = lwd,
									alpha = op.op) + 
					geom_segment(data = d, 
									aes(x = TN, y = 0, xend = TN, yend = FN),
									colour = denom.col,
									size = lwd,
									alpha = op.op) 		
		} else if (stat.type == 'pct'){
			p <- p + geom_segment(data = d, 
									aes(x = FP, y = FN, xend = 0, yend = FN),
									colour = denom.col,
									size = lwd,
									alpha = op.op) +
					geom_segment(data = d, 
									aes(x = FP, y = FN, xend = FP, yend = 0),
									colour = denom.col,
									size = lwd,
									alpha = op.op) +	
												
					geom_segment(data = d, 
									aes(x = TN, y = 0, xend = TN, yend = TP),
									colour = num.col,
									size = lwd,
									alpha = op.op)	+
					geom_segment(data = d, 
									aes(x = TN, y = TP, xend = 0, yend = TP),
									colour = num.col,
									size = lwd,
									alpha = op.op)	
											
		} else if (stat.type == 'prev'){
			p <- p + geom_segment(data = d, 
									aes(x = FP, y = FN, xend = TN, yend = FN),
									colour = denom.col,
									size = lwd,
									alpha = op.op) +
									
					geom_segment(data = d, 
									aes(x = TN, y = FN, xend = TN, yend = TP),
									colour = num.col,
									size = lwd,
									alpha = op.op)				
		} else if (stat.type == 'chisq_stat' | stat.type == 'chisq_p'){
      		p <- p + geom_rect(data = expected, 
                         aes(xmin=FP, xmax=TN, ymin=FN, ymax=TP),
                         colour = expected.col,
                         size = lwd,
                         alpha = 0)
		}
			
		return(p)
}

return.error <- function() {
	q <- ggplot() + geom_point(aes(x=0, y=0), alpha = 0) + 
		 xlab("Normals") +  ylab("Abnormals") +
		 theme(plot.margin = unit(rep(1, 4), "cm"))+
		 coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
		 ggtitle("Invalid Data!")
	
	q <- q + 
			theme(
			 	axis.text.x  = element_text(colour="black",size=15,angle=0),
			 	axis.text.y  = element_text(colour="black",size=15,angle=0),
			 	axis.title   = element_text(colour="black",size=20,angle=0, 
			 								vjust = -.5, face="plain")
			 ) +
			 geom_hline(aes(y = 0), colour = 'grey30', size = 1) + 
			 geom_vline(aes(x = 0), colour = 'grey30', size = 1) + 
			 theme(plot.margin = unit(rep(0, 4), "cm")) +
			 theme(aspect.ratio = .5, panel.grid.minor = element_blank()) +
			 theme(panel.background = element_rect(fill="white"), 
			 		panel.grid.major = element_line(colour = 'grey70'),
			 		panel.grid.minor = element_blank(),
			 		plot.margin = unit(c(1, 1, 1, 1), 'cm')) +
			 scale_x_continuous(labels = abs) +
			 scale_y_continuous(labels = abs)
		print(q)
}


