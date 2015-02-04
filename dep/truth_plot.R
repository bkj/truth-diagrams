# <<< Truth Plot >>>

truth <- function(xmin, xmax, ymin, ymax, stat.type, square){
	
		if(sum(is.na(c(xmin, xmax, ymin, ymax))) > 0){
			truth.error()
			return()
		}
		
		if(xmin > 0 | ymin > 0 | xmax < 0 | ymax < 0){
			truth.error()
			return()
		}
		
		
		xmin <- as.numeric(xmin)
		xmax <- as.numeric(xmax)
		ymin <- as.numeric(ymin)
		ymax <- as.numeric(ymax)

		d <- as.numeric(c(xmin, xmax, ymin, ymax))
		d <- data.frame(d)
			
		xmar <- max(abs(c(xmin, xmax))) + .15 * (xmax - xmin)
		ymar <- max(abs(c(ymin, ymax))) + .15 * (ymax - ymin)

		if(square == T){
			tmp <- max(xmar, ymar)
			xmar <- 2*tmp
			ymar <- tmp
		}
		
		bl.col <- 'grey20'
		br.col <- 'grey20'
		ul.col <- 'grey20'
		ur.col <- 'grey20'

		tp.col <- 'blue'
		tn.col <- 'red'
		fn.col <- 'blue'
		fp.col <- 'red'
		
		if(stat.type == 'or'){
			ur.col <- 'orange'
			bl.col <- 'green'
		}
		
		q <- ggplot() + 
#BL
			 geom_rect(data = d, 
					   aes(xmin=d[1], xmax=0, ymin=d[3], ymax=0),
					   colour = 'black',
					   fill = bl.col,
					   alpha = .05) +
#UL	
			 geom_rect(data = d, 
					   aes(xmin=d[1], xmax=0, ymin=0, ymax=d[4]),
					   fill = ul.col,
					   colour = 'black',					   
					   alpha = .05) +
#UR	
			 geom_rect(data = d, 
					   aes(xmin=0, xmax=d[2], ymin=0, ymax=d[4]),
					   colour = 'black',
					   fill = ur.col,
					   alpha = .05) +
#BR
			geom_rect(data = d, 
					   aes(xmin=0, xmax=d[2], ymin=d[3], ymax=0),
					   colour = 'black',
					   fill = br.col,
					   alpha = .05) +
# #FP					   
			 # geom_segment(data=d, aes(x = d[1], y = 0, xend = 0, yend = 0), 
			 			  # color = fp.col,
			 			  # size = .3) +
# #TN
			 # geom_segment(data=d, aes(x = 0, y = 0, xend = d[2], yend = 0), 
			 			  # color = tn.col,
			 			  # size = .3) +	
# #FN				   			   				   
			 # geom_segment(data=d, aes(x = 0, y = d[3], xend = 0, yend = 0), 
			 			  # color = fn.col,
			 			  # size = .3) +
# #TP
			 # geom_segment(data=d, aes(x = 0, y = 0, xend = 0, yend = d[4]), 			 			  				  color = tp.col,
			 			  # size = .3) +
			 			  
			 coord_cartesian(xlim = c(-xmar, xmar), ylim = c(-ymar, ymar)) +
			 xlab("False Positives     (Normals)     True Negatives") +
			 ylab("True Positives     (Abnormals)      False Negatives") +
			 ggtitle("2-by-2 Diagram") +
			 theme(
			 	axis.text.x  = element_text(colour="black",size=15,angle=0),
			 	axis.text.y  = element_text(colour="black",size=15,angle=0),
			 	axis.title.x = element_text(colour="black",size=12,angle=0,
			 								hjust=.5,vjust=-2,face="plain"),
			 	plot.title = element_text(colour="black", size = 20, vjust = 1)
			 ) +
			 theme(plot.margin = unit(rep(0, 4), "cm")) +
			 theme(aspect.ratio = .5, panel.grid.minor = element_blank())

		if (stat.type == 'lr') {
			q <- q + geom_segment(data=d, aes(x = d[1], y = d[3], xend = d[2], yend = d[4]), 
			 			  		color = 'green',
			 			  		size = 2,
			 			  		alpha = .15) +
				 	geom_segment(data=d, aes(x = d[1], y = 0, xend = 0, yend = d[4]), 
			 			  		color = 'orange',
			 			  		size = 2,
			 			  		alpha = .15)	
		}	
			
			
			
		print(q)
}

truth.error <- function(){
				q <- ggplot() + geom_point(aes(x=0, y=0), alpha = 0) + 
				 	xlab("Normals") +
			 	 	ylab("Abnormals") +
			 		theme(
			 			axis.text.x  = element_text(colour="black",size=15,angle=0),
			 			axis.text.y  = element_text(colour="black",size=15,angle=0),
			 			axis.title.x = element_text(colour="black",size=12,angle=0,
			 										hjust=.5,vjust=-2,face="plain"),
			 			plot.title = element_text(colour="black", size = 20, vjust = 1)
			 		) +
			 		theme(plot.margin = unit(rep(1, 4), "cm"))+
			 		coord_cartesian(xlim = c(-20, 20), 
			 						ylim = c(-20, 20)) +
			 		scale_y_continuous(labels=abs) + 
					ggtitle("2-by-2 Diagram - Invalid Data!")
			print(q)
}


