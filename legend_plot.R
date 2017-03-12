legend_plot <- function(stat.type){
	
	thick <- 4
	if(length(stat.type) == 0) 	{print(nothing()); return();}
	if(stat.type == 'none') 	{print(nothing()); return();}
	
	if(stat.type %in% c('sens', 'spec', 'ppv', 'npv', 'pct', 'prev')) {
		l <- orange_over_sum(thick)
	} else if (stat.type == 'or') { 
# 		l <- area_ratio(thick)
    	l <- slope_ratio(thick)
	} else if (stat.type %in% c('lr_plus', 'lr_neg')) {
		l <- slope_ratio(thick) 
	} else if (stat.type %in% c('chisq_stat', 'chisq_p')) {
    	l <- expected_box(thick)
	}
		
 	l <- l + coord_cartesian(xlim = c(-2, 2), ylim = c(-1, 1)) +
 		 theme(axis.line = element_blank(),
    				panel.grid.major = element_blank(),
    				panel.grid.minor = element_blank(),
    				panel.background = element_blank(), 
    				axis.title.x = element_blank(),
    				axis.title.y = element_blank(),
    				axis.text.x = element_blank(),
    				axis.text.y = element_blank(),
    				axis.ticks.y = element_blank(),
    				axis.ticks.x = element_blank(),
    				plot.title   = element_text(colour="black", 
    											vjust = 2, face="plain"),
    				panel.border = element_rect(colour = 'black', fill = NA)							) +
    	ggtitle('Statistic') 
	print(l)
		
}

nothing <- function(){
	ret <- ggplot() + 
 		annotate('text', x = 0, y = .5, label = "") +
 		ggtitle('') + 
 		theme(axis.line = element_blank(),
    				panel.grid.major = element_blank(),
    				panel.grid.minor = element_blank(),
    				panel.border = element_blank(),
    				panel.background = element_blank(), 
    				axis.title.x = element_blank(),
    				axis.title.y = element_blank(),
    				axis.text.x = element_blank(),
    				axis.text.y = element_blank(),
    				axis.ticks.y = element_blank(),
    				axis.ticks.x = element_blank()) 

	return(ret)
}

orange_over_sum <- function(thick){
		ret <- ggplot() +
		annotate("segment", x = -.5, xend = .5, y = .5, yend = .5,
 			 	colour = "orange", size = thick) +
 	    annotate("segment", x = -1.5, xend = 1.5, y = 0, yend = 0,
 			 	colour = "black", size = thick) +
 	    annotate("segment", x = -1.25, xend = -.25, y = -.5, yend = -.5,
 			 	colour = "green", size = thick) +
 		annotate("segment", x = .25, xend = 1.25, y = -.5, yend = -.5,
 			 	colour = "orange", size = thick) +
 		annotate('text', x = 0, y = -.5, label = "+")
 		
 		return(ret)
}

slope_ratio <- function(thick){
	
		ret <- ggplot() +
		annotate("text", x = 0, y = .5,
 			 	colour = "orange", size = 2*thick, label = 'slope') +
 	    annotate("segment", x = -1.5, xend = 1.5, y = 0, yend = 0,
 			 	colour = "black", size = thick) +
 	    annotate("text", x = 0, y = -.5,
 			 	colour = "green", size = 2*thick, label = 'slope')

 		return(ret)
}

area_ratio <- function(thick){
	
		ret <- ggplot() +
		annotate("text", x = 0, xend = .5, y = .5, yend = .5,
 			 	colour = "orange", size = 2*thick, label = 'area') +
 	    annotate("segment", x = -1.5, xend = 1.5, y = 0, yend = 0,
 			 	colour = "black", size = thick) +
 	    annotate("text", x = 0, xend = .5, y = -.5, yend = -.5,
 			 	colour = "green", size = 2*thick, label = 'area')

 		return(ret)
}

expected_box <- function(thick){
  ret <- ggplot() + 
          annotate("rect", xmin = -1.5, xmax = 1.5, ymin = -.75, ymax = .75,
                              size = .75*thick, alpha = 0, color = 'green') +
          annotate("text", x = 0, xend = .5, y = 0, yend = .5,
             colour = "black", size = 2*thick, label = 'expected\nposition') 
  
  return(ret)
}
