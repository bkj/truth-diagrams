

stat.table <- function(stats, sel, nb = F){
			
		summary.stats <- summary_statistics(stats)
		
		if(length(sel) == 0)
			sel <- 'none'
		
		html <- paste0(
					HTML('<table cellpadding = 6 width = 200 border = 1>'),
					HTML(paste0(
						'<tr><th> Statistic </th><th> Value </th></tr>
						<tr><td>',
									if(!nb) sel_radio_html('stat.type', 'none', "  None", sel),
						'</td><td>',
									"",
						'</td></tr>
						<tr><td>',
									if(!nb) sel_radio_html('stat.type', 'sens', "  Sensitivity", sel),
						'</td><td>',
									summary.stats$sens,
						'</td></tr>
						<tr><td>',
									if(!nb) sel_radio_html('stat.type', 'spec', "  Specificity", sel),
						'</td><td>',
									summary.stats$spec,
						'</td></tr>
						<tr><td>',
									if(!nb) sel_radio_html('stat.type', 'ppv', "  PPV", sel),
						'</td><td>',
									summary.stats$ppv,
						'</td></tr>
						 <tr><td>',
									if(!nb) sel_radio_html('stat.type', 'npv', "  NPV", sel),
						'</td><td>',
									summary.stats$npv,
						'</td></tr>
						 <tr><td>',
									if(!nb) sel_radio_html('stat.type', 'pct', "  Pct Accurate", sel),
						'</td><td>',
									summary.stats$pct,
						'</td></tr>
						<tr><td>',
									if(!nb) sel_radio_html('stat.type', 'prev', "  Prevalence", sel),
						'</td><td>',
									summary.stats$prev,
						'</td></tr>
						<tr><td>',
									if(!nb) sel_radio_html('stat.type', 'or', "  Odds Ratio", sel),
						'</td><td>',
									summary.stats$or,
						'</td></tr>
						<tr><td>',
									if(!nb) sel_radio_html('stat.type', 'lr', "  Lik. Ratio (+)", sel),
						'</td><td>',
									summary.stats$lr,
						'</td></tr>
            <tr><td>',
                  if(!nb) sel_radio_html('stat.type', 'chisq_stat', "  Chi-Sq. Stat", sel),
            '</td><td>',
                  summary.stats$chisq_stat,
						'</td></tr>
            <tr><td>',
						      if(!nb) sel_radio_html('stat.type', 'chisq_p', "  Chi-Sq. p-val", sel),
						'</td><td>',
						      summary.stats$chisq_p,
						'</td></tr>'
						)
					)		
				)
	
		return(html)
}

radio_html <- function(radio_name, radio_value, radio_text) {
	paste0('<input type="radio" name="', 
		radio_name, '" value="', radio_value, '">', radio_text)
}

checked_radio_html <- function(radio_name, radio_value, radio_text) {
	paste0('<input type="radio" name="', 
		radio_name, '" value="', radio_value, '" checked>', radio_text)
}

sel_radio_html <- function(radio_name, radio_value, radio_text, sel) {

	if(sel == radio_value)
		return(checked_radio_html(radio_name, radio_value, radio_text))
	
	return(radio_html(radio_name, radio_value, radio_text))
}
summary_statistics <- function(dat){
	
		if(sum(is.na(c(dat$TP, dat$TN, dat$FN, dat$FP))) > 0)
			return()
		if(dat$TP < 0 | dat$TN < 0 | dat$FN > 0 | dat$FP > 0)
			return()
	
		sens 	<- abs(dat$TP) / (abs(dat$FN) + abs(dat$TP))
		spec 	<- abs(dat$TN )/ (abs(dat$FP) + abs(dat$TN))
		ppv  	<- dat$TP / (dat$TP + abs(dat$FP))
		npv  	<- dat$TN / (dat$TN + abs(dat$FN))
		pct 	<- (dat$TP + dat$TN) / dat$N
		prev 	<- (dat$TP + abs(dat$FN)) / dat$N
		or 		<- (dat$TN * dat$TP) / (dat$FN * dat$FP)
		lr 		<- (abs(dat$TP) / abs(dat$FP)) / 
				   ((abs(dat$TP) + abs(dat$FN)) / (abs(dat$TN) + abs(dat$FP)))
    
    chq        <- chisq.test(matrix(c(abs(dat$TP), abs(dat$FP), abs(dat$FN), abs(dat$TN)), byrow = T))
		chisq_stat <- chq$statistic
    chisq_p    <- chq$p.value
    
		d <- list(sens = sens, spec = spec, ppv = ppv, npv = npv, pct = pct, prev = prev, or = or, lr = lr, chisq_stat = chisq_stat, chisq_p = chisq_p)
		d <- lapply(d, function(x) round(x, 5))
		return(d)
}


