# truth-diagrams

Code for plotting two-by-two "truth diagrams", as described in
```
Kevin M. Johnson and Benjamin K. Johnson. 
A Visual Presentation of Statistical Concepts in Diagnostic Testing: The Two by Two Diagram.  American Journal of Roentgenology. 2014; 203: W14-W20.  
http://www.ajronline.org/doi/full/10.2214/AJR.13.11954
```

Demo at https://bkjohnson.shinyapps.io/truth-diagrams/

### Installation

    cd $PROJECT_ROOT
    ./install.R
    
    # NB: Requires "shiny 0.8.0" and "ggplot2 0.9.3.1"
    # Will fix this shortly

### Quickstart

    cd $PROJECT_ROOT
    
    # Install dependencies
    ./install.R 
    
    ./run.sh

Then open `http://localhost:9000` in your browser.

# Plotting from R
`truth_plot.R` provides the main plotting function `truth`.