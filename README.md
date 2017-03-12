# truth-diagrams

Code for Kevin Johnson's 2-by-2 "truth diagrams".

# Interactive plotting via <code>Shiny</code> interface
After installing the <code>R</code> package <code>shiny</code> on your machine, navigate to folder, run
  
    Rscript -e "shiny::runApp(port = 888)"

and open <code>http://localhost:888/</code> in your browser.

# Plotting from R
<code>truth_plot.R</code> provides the main plotting function <code>truth()</code>.  Further details on the arguments
to this function are included at the top of this file.

# Citation

```
Kevin M. Johnson and Benjamin K. Johnson. 
A Visual Presentation of Statistical Concepts in Diagnostic Testing: The Two by Two Diagram.  American Journal of Roentgenology. 2014; 203: W14-W20.  
http://www.ajronline.org/doi/full/10.2214/AJR.13.11954
```
