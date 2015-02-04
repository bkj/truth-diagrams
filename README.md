# truth-diagrams

Code for Kevin Johnson's 2-by-2 "truth diagrams".

Demo at http://ycas.yale.edu/2by2.aspx

# Interactive plotting via <code>Shiny</code> interface
After installing the <code>R</code> package <code>shiny</code> on your machine, navigate to folder, run
  
    Rscript -e "shiny::runApp(port = 888)"

and open <code>http://localhost:888/</code> in your browser.

# Plotting from R
<code>truth_plot.R</code> provides the main plotting function <code>truth()</code>.  Further details on the arguments
to this function are included at the top of this file.
