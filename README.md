# bank-analysis-r

A tool to show some bank record analysis read from a csv file<br/>

Based on <a href="https://benjaminlmoore.wordpress.com/2014/01/04/analyse-your-bank-statements-using-r/">this</a> but done with the use of java.

This is done with the use of <a href="https://cran.r-project.org/">R</a>, <a href="https://www.rforge.net/Rserve/doc.html">Rserve</a>

<h2>Prerequisites</h2>

<p>Download and install <a href="https://cran.r-project.org/bin/">R</a></p>

<p>Follow the <a href="https://www.rforge.net/Rserve/doc.html">Rserve</a> documentation for installation</p>

<h2>Deploy</h2>

Start the Rserve server, this can be done with running <br/>
```
library(Rserve)
Rserve()
```
in the R console OR running<br/>
```
R CMD Rserve
```
Refer to the Rserve documentation for more information.

Run Main.java