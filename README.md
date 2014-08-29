
<a href="http://sgp.io"><img src="https://raw.githubusercontent.com/CenterForAssessment/SGP_Resources/master/misc/SGP4_Logo.png" align="left" hspace="10" vspace="15"></a>
The **SGP** Package is open source software built for the **R** software environment. The classes, functions and data within the **SGP** package are used to calculate student growth percentiles and percentile growth projections/trajectories using large scale, longitudinal assessment data. Quantile regression is used to estimate the conditional density associated with each student's achievement history. Percentile growth projections/trajectories are calculated using the derived coefficient matrices and specify the percentile growth required to reach future achievement targets.

[![Build Status](https://travis-ci.org/dbetebenner/SGP.svg?branch=master)](https://travis-ci.org/dbetebenner/SGP)

* Web site: [sgp.io](http://sgp.io)
* CRAN Web site: http://cran.r-project.org/web/packages/SGP/

### Team Members

The **SGP** Package is crafted with :heart: by: 

* [Damian Betebenner](https://github.com/dbetebenner)
* [Adam VanIwaarden](https://github.com/adamvi)
* [Ben Domingue](https://github.com/ben-domingue)
* [Yi Shang](https://github.com/shangyi)

We love feedback and are happy to answer questions.


### Install the latest stable release from [CRAN](http://cran.r-project.org/package=SGP)

```R
install.packages("SGP")
require(SGP)
```


### Install latest development release from [Github](https://github.com/CenterForAssessment/SGP/) :octocat:

```R
install.packages("devtools")
require(devtools)
install_github("SGP", "CenterForAssessment")
require(SGP)
```

To install from Github you might need: Windows: Rtools (http://cran.r-project.org/bin/windows/Rtools/), OS X: xcode (from the app store),
Linux: apt-get install r-base-dev (or similar).


### To use the SGP Package

The [SGP Package Wiki](https://github.com/CenterForAssessment/SGP/wiki/Home) contains instructions on how to prepare data and run SGP analyses.


### References

Betebenner, D. W., VanIwaarden, A., Domingue, B., and Shang, Y. (2014). SGP: An R Package for the Calculation and Visualization of Student Growth Percentiles & Percentile Growth Trajectories. 

R Development Core Team (2014). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria.
3-900051-07-0.

___________________________________________________________________


The SGP Package supports the [Foundation for Open Access Statistics (FOAS)](http://www.foastat.org/index.html)
