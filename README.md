![SGP: Student Growth Percentiles](https://raw.githubusercontent.com/CenterForAssessment/SGP_Resources/master/misc/SGP_Futura.png)

______________________________________________________________________________________________________

### An R Package for the calculation of student growth percentiles and percentile growth trajectories/projections


The **SGP** Package (Betebenner, VanIwaarden, Domingue, Shang, 2014) is an open source package built for the open source **R** software environment (R Development Core Team, 2014). The classes, functions and data within the **SGP** package are used to calculate student growth percentiles and percentile growth projections/trajectories using large scale, longitudinal assessment data. The methodology uses quantile regression to estimate the conditional density associated associated with each student's achievement history. Percentile growth projections/trajectories are calculated using the coefficient matrices derived from the quantile regression analyses and specify the percentile growth required for students to reach future achievement targets.

* Web site: [sgp.io](http://sgp.io)
* CRAN Web site: http://cran.r-project.org/web/packages/SGP/

### Team Members

The **SGP** Package is maintained by a group of four very nice people. We love feedback and are happy to answer questions. Just make sure to show us some :heart:.

* [Damian Betebenner](https://github.com/dbetebenner)
* [Adam VanIwaarden](https://github.com/adamvi)
* [Ben Domingue](https://github.com/ben-domingue)
* [Yi Shang](https://github.com/shangyi)



### Install the latest stable release from [CRAN](http://cran.r-project.org/package=SGP)

```S
install.packages("SGP")
require(SGP)
```


### Install latest development release from [Github](https://github.com/CenterForAssessment/SGP/) :octocat:

```S
install.packages("devtools")
require(devtools)
install_github("SGP", "CenterForAssessment")
require(SGP)
```

To install from Github you might need: Windows: Rtools (http://cran.r-project.org/bin/windows/Rtools/), OS X: xcode (from the app store),
Linux: apt-get install r-base-dev (or similar).


### To use the SGP Package

The [SGP Package Wiki](https://github.com/CenterForAssessment/SGP/wiki/Home) contains instructions on how to prepare data and run SGP analyses.


### Bibliography

Betebenner, D. W., VanIwaarden, A., Domingue, B., and Shang, Y. (2014). SGP: An R Package for the Calculation and Visualization of Student Growth Percentiles & Percentile Growth Trajectories. 

R Development Core Team (2014). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria.
3-900051-07-0.

___________________________________________________________________


The SGP Package supports the [Foundation for Open Access Statistics (FOAS)](http://www.foastat.org/index.html)
