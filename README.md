![NCIEA_Logo](http://www.gravatar.com/avatar/4e8359782a12ae6da19d546220e1a8b0.png)

SGP 
===

An R Package for the calculation of student growth percentiles and percentile growth trajectories/projections
-------------------------------------------------------------------------------------------------------------


The **SGP** Package (Betebenner, VanIwaarden, Domingue, Shang, 2014) is an open source package built for the open source **R** software environment (R Development Core Team, 2014). The classes, functions and data within the **SGP** package are used to calculate student growth percentiles and percentile growth projections/trajectories using large scale, longitudinal assessment data. The methodology uses quantile regression to estimate the conditional density associated associated with each student's achievement history. Percentile growth projections/trajectories are calculated using the coefficient matrices derived from the quantile regression analyses and specify the percentile growth required for students to reach future achievement targets.

* Web site: http://centerforassessment.github.io/SGP/
* CRAN Web site: http://cran.r-project.org/web/packages/SGP/

Team Members
------------

The **SGP** Package is maintained by a group of four very nice people. We love feedback and are happy to answer questions. But make sure to send us some :heart:.

* [Damian Betebenner](https://github.com/dbetebenner)
* [Adam VanIwaarden](https://github.com/adamvi)
* [Ben Domingue](https://github.com/ben-domingue)
* [Yi Shang](https://github.com/shangyi)



Install the latest stable release from [CRAN](http://cran.r-project.org/package=SGP)
---------------------------

```coffee
install.packages("SGP")
require(SGP)
```


Install latest development release from [Github](https://github.com/CenterForAssessment/SGP/) :octocat:
----------------------------------------------

```coffee 
install.packages("devtools")
require(devtools)
install_github("SGP", "CenterForAssessment")
require(SGP)
```

To install from Github you might need: Windows: Rtools (http://cran.r-project.org/bin/windows/Rtools/), OS X: xcode (from the app store),
Linux: apt-get install r-base-dev (or similar).


To use the SGP Package
______________________

The [SGP Package Wiki](https://github.com/CenterForAssessment/SGP/wiki/Home) contains instructions on how to prepare data and run SGP analyses.


![FOAS_Logo](http://www.foastat.org/images/foas.png)
