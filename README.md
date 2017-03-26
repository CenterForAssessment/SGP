
<a href="https://sgp.io"><img src="https://raw.githubusercontent.com/CenterForAssessment/SGP_Resources/master/misc/SGP4_Logo_2.png" align="left" hspace="12" vspace="15"></a>
The **SGP** Package is open source software built for the [**R** software environment](https://CRAN.R-project.org/). The classes, functions and data within the **SGP** package are used to calculate student growth percentiles and percentile growth projections/trajectories using large scale, longitudinal assessment data. Quantile regression is used to estimate the conditional density associated with each student's achievement history. Percentile growth projections/trajectories are calculated using the derived coefficient matrices and show the percentile growth needed to reach future achievement targets.

[![Build Status](https://travis-ci.org/dbetebenner/SGP.svg?branch=master)](https://travis-ci.org/dbetebenner/SGP)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SGP)](http://cran.r-project.org/package=SGP)
[![Development Version](https://img.shields.io/badge/devel-1.6--4.22-brightgreen.svg)](https://github.com/CenterForAssessment/SGP)
[![Rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/SGP)](https://github.com/metacran/cranlogs.app)
[![License](http://img.shields.io/badge/license-GPL%203-brightgreen.svg?style=flat)](https://github.com/CenterForAssessment/SGP/blob/master/LICENSE.md)
[![Join the chat at https://gitter.im/CenterForAssessment/SGP](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/CenterForAssessment/SGP?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

### SGP Resources

* Web: [sgp.io](https://sgp.io)
* CRAN Repo: https://CRAN.R-project.org/package=SGP


### Contributors

The **SGP** Package is crafted with :heart: by:

* [Damian Betebenner](https://github.com/dbetebenner)
* [Adam VanIwaarden](https://github.com/adamvi)
* [Ben Domingue](https://github.com/ben-domingue)
* [Yi Shang](https://github.com/shangyi)

We love feedback and are happy to answer questions.


### Install the latest stable release from [CRAN](https://CRAN.R-project.org/package=SGP)

```R
install.packages("SGP")
require(SGP)
```


### Install latest development release from [Github](https://github.com/CenterForAssessment/SGP/) :octocat:

```R
install.packages("devtools")
require(devtools)
install_github("CenterForAssessment/SGP")
require(SGP)
```

To install from Github you might need: Windows: [Rtools](https://CRAN.R-project.org/bin/windows/Rtools/), OS X: xcode (from the app store),
Linux: apt-get install r-base-dev (or similar).


### To use the SGP Package

* The [SGP Package Wiki](https://github.com/CenterForAssessment/SGP/wiki/Home) contains instructions on how to prepare data and run **SGP** analyses.
* The [SGP Package Documentation](https://CRAN.R-project.org/web/packages/SGP/SGP.pdf) contains man pages for all functions and data sets in the the **SGP** Package.
* Numerous [SGP Gists](https://gist.github.com/dbetebenner) provide other examples of analyses that can be performed.


### References

Betebenner, D. W., VanIwaarden, A., Domingue, B., and Shang, Y. (2017). SGP: Student Growth Percentiles & Percentile Growth Trajectories.

R Development Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria.
3-900051-07-0.
