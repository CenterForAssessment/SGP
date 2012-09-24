SGP 
===


Student Growth Percentiles & Percentile Growth Trajectories/Projections
-----------------------------------------------------------------------

Functions to calculate student growth percentiles and percentile growth projections/trajectories for students using large scale, longitudinal assessment 
data. Functions use quantile regression to estimate the conditional density associated with each student's 
achievement history. Percentile growth projections/trajectories are calculated using the coefficient matrices derived from the quantile 
regression analyses and specify what percentile growth is required for students to reach future achievement targets.

* Web site: http://studentgrowthpercentiles.github.com/SGP/
* CRAN Web site: http://cran.r-project.org/web/packages/SGP/


Install stable CRAN release
---------------------------

```R 
install.packages("SGP")
require(SGP)
```



Install latest development release from Github
----------------------------------------------

```R 
install.packages("devtools")
require(devtools)
install_github("SGP", "SchoolView")
require(SGP)
```

To install from Github you might need: Windows: Rtools (http://cran.r-project.org/bin/windows/Rtools/), OS X: xcode (from the app store),
Linux: apt-get install r-base-dev (or similar)

