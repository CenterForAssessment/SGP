# SGP Student Growth Percentiles and Percentile Growth Trajectories/Projections

Functions to calculate student growth percentiles and percentile growth projections/trajectories for students using large scale, longitudinal assessment 
data. Functions use quantile regression to estimate the conditional density associated with each student's 
achievement history. Percentile growth projections/trajectories are calculated using the coefficient matrices derived from the quantile 
regression analyses and specify what percentile growth is required for students to reach future achievement targets.

To install the most recent stable release of the SGP package from CRAN:


```R 
install.packages("SGP")
require(SGP)
```



To install the latest development build of the SGP package from Github:

```R 
install.packages("devtools")
require(devtools)
install_github("SGP", "StudentGrowthPercentiles")
require(SGP)
```

To install from Github you might need: Windows: Rtools (http://cran.r-project.org/bin/windows/Rtools/), OS X: xcode (from the app store),
Linux: apt-get install r-base-dev (or similar)

