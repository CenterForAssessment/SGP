# SGP 2.1-0.0

## User Visible Changes:

* General bug fixes and meta-data updates to SGPstateData

# SGP 2.0-0.0

## User Visible Changes:

* General bug fixes and meta-data updates to SGPstateData

# SGP 1.9-5.0

## User Visible Changes:

* Thorough update of tests to optimize package performance
* General bug fixes and meta-data updates to SGPstateData


# SGP 1.9-0.0

## User Visible Changes:

* Update of web-based documentation (https://sgp.io)
* Various state meta-data updates
* Adding functionality to return number of years to reach SGP_TARGET returned. Accessible through SGPstateData meta-data currently. See testSGP(3) source code for example.


# SGP 1.8-0.0

## User Visible Changes:

* Many general meta-data updates, performance improvements, and much better web-based documentation (https://sgp.io)


# SGP 1.7-0.0

## User Visible Changes:

* Adding in dual mode for RLI analyses to be performed using RASCH scores (default) or STAR scores
* Better accomodation of duplicates in data. Nonetheless, MUCH better to get rid of duplicates


# SGP 1.6-0.0

## User Visible Changes:

* Added new function 'courseProgressionSGP' which analyzes a long data set to return a list of potential course progressions suitable for SGP analysis.
* Color output added as part of diagnostic messages.
* Added capability to request a subset of students from the cohorts to produce coefficient matrices and calculate SGPs in order to test data, configuration scripts, etc.  The abcSGP, updateSGP, analyzeSGP, and studentGrowthPercentiles functions all now accept the 'sgp.test.cohort.size' argument.  Results from the tests can be returned in the higher level functions by setting the argument 'return.sgp.test.results' to TRUE.


# SGP 1.5-0.0

## User Visible Changes:

* Analyses now export output to txt file contained in Log directory.
* Growth and Achievement Plots now support individual student reports and arbitrary grade level starting points.
* testSGP automatically detects YEAR in SGPdata so annual updates are no longer necessary for tests.


# SGP 1.4-0.0

## User Visible Changes:

* Package accomdates assessment transitions with equating from old to new test and provides robust diagnostics associated with equating.
* Adding ability to generate student reports for examining showing two different tests.
* Goodness of fit plots now provide diagnostic information on floor and ceiling effects.
* SGP package now calculates time dependent student growth percentiles/projections (SGPt).
* Numerous performance improvements (speed and memory use)!


# SGP 1.3-0.0

## User Visible Changes:

* Adding internal getJSON function to studentGrowthPlot_Styles to export student level JSON suitable to interactive student reports.
* Added Illinois meta-data to SGPstateData.
* Adding cuts to projection fan.
* Bumped YEAR up 1.
* analyzeSGP now accepts argument sgp.sqlite - if set to TRUE function will output a temporary SQLite file of essential data from SGP object's @Data slot and use SQLite queries to pull data subsets used in lower-level functions.  This option entails some time overhead for read/write to/from disk, but ultimately provides substantial RAM memory saving during computation (particularly parallel computation).  Default is NULL, in which case @Data slots larger than 1 GB are written to disk (set internally to TRUE).
* Simulation data in .simex.sgp routine of studentGrowthPercentiles is now saved to .sqlite databases as a RAM memory saving measure.
* Results and simulation data used in summarizeSGP to produce summary tables are now exported to temporary a .sqlite file, and utility functions have been externalized.  This provides memory savings, particularly for Windows users.
* PNG versions of bubblePlots can now be produced through visualizeSGP using the bPlot.output argument.


# SGP 1.2-0.0

## User Visible Changes:

* Removed LICENSE and changed copyright to GPL-2.
* Added capability to calculate student growth projections for End of Course testing sequences.
* Added capability to calculate scale score targets to combineSGP and visualize them on student growth plots.
* Added capability to calculate weighted medians and weighted means in summarizeSGP.
* Added Guatemala meta-data to SGPstateData.
* Added argument 'steps' to updateSGP allowing user to specify steps that abcSGP uses as part of updateSGP.
* Added MS baseline coefficient matrices to SGPstateData.
* Corrected error message provided in summarizeSGP for INSTRUCTOR_NUMBER summaries.
* outputSGP output.type='SchoolView' now outputs JSON data for tables.
* Added argument return.norm.group.scale.scores to studentGrowthPercentiles to return score vector of each student associated with their SGP.
* Changed growth trajectory to distinguishing color on growth achievement plots.
* Added argument sgp.cohort.size to studentGrowthPercentiles to control minimum cohort size restriction.
* summarizeSGP now calculates bootstrap standard errors as well as bootstrap confidence intervals.
* outputSGP now includes ability to export data for FINAL YEAR of long format 'LONG_FINAL_YEAR_Data'.
* combineSGP now produces current year targets as well as lagged targets adding arguments 'sgp.projections' and 'sgp.projections.baseline'.
* student growth plot now functions with end of course tests. See testSGP(4) for an implemented example.


# SGP 1.1-0.0

## User Visible Changes:

* Updated Thanks in DESCRIPTION


# SGP 1.0-9.9

## Bug Fixes:

* growthAchievementPlot now renders baseline referenced SGP correctly.


# SGP 1.0-9.8

## User Visible Changes:

* Updated CSEM calculation algorithm and refined code in analyzeSGP to calculate CSEMs for both cohort and baseline referenced SGPs


# SGP 1.0-9.7

## User Visible Changes:

* Changed examples to work with 2012_2013 year of new SGPdata.


# SGP 1.0-9.6

## User Visible Changes:

* Update variable names for Hawaii state data


# SGP 1.0-9.5

## Bug Fixes:

* studentGrowthPercentiles now correctly adjusts length of content.area.progression (when supplied) when num.prior also used.


# SGP 1.0-9.4

## User Visible Changes:

* Updated package SGPdata separating INSTRUCTOR_NUMBER from sgpData_LONG and creating sgpData_INSTRUCTOR_NUMBER as a prototype multiple membership table.
* Removed INSTRUCTOR_NUMBER variables names from the variable name lookup for DEMO.
* Altered the way the package deals with 'multiple.membership.groups'. New slot @Data_Supplemental includes multiple membership supplemental tables (like sgpData_INSTRUCTOR_NUMBER).


# SGP 1.0-9.3

## User Visible Changes:

* Added use.my.coefficient.matrices argument to abcSGP to faciliate updateSGP.
* updateSGP now allows for single argument of 'what_sgp_object' to be supplied allowing for re-calculation of SGPs using embedded coefficient matrices.


# SGP 1.0-9.2

## User Visible Changes:

* Added prior achievement content area to goodness of fit charts showing growth by prior achievement.
* Added two more Economics repeat sequences to baseline matrices for Georgia.


# SGP 1.0-9.1

* Completed embedding of GA baseline matrices into SGPstateData.


## User Visible Changes:

* Integrated YEAR_WITHIN into getPanelData.
* Integrated SGP_Norm_Group_Preference into combineSGP for states with multiple end of course assessments.
* Integrated ME baseline coefficient matrices into SGPstateData.

## Bug Fixes:

* Various minor bug fixes and refinements.


# SGP 1.0-8.0

## User Visible Changes:

* Added YEAR_WITHIN capability to package to better accomodate interim assessment.

## New Features:

* Added YEAR_WITHIN capability to package to better accomodate interim assessment.


# SGP 1.0-7.0

## User Visible Changes:

* Added Professor Yi Shang as author of package.

## New Features:

* Final integration of SIMEX measurement error correction into studentGrowthPercentiles.

## Bug Fixes:

* Fixed bug in as.splineMatrix that was applied to old object of class SGP.


# SGP 1.0-6.0

## New Features:

* Initial integration of SIMEX measurement error correction into studentGrowthPercentiles.


# SGP 1.0-5.0

## New Features:

* Integrate ability to specify preferred SGP using SGPstateData and internal function getPreferredSGP. See GA SGPstateData for an example.

## User Visible Changes:

* Modified two physical science baseline coefficient matrices to EOCT from grade 8.
* Output messages for studentGrowthPercentiles and studentGrowthProjections indicate number of students utilized for the analyses.


# SGP 1.0-4.0

## New Features:

* summarizeSGP now provides the standard deviation of the SCALE_SCORE_PRIOR_STANDARDIZED for each group indicating a level of incoming student homogeneity/heterogeneity.

## Bug Fixes:

* Fixed bug associated with creation of enhanced goodness of fit plots.


# SGP 1.0-3.0

## User Visible Changes:

* Deprecated use of MULTICORE, MPI, and REDIS in favor of PARALLEL.
* Exported previously internal function createKnotsBoundaries allowing for users to generated Knots, Boundaries, Loss/Hoss values separately.
* studentGrowthPercentiles includes new argument for 'return.prior.scale.score.standardize' that generates the variable 'SCALE_SCORE_PRIOR_STANDARDIZED'.
* growthAchievementPlot includes new argument for 'output.format' that allows for both 'PDF' and 'PNG' output.
* Previous growthAchievementPlot argument 'pdf.folder' changed to 'output.folder'.
* analyzeSGP prints goodness of fit plots in two formats: 'PDF' and 'PNG'.
* Added Australia/NAPLAN data to SGPstateData.
* Updated CITATION.


# SGP 1.0-0.0

## New Features:

* Update goodness of fit plots to include an additional representation of growth percentiles by prior achievement level.
* Incorporated updateSGP into SGP package including a new test in testSGP, testSGP(2), utilizing it.

## User Visible Changes:

* Removed parallel functionality for deprecated SNOW and MULTICORE packages, as well as FOREACH implementations of doMC, doMPI, doSNOW and doRedis.
* Removed chunk.size argument from 'studentGrowthProjections' function based upon refined implementation using data.table.
* Changed SGP_LEVEL and SGP_LEVEL_BASELINE to ordered factor from factor.
* SGPstateData is now an environment instead of a list so that updates can be smoothly added.


# SGP 0.9-9.9

## New Features:

* Update of splineMatrix class to include a validity check and update of as.splineMatrix to convert older splineMatrices to current version.


# SGP 0.9-9.8

## New Features:

* Major re-factoring of package involving externalization of previously internal functions for better unit testing.


# SGP 0.9-9.7

## New Features:

* studentGrowthPlot allows arrow.legend.color to be specified in a state specific fashion and controlled from SGPstateData.
* checkSGP converts ID to numeric to take advantage of data.table improvements.
* checkSGP checks for match between levels in ACHIEVEMENT_LEVEL and SGPstateData.
* Added organization abbreviation to SGPstateData.
* Added Move Up Calculations to combineSGP.
* Made major refinements to growthAchievementPlot and visualizeSGP that speeds up the creation of growth and achievement plots.
* Improved baselineSGP configuration for dealing with non-standard content area progressions.
* On *NIX OS types studentGrowthPlot directories are zipped/gzipped and directory removed to minimize space requirements.
* outputSGP converts names back to @Names$name.original.
* Projection fan in student growth plots now projects across multiple year spans.
* Internal capwords function now exported and available for use.
* Added argument sgPlot.zip to visualizeSGP and studentGrowthPlot_Styles to control zip compression of school studentGrowthPlot folders.
* Added shrinkage estimator calculation to summarizeSGP.
* Added PNG and PDF_PIECES output of student growth plots via the visualizeSGP option sgPlot.output.format='PNG' or sgPlot.output.format='PDF_PIECES'.
* splineMatrix class now includes slots for 'Content_Areas' and 'Grade_Progresssion' to accomodate more varied prior score combinations.
* Added argument 'content.area.progression' to studentGrowthPercentiles to accomodate non-uniform content area progressions as priors to be included in the coefficient matrices produced/used.
* Integrated sgp.loss.hoss.adjustment into analyzeSGP.
* Added STAY_UP as part of previous MOVE_UP analyses. combineSGP now calculates CATCH_UP/KEEP_UP/MOVE_UP/STAY_UP.
* Added additional functionality to 'studentGrowthPlot_Styles' allowing for the construction of student growth plot catalogs by instructor.
* Refinements of merges in 'combineSGP' to maximize speed and efficiency.
* Created internal function 'getStateAbbreviation' to handle calculation of state abbreviation with SGP functions based upon supplied sgp_object argument name.
* Added gofSGP function to create more thorough goodness-of-fit plots including results distributed by prior achievement level.
* Added Indiana baseline coefficient matrices to SGPstateData.

## User Visible Changes:

* Updated Hawaii variable name lookup table.
* Zipped files for studentGrowthPlots and outputSGP no longer include full file path.
* Internal capwords function now exported and available for use.
* Removed parentheses in naming of folders when sgPlot.folder.names='name' is supplied.
* Instructor by School Number tables now include EMH_LEVEL aggregations as well.

## Bug Fixes:

* Fixed spacing bug for attaching front page to studentGrowthPlots.
* Corrected logic bug in combineSGP leading to the creation of SGP_TARGET when no projections exist.
* Projection fan cuts calculated across a gap year using a piecewise transformation corrected to accomodate corect increase in years.
* studentGrowthPercentiles and studentGrowthProjections better checks for empty data.
* Corrected naming of baseline coefficient matrices for New Jersey.


# SGP 0.9-9.0

## User Visible Changes:

* Updated URLs in package to github site that hosts SGP code development.
* Added timing to outputSGP and sqliteSGP and integrated outputSGP into abcSGP.
* Baseline SGP tables embedded in SGP object have variable names appended with '_BASELINE'.
* Streamlined growthAchievementPlot production by removing growthAchievementPlot_Styles and integrating its functionality elsewhere.
* outputSGP now exports contents of @Data by default to 'Data' directory.
* Updated SGPstateData with Georgia variable name lookup table.
* studentGrowthPercentiles deals better with holes in data structures where certain grade progressions yield empty data sets.
* baselineSGP analyses from analyzeSGP now accept arguments supplied to content_areas, years, and grades if one wants to filter those for baseline SGP analyses or coefficient matrix generation.
* summarizeSGP does not produces by default, all possible summary tables (controlled by the arugment 'produce.all.summary.tables'). A subset of about 70 summary tables are produced.
* Added baseline coefficient matrices for Georgia to SGPstateData.
* Fixed prepareSGP to better update @Names slot.
* Added Archdiocese of Baltimore to SGPstateData.
* Integrated achievement_level_recode function into prepareSGP.
* Added ability to print studentGrowthPlots for one, four and five content areas.
* Added timing output to outputSGP LONG file production.
* If grade.progression supplied to studentGrowthPercentiles exceeds number of panels available, function now uses the tail of the grade progression up to the number of panels supplied.
* Added 'WIDE_Data' output.type to outputSGP.
* Added variable name lookup table for Maine.
* Updated summarizeSGP to create relevant tables based upon state provided institution levels.
* Added 'use.cohort.for.baseline.when.missing' argument to combineSGP that allows for the use of cohort reference SGPs in place of baseline referenced SGPs when no baseline SGPs exist.
* Added Variable_Name_Lookup for New Jersey to SGPstateData.
* Updated list of contributors.
* Added testSGP function to package to faciliate development and testing of package.
* Added argument 'create.additional.variables' to prepareSGP indicating whether additional variables should be added to @Data (defaults to TRUE).
* prepareSGP now adds variable HIGH_NEED_STATUS to @Data by default indicating students in top and bottom quartile by school, grade, content area and year.
* Changed naming convention between from TESTING_YEAR to CURRENT to apply to all states going forward.
* Tidied up title for goodness of fit plots and modified plots to deal with edge cases where there are not 10 prior score deciles.
* Added variable name lookup for Massachusetts.
* Refined summarizeSGP to better detect ENROLLMENT_STATUS variable in supplied @Data.
* Added argument 'sgp.loss.hoss.adjustment' to studentGrowthPercentiles to adjust SGP calculation for hoss values where ceiling effect impacts SGP calculation.
* Updated loss/hoss values for Hawaii based upon apparent scale change in 2011.
* Modified .year.increment in studentGrowthPercentiles to accomodate baseline SGP analyses.
* Added Washington data to SGPstateData including basseline coefficient matrices.
* growthAchievementPlot not accomodates gaps in years tested if coefficient matrices are available across that gap.
* Updated Idaho knots and boundaries to accomodate 2011 scale scores.
* Added West Virginia variable name lookup table.
* Changed implementation of VALID_CASE to character from factor to better coincide with data.table 1.8.2 requirements.
* Added argument return.prior.scale.score to studentGrowthPercentiles to incorporate the return of the prior scale score for the student.
* studentGrowthPercentiles now accomodates multiple priors from the same year. See examples in 'analyzeSGP' details.
* Updated Kansas metadata in SGPstateData.
* Fixed bug in get.panel.data and get.panel.data.vnames that didn't accomodate missing grades in state testing programs correctly.
* Added function (internal) checkSGP to externalize checks on SGP object confirming it is up to date.
* Added Colorado variable name lookup table to SGPstateData.
* Changed default of outputSGP to export LONG and WIDE data.
* Added CO baseline matrices based upon 5 years of panel data from 2005 to 2009.

## Bug Fixes:

* Added PERCENT_CATCHING_UP_KEEPING_UP to summarizeSGP if SGP_TARGET is present.
* Corrected issues with conversion of factors to characters in summarizeSGP.
* Corrected names.type error in RI and NH lookup tables.
* Corrected incorrect timing in visualizeSGP due to over writing time.started in function.
* Rewrote merge in combineSGP that adds data to @Data.
* Made correction to cutscore year lookup code in growthAchievementPlot and prepareSGP.
* studentGrowthPercentiles checks for CSEMs and ensures availability by content area.
* Fixed bug in way state name is detected in argument supplied as part of data name.
* Removed redundant years, content_areas specification in abcSGP.
* analyzeSGP not checks for content area specific coefficient matrices and only calculates baseline referenced results for those content areas.
* Supplied panel.data.vnames now subsets only those names in supplied panel data.
* Refined of merge of sgp targets in combineSGP to avoid memory copy of sgp_object.
* studentGrowthPlot function now detects year dependent loss/hoss values.
* summarizeSGP now checks for existence of ACHIEVEMENT_LEVEL_PRIOR before attempting to calculate it.
* summarizeSGP better checks for inclusion variables.
* Corrected various bugs in bubblePlot Style 100 introduced with @Names data.frame.
* Modified analyzeSGP to work with data.table changes in 1.8.2.


# SGP 0.9-0.0

## New Features:

* Added Hawaii data to SGPstateData and updated Nevada transitional cutscores in Mathematics.
* analyzeSGP now passes projcut.digits to studentGrowthProjections to control digits based upon state level configurations.
* visualizeSGP now checks stateData for grades AND years to be reported in studentGrowthPlots. Default is most recent 5 years
* studentGrowthPlot accomodates situations where there is a gap between grades (e.g., 8 to 10) and the state calculates 2 year SGPs.
* Added tag to SGPstateData for earliest year reported to accomodate possibility of reporting only certain years for a state assessment system (e.g., Arizona).
* ACHIEVEMENT_LEVEL_PRIOR and CATCH_UP_KEEP_UP_STATUS_INITIAL, used for lagged projections, are now calculated as part of analyzeSGP and not combineSGP. This was required in order to allow for growth to standard across holes in grades progression (e.g., 8th to 10th grade).
* Added addition argument to summarizeSGP (growth_only_summary) that calculates summaries using only student with growth percentiles. This makes bubblePlots better for research investigations (but not as relevant for accountability displays).
* Added parallel growthAchievementPlot figure production within visualizeSGP.
* Added population size to goodness of fit plots.
* Changed Grade to Grade Progression in goodness of fit plots to better identify groups being analyzed.
* Added median SGP line to Style 100 of bubblePlot_Styles.
* prepareSGP now accepts data.frame with arguments names.original, names.sgp, names.type, names.info to enhance analyze flow through SGP analyses.
* Removed doSMP parallel configuration due to removal of doSMP from CRAN.
* Added 'grade.progression.labels' argument to studentGrowthPercentile to faciliate coefficient matrix storage in Coefficient_Matrix list.
* prepareSGP now creates knots and boundaries for embedding in SGPstateData and utilizes those quantities in currently executing calculations.
* Added additional metadata to @Names slot: names.provided, names.sgp, names.type, names.info, names.output.
* prepareSGP creates @Names meta data.
* Added sqliteSGP to SGP package and integrated that into the outputSGP functionality.
* Added additional Mississippi state data to SGPstateData.
* For studentGrowthPlot production in parallel, jobs are submitted by school size in decreasing order to increase performance.
* Added analytic median SGP confidence interval calculation to bootstrap confidence interval. Analytic confidence intervals calculated by default for all groups.
* Added New Hampshire Variable Name Lookup to SGPstateData.
* Added startParallel and stopParallel to better externalize parallel processing implementation.
* Made some slight tweaks to capwords to better accomodate all caps situations.


## User Visible Changes:

* Consolodated warning messages to provide better information, especially when working in parallel configurations.
* Updated Massachusetts grade 10 cutscores between proficient and advanced achievement levels.
* Lagged projections are only calculated for students with an SGP within analyzeSGP. This eliminates the necessity for the sgp.target.to.NA argument and feature in combineSGP.
* sgp.target.to.NA feature eliminated from combineSGP for reasons explicated in previous remark.
* Changed "Phone Number" to "Contact" on studentGrowthPlot and made associated changes of label in SGPstateData.
* Updated NECAP CSEMs for 2011_2012 in SGPstateData.
* Added Hawaii baseline coefficient matrices to SGPstateData.
* Added test to analyzeSGP to see if CSEMs exist in SGPstateData.
* Added growthAchievementPlot_Styles to better accomodate utilization of growthAchievementPlot in parallel.

## Bug Fixes:

* studentGrowthPlot in visualizeSGP now filters on GRADES that correspond to VALID_CASE.
* Fixed issue when projections occur across skipped grades. Made major refinements to analyzeSGP in the process.
* Updated code to conform with changes in data.table 1.7.8.
* bPlot.format no case insensitive.
* Switch installed.packages to find.package to test for pdf2 installation.
* Modified subsetting of get.panel.data inside studentGrowthPercentiles to avoid data.table seg fault.


# SGP 0.8-0.0

## New Features:

* Separated sgpData and sgpData_LONG into new package SGPdata.
* Changed name of stateData to SGPstateData.
* Added System_Type slot to stateData to indicate whether state uses 'Cohort Referenced', 'Baseline Referenced', or 'Cohort and Baseline Referenced'.
* Added Test_Vendor slot to stateData.
* Added California data to stateData.
* Added New York baseline coefficient matrices to stateData.
* Modified stateData to include "Scale_Change" slot in "Assessment_Program_Information" to indicate content area and year when scale has been changed.
* Modified stateData and growthAchievementPlot to accomodate states that change vertical scales from one year to next (e.g., Arizona Mathematics).
* Improved parallel implementation of baseline SGP coefficient matrix calculation.
* Added ability to accomodate year specific knots and boundaries supplied in stateData.
* Added Arizona knots and boundaries to stateData.
* Began implementing changes utilizing new pointer features in data.table 1.7.3.
* Added lag.increment argument to studentGrowthProjections to accomodate state with scale and cutscore changes.
* Added knots.boundaries.by.panel argument to studentGrowthProjections to accomodate knots and boundaries calculations by panel (instead of pooling grade level data across panels).
* prepareSGP augments instead of overwrites SGP_Package_Version and Date_Prepared keeping more meta data on the construction of the object.
* Added function as.splineMatrix to faciliate conversion of older coefficient matrices to S4 class splineMatrix.
* Added test of is.splineMatrix to prepareSGP and conversion of coefficient matrices to splineMatrix class otherwise.
* Added max.order.for.percentile argument ot studentGrowthPercentiles to limit number of priors used in analyses (which could already be accomplished by a variety of other means).
* Added sgp.target.to.NA argument to combineSGP (defaults to TRUE) to change SGP_TARGET and SGP_TARGET_BASELINE to NA when SGP is NA (e.g., for retained students). Thanks for Ayaka Nukui for observation and suggestion.
* Added sgPlot.baseline arguement to visualizeSGP to control whether cohort or baseline referenced SGPs are depicted in student growth plots.

## User Visible Changes:

* bubblePlot doesn't load package pdf2 with R version 2.14 or greater until bugs are reconciled with data tips. Use of pdf2 requires R version 2.13.2 or earlier.
* analyzeSGP calls to studentGrowthProjections for cohort and lagged projections checks for max.order.for.progression with Scale_Change slot in stateData. This will be required when states switch to either assessment consortium.
* Modified growthAchievementPlot call in visualizeSGP to adjust max.order.for.progression based upon previous two changes.
* Added argument gaPlot.max.order.for.progression to growthAchievementPlot allowing user to specify maximum order for growthAchievementPlot projection. Useful for states changing scales.
* Improved parallel implementation of baseline SGP coefficient matrix calculation.
* studentGrowthProjections now checks if current year's data fall within loss.hoss boundaries and corrects if they don't.
* Better integrated parallelization routines into analyzeSGP.
* Updated analyzeSGP with doSMP to better faciliate parallel analyses in Windows environments.
* Continued to optimize studentGrowthPercentiles and studentGrowthProjections functions for better memory mangement and speed.
* Created a single baselineSGP function for both use cases previously provided by the baselineSGP and baselineSGPLong functions.
* bubblePlot appropriately scales labels provided by argument bubble_plot_data.BUBBLE_CENTER_LABEL.
* When knots and boundaries are automatically calculated, only prior scores are used for pooling purposes. Previously both prior and current scores were pooled.
* Updated Idaho meta data in stateData including knots, boundaries and organizational information.
* studentGrowthProjections now includes arugment calculate.sgps (default to TRUE) that is used to bypass calculations by analyzeSGP in certain special cases (e.g., scale change in state).
* Updated Maine metadata to include knots and boundaries.
* Changed internal naming implementation of SGP_BASELINE in combineSGP.

## Bug Fixes:

* Refined combineSGP to better accomodate irregular content area and year specifications for both cohort and baseline referenced SGPs.
* Corrected bug in bubble_plot_configs.BUBBLE_COLOR where specifying a single color didn't provide the correct color.
* studentGrowthProjections now checks for grades in cutscores based upon the supplied subject given in sgp.labels.
* Modified sgp.config in analyzeSGP to correctly work with grades supplied as an argument (Thanks to Ayaka Nukui for pointing this out).
* Argument grades now correctly passed from abcSGP to analyzeSGP (Thanks to Ayaka Nukui for pointing this out).
* Changed incorrect cutscore in state data for advanced level in DEMO data from 509 to 510.
* studentGrowthProjections start/stop message now reports correct grade progression when limited number of priors is selected.


# SGP 0.7-2.0

* prepareSGP now adds an updated Version to an SGP object that is passed to prepareSGP.
* In cases where stateData cutscores change by year, studentGrowthProjections correctly chooses annual cutscores to use for projections.
* Corrected bug in bubblePlot_Styles that mishandled situation when no ACHIEVEMENT_LEVEL_PRIOR is present.
* Updated/correct abcSGP man page.
* Refined file naming in bubblePlot_Styles and growthAchievementPlot to change spaces to underscores.
* Updated New York cutscores in stateData.
* Added Arizona data to stateData.
* Added NECAP CSEMs (2010 and 2011) to stateData.
* Added Rhode Island Baseline Coefficient Matrices to stateData and added additional Rhode Island data.
* Refined abcSGP to not calculate simulated SGPs if missing in stateData for supplied state.
* Changed naming of saved baseline coefficient matrix object.
* Corrected bug with SCALE_SCOREs and TRANSFORMED_SCALE_SCOREs in growthAchievementPlot.
* Modified studentGrowthPercentiles to test for missing years in provided CSEM data.
* Refined .mergeSGP in analyzeSGP to increase performance.
* Made slight modification to arguments determining whether simulated SGPs will be calculated.
* Refined rbind.all in combineSGP to accomodate different annual SGP data calculations (e.g., CSEMs in 2011 but not 2010).
* Created arguments to analyzeSGP: sgp.percentiles.baseline.max.order, sgp.projections.baseline.max.order and sgp.projections.lagged.baseline.max.order to control baseline SGP order (defaulting to 3).


# SGP 0.7-1.0

* Updated Nevada state data for low, typical, and high cutscores.
* Added "Percentile Growth Trajectories" to package title.
* Rectified argument passing between abcSGP and analyzeSGP for parallel.config argument.


# SGP 0.7-0.0

* ETHNICITY and GENDER variables are no longer required to create anonymized (random name) student growth plots. Dummy values are created if missing.
* Began outputSGP as a function to faciliate output of SGP results in common formats.
* Added robust parallelism to analyzeSGP to better facilitate analysis of large scale data sets on multi-core/multi-cpu environments.
* Added functionality to combineSGP to merge in baseline student growth percentiles and baseline student growth percentile targets.
* Added functionality to analyzeSGP to calculate baseline student growth projections and lagged baseline student growth projections.
* Fixed a bug preventing use of provided CSEM variable when supplied as an addition variable in the supplied data.frame.
* Fixed a bug in visualizeSGP for studentGrowthPlot production when sgPlot.years is supplied.
* Refined scale score axis span and labels in growth achievement plot.
* Changed goodness.of.fit default to TRUE with the calculation of baseline student growh percentiles.
* Added state test abbreviation to main title of bubble plots.
* Refined analyzeSGP and baselineSGP to deal with non-consecutive grades when calculating baseline referenced student growth percentiles.
* Added Utah data to stateData.
* Fixed naming issue in bubblePlot_Style 2.
* Fixed bugs impacting states with non-uniform cutscores by year in bubblePlot_Styles and studentGrowthPlot (Thanks Juan D'Brot).
* Fixed a bug preventing users from using their own supplied list of cutscores (Thanks Katherine Castellano).
* Added baseline coefficient matrices for Massachusetts to stateData and changed cutscores to match g-theta values used in SGP calculations.
* Added baseline coefficient matrices for Nevada to stateData.
* Added dummy instructor numbers, weights, and enrollment status to sgpData_LONG.
* Changed "Date" slot to "Version" slot in splineMatrix class so that SGP version meta-data was available and consistent with SGP class.
* Improved goodness of fit color function that avoids errors when empty cells (i.e., color=NA) occur because of badly misfit data.


# SGP 0.6-1.0

* Added check on file name to bubblePlot to remove forward slashes "/".


# SGP 0.6-0.0

* Removed modulo score transformation from stateData as piecewise.transform takes care of this internally.
* Changed default of summarizeSGP to calculate multi-year summaries across the 3 most recent years instead of all years except the first two.
* Modified visualizeSGP to better faciliate mass production of studentGrowthPlots with parallel back end.
* Added studentGrowthPlot_Styles to separate data manipulation from report formatting.
* Changed default sgPlot.demo.report in visualizeSGP from TRUE to FALSE.
* Added is.SGP to test for SGP class.
* Added Version slot to class SGP indicating Version of SGP package used to create SGP object and date object was created.
* Modified growthAchievementPlot to depict regions showing college/career readiness if cutscores are available in terminal grade of growth and achievement chart.


# SGP 0.5-0.0

* Modified functionality to calculate confidence intervals for SGP. calculate.confidence.intervals argument now accepts state acronym and supplied CSEM variable as part of panel.data.
* Added Mississippi data to stateData.
* Refined and corrected minor bug in .get.config in analyzeSGP.
* Improved sgpPlot.cleanup to avoid removing files unintentionally.
* Refined parallel implementation in analyzeSGP.


# SGP 0.4-6.0

* Worked out bug in production of studentGrowthPlots for students with completely missing data.
* Added to and corrected stateData for West Virginia.


# SGP 0.4-5.0

* Added enhanced check to built in knots and boundaries for states.
* Fixed a bug in studentGrowthProjections when chunk.size is a perfect divisor of the number of rows of the data.
* Refined studentGrowthPlot to accomodate annual cutscore changes in states with a vertical scale (e.g., West Virginia). Thanks Juan D'Brot WVDOE.


# SGP 0.4-0.0

* Fixed a bug in visualizeSGP that didn't transform projection cuts for the studentGrowthPlot to the transformed scale when necessary.
* Improved look of two panel student growth plot.
* Improved header to bubblePlots. Thanks to Dr. Sharon Schattgen (MO) for the suggestion.
* Added NAMESPACE to package.
* Modified DESCRIPTION file to accomodate Authors@R requirements.
* Updated West Virginia stateData.
* Added Minnesota to stateData.
* Made slight tweaks to growthAchievementPlot title.
* Added ability for user to supply CSEMs as a separate variable for SGP confidence interval calculations.
* Removed eval(eval() calls used for data.table based upon refinements in data.table package.
* CATCH_UP_KEEP_UP_STATUS is now a factor instead of (wrongly) a character.
* CATCH_UP_KEEP_UP_STATUS_INITIAL variable is now a factor instead of (wrongly) an ordered factor.
* studentGrowthPlot and growthAchievementPlot now accommodate the existence of both non-vertical and vertical scales for different content areas in the same state assessment system (e.g., Connecticut).
* Fixed exact.grade.progression argument to studentGrowthPercentiles. When set to true, function will calculate coefficient matrix and SGPs (if requested) for just that progression.
* Changed name of Variable_Name_Lookup slot in SGP class to Names.
* Made minor modifications to growthAchievementPlot to improve the look of growth achievement plots.
* Made minor modifications to studentGrowthPlot and visualizeSGP to improve the look of student growth plots.
* Updated student growth projections function to allow for skipped percentiles.
* Added bubblePlot_Style 10 which shows district schools highlighted against the state.
* Made minor modifications to the bubblePlot function allowing better highlighting in the presence of hundreds of bubbles.
* Coefficient matrix objects returned from studentGrowthPercentiles are S4 class splineMatrix with Knots and Boundaries and Date slots.
* Added wrapper function baselineSGP to facilitate baseline SGP analyses (i.e. multiple cohort norm groups).
* Added production of baseline referenced coefficient matrices and student growth percentile functionality to analyzeSGP.


# SGP 0.3-6.0

* Updated state data to include New Haven School District specific fields including fixed knots and boundaries.
* Updated Wisconsin state data to produce growthAchievementPlots and studentGrowthPlots.
* growthAchievementPlot now filters out grades not in stateData Grades_Reported.
* Changed argument max.forward.progression to max.forward.progression.years and added argument max.forward.progression.grade.


# SGP 0.3-5.0

* Corrected an offset of 1 in studentGrowthProjections (Thanks for Marie Huchton for finding discrepancies).
* Improved header in growthAchievementChart.


# SGP 0.3-0.0

* Added an example to studentGrowthPercentiles showing how embedded coefficient matrices can be used to calculate student growth percentiles.
* Lots of little bug fixes in visualizeSGP, studentGrowthPlt, bubblePlot, and growthAchievmentPlot, particularly when scale is transformed.
* In analyzeSGP, the automatic configuration has been improved to better select grade progressions associated with the provided data.
* Began adding functionality to use parallel backends from using different parallelization implementations (SNOW, FOREACH, MULTICORE).
* studentGrowthProjections now accomodates year dependent cutscores (in situations where states change cutscores).
* piecewise.transform function accomodates year dependent cutscores. This impact both studentGrowthPlots and growthAchievementPlots when scale score transformations are employed to view SGP results.


# SGP 0.2-1.0

* Added functionality to auto-detect state names based upon SGP object name convention: State_Data (e.g. ,Colorado_Data).
* Fixed bug allowing users to specify multiple sgp.summaries in summarizeSGP.
* Corrected error in achievement level labels in Nevada state data.


# SGP 0.2-0.0

* prepareSGP returns object of class SGP unaltered and prepares data.frames as before.
* abcSGP now passes supplied years and content_areas arguments to combineSGP to allow for year/content area specific merges. This functionality will assist in updating results from year to year.
* combineSGP now accepts arguments for years and content_areas allowing for year/content area specific merges. This functionality will assist in updating results from year to year.
* Improvement of bubblePlot to better represent situations with hundreds/thousands of bubbles.
* Inclusion of growthAchievementPlot that provides plots like that on Betebenner (2009) Educational Measurement: Issues and Practice Article.
* Modification of visualizeSGP to allow for automatic use of growthAchievementPlot.
* Added arguments to studentGrowthPercentiles and studentGrowthProjections (print.time.taken) to allow user to turn off time.taken message. Defaults to TRUE.
* Changed splinefun method for CSEM based confidence interval construction to "natural" due to highly non-monotonic behavior with Wisconsin CSEMs.
* Added argument to bubblePlot "bubble_plot_configs.BUBBLE_COLOR_GRADIENT_REVERSE" allowing user to reverse color gradient. Defaults to FALSE.
* Changed default summaries in abcSGP to include summary calculations involving ACHIEVEMENT_LEVEL_PRIOR (such as prior percent at/above proficient).
* Implemented bug fix associated with data.table in combineSGP function that simplifies data.table implementation of catch-up/keep-up targets.
* Supplying a non-traditional/sequential grade.progression argument to studentGrowthPercentiles now removes (unless setting argument drop.nonsequential.grade.progression.variables=FALSE) supplied panel.data columns from "holes" in the supplied grade.progression.


# SGP 0.1-0.0

* Implementation of SGP class.
* Specified state specific percentile trajectories for student growth projections and stipulated max.forward.progression of 3 years for these trajectories.
* Fixed bugs related to bootstrap calculating confidence intervals for all groups instead of those specified.
* Completed reportSGP.
* Added New Jersey to stateData.


# SGP 0.0-8.5

* Changed analyzeSGP so that it calculates plus/minus 1 standard error confidence bands on individual SGPs by default.
* Added bootstrap confidence intervals to summarizeSGP.
* Added West Virginia to stateData.
* Added Nevada knots and boundaries to stateData.
* studentGrowthPercentiles can now access CSEM by year from stateData if available.
* Consolidated timing messages in studentGrowthPercentiles and studentGrowthProjections at end of function.


# SGP 0.0-8.0

* Implemented more intelligent error handling for missing arguments in studentGrowthPercentiles and studentGrowthProjections passed from analyzeSGP.
* Change default argument for simulate.sgps in analyzeSGP to TRUE.
* Added Georgia and Nevada to stateData.
* combineSGP now creates CATCH_UP_KEEP_UP_STATUS and CATCH_UP_KEEP_UP_STATUS_INITIAL variables if requesting lagged projections.
* Parallelized analyzeSGP using foreach.
* Fixed incorrect naming of percentile cuts in studentGrowthProjections function.
* Achievement Level in sgpData_LONG is now an ORDERED factor.
* Corrected numerous bugs in studentGrowthProjections (Thanks to New Hampshire Department of Education/Deb Wiswell and Tim Kurtz) that led to incorrect catch-up/keep-up results.


# SGP 0.0-7.0

* Changed LICENSE to Creative Commons CC BY-NC-SA 3.0 and CC BY-SA 3.0 for graphics producing and non-graphics producing functions, respectively.
* combineSGP now calculates SGP targets based upon catch-up/keep-up to proficient growth-to-standard recommendations.
* Cleaned up documentation for analyzeSGP, combineSGP, and summarizeSGP.
* Fixed numerous bugs in analyzeSGP, combineSGP, and summarizeSGP.
* Added Missouri, New York, Colorado, and Massachusetts CSEMs to stateData.
* Added two graphical functions bubblePlot and studentGrowthPlot to the package.
* Added fields to sgpData_LONG including IEP_STATUS, DISTRICT_NUMBER, DISTRICT_NAME, SCHOOL_ENROLLMENT_STATUS, DISTRICT_ENROLLMENT_STATUS, STATE_ENROLLMENT_STATUS to better reflect the structure of state data sets.


# SGP 0.0-6.9

* Added additional argument to studentGrowthPercentiles, growth.levels, allowing user to get a variable indicating the level of growth based upon state or user specified cuts and labels.
* Changed my.grade argument in sgp.labels to my.extra.label to better reflect nature of argument. This argument is now available in use.my.knots.boundaries and use.my.coefficient.matrices in both the studentGrowthPercentiles and studentGrowthProjections functions.
* Changed output names of studentGrowthProjections results to better indicate GRADE.
* Changed compression on stateData file to xz.
* Changed default values of projection.unit and projcut.digits in studentGrowthProjections to GRADE and 0, respectively.
* Added wrapper functions prepareSGP, analyzeSGP, combineSGP, and summarizeSGP to facilitate operational SGP analyses.
* use.my.knots.boundaries with studentGrowthProjections now allows state to be supplied.
* Added time elapsed to studentGrowthPercentiles and studentGrowthProjections print out.
* Fixed bugs and updated documentation/examples.
* Added requirement to data.table depends for version >= 1.5.3.
* Changed default of percentile.trajectory.values from 1:99 to NULL (which seemed much more sensible).


# SGP 0.0-6.8

* Includes additional dummy data set, sgpData_LONG, to be used in upcoming operational examples.
* Updated documentation.
* studentGrowthPercentiles outputs SGPs as integer instead of numeric.
* studentGrowthProjections outputs SGP cuts as integer instead of numeric.
* Fixed bug on how studentGrowthPercentiles handles panel.data supplied as a matrix.


# SGP 0.0-6.7

* Added functionality to calculate confidence intervals (calculate.confidence.intervals) of student growth percentiles using CSEMs from state assessment data.
* Added argument print.sgp.order to request that the order of the student growth percentile be provided as well as the student growth percentile.
* Integrated data.table functionality into studentGrowthPercentiles function to significantly increase speed of some calculations.
* Removed INDEX file from package.
* studentGrowthPercentiles now converts all input scores to numeric.
* Added data to stateData file.


# SGP 0.0-6.6

* For use.my.knots.and.boundaries user can now supply a two letter state acronym to access embedded, baseline knots and boudaries compiled from work with states.
* Fixed goodness of fit printouts so that they show appropriate number of digits.
* Fixed DESCRIPTION and eliminated NAMESPACE file.
* Added extensive documentation to stateData file and fixed typos.
* Updated documentation.
* Added date() to finishing message when studentGrowthPercentiles completes an analysis.
* Added exact.grade.progression option to studentGrowthPercentiles to accomodate rare situations where one wants the exact grade progression supplied to be run.


# SGP 0.0-6.5

* Added my.grade to sgp.labels to accomodate situations where my.year and my.subject are not unique.
* Fixed bugs associated with use of data.tables.
* Changed license to GPL-3.
* Fixed bug associated with argument print.other.gp=TRUE.

# SGP 0.0-6.4

* Fixed bug associated with goodness-of-fit table production when no SGPs are requested.
* Fixed studentGrowthPercentiles function to work with character grade levels as well as numeric.


# SGP 0.0-6.3

* Fixed issue with argument calculate.sgps in studentGrowthPercentile function.


# SGP 0.0-6.2

* Cleaned up documentation.
* Corrected data.table bug in studentGrowthProjections.


# SGP 0.0-6.1

* Fixed scoping issue with data.table implementation in studentGrowthProjections.


# SGP 0.0-6

* studentGrowthPercentiles and studentGrowthProjections function now produce a list with results (Knot_Boundaries, Coefficient_Matrices, Goodness_of_Fit, SGPercentiles, SGProjections) embedded.
* studentGrowthPercentiles and studentGrowthProjections currently accept any number of panels and no longer limited to datasets of at most 8 panels.
* studentGrowthPercentiles now accepts argument panel.data.vnames allowing user to specify a subset of variables from supplied data for analysis.
* studentGrowthPercentiles now accepts argument grade.progression allowing user to specify a not necessarily consecutive grade progression to perform analyses.
* studentGrowthPercentiles now accepts argument use.my.coefficient.matrices allowing user to specify pre-calculated coefficient matrices for student growth percentile calculation.
* studentGrowthPercentiles now automatically calculates maximum number of priors unless when using the grade.progression argument.
* studentGrowthPercentiles can calculate student growth percentiles on panel data without any filtering on columns/fields.
* studentGrowthPercentiles now defaults to providing NO percentile cuts associated with each conditional distribution. The function used to provide 1st, 35th, 65th, and 99th percentile cuts.
* Knots_Boundaries now also includes a field for loss (lowest obtainable scale score) and hoss (highest obtainable scale score) named "loss.hoss".
* Boundaries that are automatically calculated by the function now extend 10 percent beyond the range to better accomodate scale extension over time. Previous was 1 percent.
* studentGrowthPercentiles now accepts boolean argument calculate.sgps controling whether function actually produces student growth percentiles (possibly just stopping after calculating coefficient matrices)
* studentGrowthPercentiles now accepts argument knot.percentile.cuts allowing user to specify the quantile cuts used for knot calculation. Defaults to c(0.2,0.4,0.6,0.8).
* studentGrowthPercentiles now accepts argument sgp.quantiles allowing user to specify the quantiles used for studentGrowthPercentile calculations (i.e., not always percentiles)
* studentGrowthProjections projects out an arbitrary number of grades/years/time periods.
* studentGrowthProjections no longer accepts argument num.panels, num.prior.scores, max.num.scores and instead uses the arguments grade.progression and max.forward.progressionand tries to internally select maximum order possible for a students percentile.growth projection/trajectory.
* studentGrowthProjections now accepts argument performance.level.cutscores that can utilize state assessment cutscores available within the SGP package.
* studentGrowthProjections now accepts argument projection.unit that allows percentile growth projections/trajectories to be displayed in terms of either GRADE or YEAR.
* Embedded goodness_of_fit plots are Grid grobs (Grid graphical objects) that can be printed as PDFs (see help for code to produce PDFs of goodness.of.fit plots).
* sgpData, the included longitudinal student assessment data file, has been expanded to 5 panels to better illustrate SGP functionality.
* Includes list stateData to provide state specific information for analyses and reporting. stateData currently includes only performance level cutscores.
* Updated help file to reflect changes in functions.


# SGP 0.0-5

* Moved CITATION file to inst directory
* Fixed minor bugs in studentGrowthPercentiles function.


# SGP 0.0-4

* Added LICENSE file including details on the Creative Commons Attribution-Noncommercial-Share Alike 3.0 United State License.
* Added Goodness-of-fit argument/option/capability to studentGrowthPercentiles function.
* Incorporated vignette.
* Cleaned up documentation.
* Increased percentile growth trajectory/projection to four years from three.
* Fixed minor bugs in studentGrowthProjections (Thanks to Bob Lee for finding these).


# SGP 0.0-3

* Included option/argument in studentGrowthPercentiles function to avoid quantile crossing based upon Dette & Volgushev (2008)
* Cleaned up minor errors in documentation.
* Added optional argument, convert.using.loss.hoss, to studentGrowthPercentiles to convert percentile cuts extended below/above the lowest obtainable scale score/highest obtainable
    scale score (loss/hoss) to the loss/hoss (defaults to TRUE).


# SGP 0.0-2

* Updated Contributors.
* Changed rounding of percentile cuts used for growth percentile calculation to 5 decimal places.
* Changed rounding of automatically produced B-spline knots to 2 decimal places.
* Extended range of automatically produced B-spline boundaries.
* Added optional argument, percuts.digits, to studentGrowthPercentiles to allow specification of rounding digits for growth percentile cuts (defaults to 2).
* Added optional argument, percentile.trajectories, to studentGrowthProjections allowing the function to return 1, 2, and 3 year student percentile trajectories based upon supplied percentile values
    (defaults to 10th, 35th, 50th, 65th, and 90th).
* Added optional argument, projcuts.digits, to studentGrowthProjections to allow specification of rounding digits for percentile trajectory cuts (defaults to 2).


# SGP 0.0-1

* Package Released.
