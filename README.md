The SGP Package 
===============


Student Growth Percentiles & Percentile Growth Trajectories/Projections
-----------------------------------------------------------------------

The **SGP** Package (SGP, 2013) is an open source package built for the open source **R** software environment (R Development Core Team, 2013). The classes, functions and data within the **SGP** package are used to calculate student growth percentiles and percentile growth projections/trajectories using large scale, longitudinal assessment data. The methodology uses quantile regression to estimate the conditional density associated associated with each student's achievement history. Percentile growth projections/trajectories are calculated using the coefficient matrices derived from the quantile regression analyses and specify the percentile growth required for students to reach future achievement targets.

* Web site: http://SchoolView.github.com/SGP/
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
install_github("SGP", "SchoolView", args="--byte-compile")
require(SGP)
```

To install from Github you might need: Windows: Rtools (http://cran.r-project.org/bin/windows/Rtools/), OS X: xcode (from the app store),
Linux: apt-get install r-base-dev (or similar).

Analysis of data utilizing the **SGP** methodology requires, broadly, two steps: Data Preparation and Data Analysis with these two steps consisting of several substeps:

* Data Preparation
	* Preparation of data into *LONG* format.
* Data Analysis
	* prepareSGP
	* analyzeSGP
	* combineSGP
	* summarizeSGP
	* visualizeSGP
	* outputSGP



Data Preparation
================

The following provides thorough **SGP** data formatting/preparation specifications for
utilizing the utility function of the package (SGP:2013). To help
illustrate these specifications there is an embedded data set, , within
the package. The development team uses this data set frequently to test
new features included in the package. Ensuring your data is set up in
the proper format will minimize later efforts to run analyses. The goal,
if you supply properly formatted data, is to make generation of student
growth data and associated visualizations as easy as *abc*.


Data must be in long format
---------------------------

The first and most fundamental requirement for the data is that it must
be in long, as opposed to wide, format. For our purposes, this means
that each row represents a unique *student* by *content area* by *year*
combination. This uniqueness is relaxed somewhat with the addition of a
`VALID_CASE` variable that defines which case is unique if there are
duplicate records by student, content area and year. Thus, in the final
long file, each student, by content area by year by valid case
identifier must be unique. By contrast, a row in the wide data format
would contain all available information for a single student. For
example, here are the first four rows (only the first 8 columns) of the
sample data:

```R
> sgpData_LONG[1:4,1:7]
ID LAST_NAME FIRST_NAME CONTENT_AREA   YEAR GRADE SCALE_SCORE
1 1000079   Nixon  Daniela MATHEMATICS 2006_2007   8     463
2 1000079   Nixon  Daniela MATHEMATICS 2007_2008   9     519
3 1000079   Nixon  Daniela   READING 2006_2007   8     587
4 1000079   Nixon  Daniela   READING 2007_2008   9     614
```

Notice that the same student is in each row, but that the rows represent
different grades and content area combinations. This is what is meant by *long* formatted data.

Required Variables
------------------

The following list gives the columns that are requires for the
calculation of Student Growth Percentiles and how they should be
formatted (if applicable).

-   `ID` This column contains the unique student identifiers. Values will be set to
	class *character*.

-   `CONTENT_AREA` This column describes the content area for a given
    row. Most datasets would presumably contain *MATHEMATICS* and
    *READING*, but other values are possible. These values must be
    capitalized and match the states’ assessment information contained
    in the **SGPstateData** object that comes embedded within the **SGP** package.
    Please contact @dbetebenner to have assessment data added to this
    object. These values should be of class *character*.

-   `YEAR` This column gives either the academic year (e.g., *2006_2007*
    as in the sample data) or the year in which the assessment took
    place (e.g., *2007*). If the latter form is used, the class of this
    column should be set to *integer*. Hyphens may NOT be used (e.g.,
    *2006-07*).

-   `GRADE` The grade in which the assessment was administered. The
    column of this class should be set to *integer*.

-   `SCALE_SCORE` The assessment scale score for each observation. This
    column’s class should be set to *integer* or *numeric*.

-   `VALID_CASE` This column identifies those students who should be
    included in subsequent analyses (value set to *VALID_CASE*) and
    those that should not be included (value set to *INVALID_CASE*.
    Duplicate cases are often left in the data and flagged as an
    *INVALID_CASE*.

Secondary Columns
-----------------

Although these columns are not required for Growth Percentile analyses,
they are required for Growth to Standard analyses, and/or the
visualization and reporting functionality:

-   `ACHIEVEMENT_LEVEL` The achievement or proficiency category
    associated with each observed scale score. Values in this column
    should be set to *factor*, and should match the assessment program
    information included in contained in the *SGPstateData* object.
    `ACHIEVEMENT_LEVEL` should be a *factor*.

-   `FIRST_NAME` Student first name. A *character* or a *factor*.

-   `LAST_NAME` Student last name. A *character* or a *factor*.

-   `SCHOOL_NUMBER` Unique identifier for the school/institution in
    which a student is enrolled in a given year. Either an *integer* or
    *factor*.

-   `SCHOOL_NAME` Name of the school/institution in which a student is
    enrolled in a given year. A character string or `factor`.

-   `DISTRICT_NUMBER` A unique identifier for the district/educational
    authority in which a student is enrolled in a given year. Either a
    `factor` or `integer`.

-   `DISTRICT_NAME` District/educational authority name in which a
    student is enrolled in a given year. A character string or `factor`.

-   `STATE_ENROLLMENT_STATUS` Binary indicator of whether the student
    was continuously enrolled in the state and should be included in
    summary statistics. Indicator must be a `factor`, preferably with
    informative labels such as those in ; `Enrolled State: Yes` and
    `Enrolled State: No`.

-   `DISTRICT_ENROLLMENT_STATUS` Binary indicator of whether the student
    was continuously enrolled and should be included in district summary
    statistics. Indicator must be a `factor`, preferably with
    informative labels such as those in ; `Enrolled District: Yes` and
    `Enrolled District: No`.

-   `SCHOOL_ENROLLMENT_STATUS` Binary indicator of whether the student
    was continuously enrolled and should be included in school summary
    statistics. Indicator must be a `factor`, preferably with
    informative labels such as those in ; `Enrolled School: Yes` and
    `Enrolled School: No`.

-   `GENDER` DO WE WANT THIS AND OTHER DEMOGRAPHICS? USER CAN CHANGE
    SUMMARY GROUPS IN summarizeSGP.

-   `ETHNICITY` Ethnicity needed for randomNames, so maybe
    these two...

A number of other demographic variables can be provided when creating
summaries.

Variable Name Lookup Table
---------------------

If a user does not wish to rename the columns in their data to match the
conventions used in the `SGP` package listed above, the
`Variable_Lookup_Table` option can be used. The user must supply an
appropriate list of variable names in the `var.names` argument of the
`prepareSGP` function. For example, if a state has a unique student
identifier named \`\`My\_Student\_ID" and an assessment subject variable
named \`\`My\_Subject" (all other variable names match), an example call
to `prepareSGP` would include this argument:

    My_State_Data <- prepareSGP(..., 
       var.names=list(ID="MyStudent_ID", CONTENT_AREA="My_Subject"))

The `var.names` list *must* include all required columns that do not
match the conventions, as well as all secondary columns needed for
summarization and reporting.

Data Analysis
-------------

Once a dataset is properly formatted, a comprehensive analysis can be
conducted using **abcSGP**. An example of the call using the sample data
is below.

    Demonstration_SGP <- abcSGP(sgpData_LONG)

This call returns an object of class *SGP* named `Demonstration_SGP` object which contains student growth percentiles and other information, but it also produces goodness of fit and visualization folders containing files on those two topics.
The function accepts multiple arguments detailed in the 
[documentation](https://github.com/dbetebenner/SGP/blob/master/man/abcSGP.Rd).

Bibliography
------------

Betebenner, D. W., VanIwaarden, A., and Domingue, B. (2013). SGP: An R Package for the Calculation and Visualization of Student Growth Percentiles & Percentile Growth Trajectories.

R Development Core Team (2013). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria.
3-900051-07-0.
