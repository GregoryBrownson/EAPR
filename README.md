# EAPR

This github repository contains the development of an R package for supporting empirical asset pricing research, or EAPR for short. To install the package, execute the following command:
```
devtools::install_github("GregoryBrownson/EAPR")
```

## Introduction
A major effort in empirical asset pricing research (EAPR) is the initial stage of gathering the data, cleaning and filtering it, and then formatting it in a way that simplifies further statistical analysis. This process, when done properly, takes a large portion of a researcher’s time when it would be better spent on doing the actual analysis. By developing an EAPR package that automates much of the data import, cleaning, filtering and standardization process, a substantial fraction of the researcher’s time will be saved, while essentially standardizing a significant portion of the data gathering and management aspect of asset pricing research. We expect this last aspect to support the reproducibility of EAPR research by academics and financial professionals. Our over-arching goal is to make the EAPR package an ideal support tool for the wide range of asset pricing research as described in Ball, Engle and Murray (2016). The initial version of the package will work very effectively with asset price, returns and factors (exposures) data delivered by Wharton Research Data Services (WRDS), a major source of empirical data for academic asset pricing researchers. The software tools to extract data from WRDS will be designed to be easily adapted to use with other data sources.

## Setup
In order to use the package, you will need an account with WRDS and access to the The Center for Research in Security Prices (CRSP) equities database, the S&P Capital IQ Compustat database, and the CRSP/Compustat merged database. If you have this access, then you will need to set up the **pgpass** file on your computer. Please go to [this page](https://wrds-www.wharton.upenn.edu/pages/support/programming-wrds/programming-r/r-from-your-computer/) on the WRDS website and follow the instructions under the section titled "Initial Setup - The .pgpass / pgpass.conf File." If you are a Windows user, typing **%APPDATA%** may bring you to the ".../AppData/Roaming" folder. If this happens then create the folder 'postgresql' there and follow the rest of the instructions for creating the **pgpass.conf** file. For Linux users, be sure to put the **.pgpass** file in your local user directory, not in the root directory. Once the package is installed and the **pgpass** file is created, then we are ready to extract the data and begin our analysis.

## Extracting the data
Instead of having to use SQL or the online interface to extract data, we have created a single method which will extract the data from WRDS databases, compute the variables, and return an object containing a data.table of the results. The function which does all this is simply called `extract`. However, the run time of this function is fairly long due to downloading a large dataset and processing power required to compute each variable. The function and its parameters are
```
  extract(username,
          src = "wrds", # Only interfaces with WRDS, but we are looking into other options
          fundamental.var = c("ME", "BE", "BE/ME", "A/ME", "A/BE", "OP", "INV", "E/P", "CF/P", "D/P"),
          from            = as.Date("1963-07-31"), # Default start date
          to              = as.Date(paste0(format(Sys.Date() - years(1), "%Y"),"-12-31")),
          periodicity     = 'M', # Options will be D - daily, W - weekly, M - Monthly (only monthly available)
          rebalance.freq  = 'A', # Options will be A - annual, S - semiannual, Q - quarterly (only annual available)
          drop.excess     = T, # Boolean to drop extra variables extracted from wrds database
          preceding       = 60,
          min.prec        = 0.4)
```
Here is an explanation of each parameter:
- **username**: Username for database
- **src**: Source of database
- **fundamental.var**: Fundamental variables to be computed/extracted
- **from**: Starting date of time series
- **to**: End date for time series
- **periodicity**: frequency of the data
- **rebalance.freq**: How often portfolios are rebalanced
- **drop.excess**: Drop variables used to calculate fundamentals and technicals
- **preceding**: Number of preceding periods to consider for technical variables
- **min.prec**: Multiply by preceding to determine the minimum number of preceding periods necessary to compute technical variables

## Filters

## Fama MacBeth Regressions

## Quantile Portfolios

## Replicating Fama French 1992
In the future, I will be adding a script to provide a practical example of EAPR's use cases by replicating some of the methodology from Dr. Eugene Fama and Dr. Kenneth French's 1992 paper, "The Cross-Section of Expected Stock Returns."

## Reporting bugs and suggested features
To help improve our package, please follow the following instructions for submitting bug reports and/or feature requests.

#### Guidelines for bug reports:

We use [GitHub issues](https://guides.github.com/features/issues/) to track and solve potential bugs in our package. When submitting your bug report please:

1. Use our package's [GitHub issue search](https://github.com/msalibian/RobStatTM/issues) to check
whether your issue / bug has already been reported.

2. Check if your issue / bug has been fixed by trying to reproduce it using the latest version of the package.

3. Isolate the problem by creating a **minimal reproducible example** (see below)

4. Create an [issue](https://guides.github.com/features/issues/) for this repository. Refer to [this page](https://help.github.com/en/articles/creating-an-issue) for instructions on how to create a GitHub issue.

A good bug report should not require others to contact you to find more information. Please
try to be as detailed as possible in your report. What is your environment? What steps will
reproduce the issue? What outcome did you expect and what outcome did you get?

###### Example:

> A short and descriptive bug report title
>
> A summary of the issue and the OS environment in which it occurs.
> Include the steps required to reproduce the bug.
>
> 1. This is the first step
> 2. This is the second step
> 3. Further steps, etc.
>
> Any other information you want to share that is relevant to the issue being
> reported. This might include the lines of code that you have identified as
> causing the bug, and potential solutions.

##### Minimal reproducible examples

(This section is adapted from [Rob Hyndman's notes on minimal reproducible examples](https://robjhyndman.com/hyndsight/minimal-reproducible-examples/)).

A Minimal reproducible example (MRE) is intended to reproduce an error using the smallest amount of
code possible. To check that your MRE code is reproducible, try running it in a fresh R
session before you submit the issue. Using minimal reproducible examples
saves package developers time in wading through messy code that is not
relevant to the apparent bug.

A MRE should consist of a single R script file that can be run without error in a fresh R
session, and should contain the following three sections:


  * Packages to be loaded.
  * The shortest amount of code that reproduces the problem.
  * The output of `sessionInfo()` as a comment.

Please remove anything that is not necessary to reproduce the problem.

Try to use one of the built-in datasets if possible. If you need to include
some data, then use `dput()` so
the data can be included as part of the same text file. In
most cases, you do not need to include all of
your data, just a small subset that will allow the problem to be reproduced.

If you randomly generate some data, use `set.seed(somenumber)`.

Please spend time adding comments so we can understand your code quickly.

#### Feature Requests
If there is a feature that you may want added to this package, please make a suggestion by creating an [issue](https://guides.github.com/features/issues/) detailing what you like to see added to the package. For example, you may have a method which should be included in the package or find problems or inefficiencies with the current methods. We believe this is the first package of its kind, and subsequently will increase reproducibility of research; your contribution will be valued greatly. When submitting a **feature request**, please be sure to be as detailed as possible to avoid ambiguity. Please cite any research related to your method. 