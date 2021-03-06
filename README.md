# SnowCholera
Data and code for analyzing John Snow's "South London" cholera data from 1854

**New Working Paper** (Sep 2020) re-analyzing Snow's south London data: https://papers.ssrn.com/abstract=3696028
The following updates are for this working paper:
1. RStudio notebook(s)
  + Snow_ReAnalyzed1.Rmd - master that reads in data and runs all the regressions used in the paper
    + This notebook runs, but it is a very rough draft and need more documentation, clean-up, etc. 
  + Snow_ReadData.Rmd - subsidiary workbook to read data. Called by the above
  + Snow_PlotFns.r - plot functions called by the above
2. Data
  + Snow1855_TableVIII.csv - deaths by sub-district, assigned to water supplier, for first 7 weeks ending 26th August 1854
  + Snow1855_TableIX.csv - Snow's comparison of mortality rates for Southwark customers vs Lambeth customers
  + Snow1855_TableXII.csv - deaths by sub-district (not assigned to water supplier) for 1849 and 1854 epidemics
  + Snow1856_TableI.csv - (actually Tables I & II combined, so showing all 32 sub-districts) - deaths and population assigned to water supplier, by *sub-district*, for first 7 weeks ending 26th August 1854 (cross-checks with 1855 Table VIII)
  + Snow1856_TableV.csv - deaths and population assigned to water supplier, by *District*, for the whole of the 1854 epidemic (cross-checks with 1855 Table XII)
  + Snow1856_TableVI.csv - deaths by sub-district (not assigned to water supplier) for 1854 epidemic, with Snow's population-weighted predicted mortality
  + Simon1856_TableII.csv - deaths and population by _district_, including population estimates 1849 and 1854
  + Simon1856_TableIII_pop.csv - population by _subdistrict_, including estimates for 1849 and 1854. 

**Older Working Paper** with discussion of Snow, the data, and the statistics, on SSRN (long, roughly 90 pages): https://papers.ssrn.com/abstract=3262234

**Data** (more detailed explanation at http://www.hilerun.org/econ/papers/snow)

1. Snow1855_TableVII.csv - deaths by sub-district, assigned to water supplier, for first 4 weeks
2. Snow1855_TableVIII.csv - deaths by sub-district, assigned to water supplier, for first 7 weeks ending 26th August 1854
3. Snow1855_TableIX.csv - Snow's comparison of mortality rates for Southwark customers vs Lambeth customers
4. Snow1855_TableXII.csv - deaths by sub-district (not assigned to water supplier) for 1849 and 1854 epidemics
5. Snow1856_TableI.csv - (actually Tables I & II combined, so showing all 32 sub-districts) - deaths and population assigned to water supplier, by *sub-district*, for first 7 weeks ending 26th August 1854 (cross-checks with 1855 Table VIII)
6. Snow1856_TableV.csv - deaths and population assigned to water supplier, by *District*, for the whole of the 1854 epidemic (cross-checks with 1855 Table XII)
7. Snow1856_TableVI.csv - deaths by sub-district (not assigned to water supplier) for 1854 epidemic, with Snow's population-weighted predicted mortality

**RStudio notebooks**, each with explanation and code. The notebook (.Rmd) can be run in RStudio. For each notebook there is an associated html sheet (.nb.html) which should be a self-contained web page that displays the results.

Each notebook is designed to be self-contained and independent (in terms of code) from the others. The order for looking at and running these notebooks is:

1. Snow1855_read1
  + reads in data for Tables VII, VIII, IX, XII and runs some basic cross-checks
2. Snow1855_SimpleDiD_QRCT
  + Simple Difference-in-Differences and Quasi-Randomized Comparision:
  + calculates and explains a simple diff-in-diffs table comparing 1849 versus 1854 for the "first-12" and "next-16" sub-districts
  + calculates Table IX (mortality rates by houses, simple quasi-randomized comparision - quasi-randomized control trial), and calculates imputed houses for Southwark versus Lambeth and mortality rates.
3. Snow1855_DiDRegression1
  + Runs linear and count regressions for the comparision of 1849 versus 1854, using data from Snow 1855 (i.e. without sub-district population separately by supplier)
4. Snow1855_DidRegression2_ErrorAnalysis
  + Discusses in more detail the error process for count regressions for 1849 versus 1854
  + The data exhibit more variability than consistent with Poisson counts - what is called in the literature "overdispersion"
  + Graphs (using helper functions) the actual and predicted rates by sub-district, highlighting the variation ("overdispersion") both across and within sub-districts
5. Snow1856_TableVI_Intro
  + Reads in and does some simple checks on Snow 1856 Tables I, II, V, and VI
  + Discusses the variation across sub-districts observed in mortality rates in Table VI
  + Discusses the 623 "unascertained" deaths in Table V
  + Discusses the Koch & Denike (2006) paper that (incorrectly) re-assigns the 623 deaths
6. Snow1856_TableVI_DiD
  + Combines the counts from Snow 1855 Table XII with population from Snow 1856 Tables I & II
		to run a difference-in-differences using sub-district specific population weights
  + Conclusion: Snow was right in saying that water supply was a large and significant factor.
7. Snow1856_QuasiRandomized1
  + Snow 1855 Table VIII: Combines counts for deaths (assigned to water supplier) from Snow 1855 Table VIII with the sub-district population by water supplier from Snow 1856 Tables I & II
  + Estimates count regressions to compare mortality rates by sub-district, allowing for the variation across sub-districts
  + This is the natural extension of Snow 1855 Table IX using sub-district population
  + Snow 1855 Table V: Counts for deaths (assigned to water supplier) by District and population by District

btw, if you want to extract just the R code, run
- library (knitr)
- knit('notebook.Rmd', tangle=TRUE)

and this will save *notebook.R* under your working directory

