# SnowCholera
Data and code for analyzing John Snow's "South London" cholera data

See my John Snow Project website: http://www.hilerun.org/econ/papers/snow for more detailed explanation of data and scripts

Working paper with discussion of Snow, the data, and the statistics, on SSRN (long, roughly 90 pages): https://papers.ssrn.com/abstract=3262234

These are RStudio notebooks, each with explanation and code. The notebook (.Rmd) can be run in RStudio. For each notebook there is an associated html sheet (.nb.html) which should be a self-contained web page that displays the results.

btw, if you want to extract just the R code, run
- library (knitr)
- knit('Snow1855_SimpleDiD_QRCT.Rmd', tangle=TRUE) 

Each notebook is designed to be self-contained and independent (in terms of code) from the others. The order for looking at and running these notebooks is:

1. Snow1855_read1
  + reads in data for Tables VII, VIII, IX, XII and runs some basic cross-checks
2. Snow1855_SimpleDiD_QRCT
  + Simple Difference-in-Differences and Quasi-Randomized Comparision:
  + calculates and explains a simple diff-in-diffs table comparing 1849 versus 1854 for the "first-12" and "next-16" sub-districts
  + calculates Table IX (mortality rates by houses, simple quasi-randomized comparision - quasi-randomized control trial), and calculates imputed houses for Southwark versus Lambeth and mortality rates.
3. Snow1855_DiDRegression1
  + Runs linear and count regressions for the comparision of 1849 versus 1854, using data from Snow 1855 (i.e. without detailed sub-district population)
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
