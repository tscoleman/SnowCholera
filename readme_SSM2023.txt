These are data files (.csv) and R Studio program files (.Rmd) to accompany the paper "Re-Evaluating John Snow’s 1856 South London Study" by Thomas S. Colemn

DATA FILES:

  + Snow1855_TableVII.csv - Table VII from Snow "On the mode of communication of cholera" 1855, deaths by sub-district, assigned to water supplier, for first 4 weeks of 1854 outbreak
  + Snow1855_TableVIII.csv - Table VIII from Snow "On the mode of communication of cholera" 1855, deaths by sub-district, assigned to water supplier, for first 7 weeks ending 26th August 1854
  + Snow1855_TableXII.csv - Table XII from Snow "On the mode of communication of cholera" 1855, deaths by sub-district (not assigned to water supplier) for 1849 and 1854 epidemics
  + Snow1856_TableI.csv - Tables I & II from Snow "Cholera and the water supply in the south district of London in 1854" (Tables I & II combined, so showing all 32 sub-districts) - deaths and population assigned to water supplier, by *sub-district*, for first 7 weeks ending 26th August 1854 (cross-checks with 1855 Table VIII)
  + Snow1856_TableV.csv - Tables V from Snow "Cholera and the water supply in the south district of London in 1854" deaths and population assigned to water supplier, by *District*, for the whole of the 1854 epidemic (cross-checks with 1855 Table 
  + Snow1856_TableVI.csv - Tables VI from Snow "Cholera and the water supply in the south district of London in 1854" deaths by sub-district (not assigned to water supplier) for 1854 epidemic, with Snow's population-weighted predicted mortality
  + Simon1856_TableIII_pop.csv - from Simon's "Report on the last two cholera-epidemics of London" transcription of only Simon's published population estimates for 1849, 1851, and1854
  + Simon1856_TableIII_StNoDeath.csv - from Simon's "Report on the last two cholera-epidemics of London" transcription of the population by District of streets with no deaths, reported at District but not subdistrict level
  + KochDenikeFigure7.csv - transcription of "Figure 7" from “Rethinking John Snow’s South London Study: A Bayesian Evaluation and Recalculation.” by Thomas Koch and Kenneth Denike (2006). Social Science & Medicine 63 (1): 271–83.
  + GRO1854Elevation - Elevation above Trinity High-water Mark for subdistricts, transcribed from General Register Office, Weekly Return of Births and Deaths Sept 2, 1854.


PROGRAM FILES:

  + Snow_SSMPaper_202311.Rmd - R Studio notebook with data and analysis for the paper submitted to Social Science and Medicine "Re-Evaluating John Snow's 1856 South London Study"
    + Read in the data for various of Snow's tables, and Koch & Denike's tables. These are all csv files that I have transcribed
    + Calculate Snow's predicted counts and rates by subdistrict
    + Calculate Koch & Denike's t-test
    + Calculate example table to explain t-test versus sum of squares
    + Adjust population data from Simon for the problems outlined in the paper
      + Some subdistricts are reported to have Lambeth customers when they do not
      + Subdistrict population-by-supplier does not add to district population-by-supplier
      + Subdistrict population for S&V + Lambeth sometimes exceeds 1851 census and 1854 Simon estimates
    + Run regressions
      + OLS
      + Single Negative Binomial 
  + Snow_ReadDataFn.Rmd - utility function that reads in data files and creates arrays for regression. Also adjusts population data as described in the paper. Called automatically from "Snow_SSMPapaer_202311.Rmd"



The data files from Snow and Simon are transcriptions of publications in the public domain. 
The Koch & Denike data file is a transcription of their "Figure 7". 

The program files are released under a non-restrictive BSD 2-Clause license:

BSD 2-Clause License

Copyright (c) 2023, Thomas Coleman
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
