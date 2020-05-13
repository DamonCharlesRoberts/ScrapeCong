---
title: "cran-comments"
author: "Damon C. Roberts"
date: "5/13/2020"
output: pdf_document
---
## Test environments

* local OS X install, R 4.0.0
* RStudio Cloud, R 3.6.0
* Devtools::win_check() Windows 64-bit, R 4.0.0
* Devtools::win_check() Windows 64-bit, R 3.6.3

## R CMD check results

There were no errors or warnings.

For windows, there was one note about potential misspellings in the DESCRIPTION file. These were incorrectly flagged.
  
## Downstream dependencies

I have run a R CMD check on downstream dependencies for my packages.
All packages passed.
