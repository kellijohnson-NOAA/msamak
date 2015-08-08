---
Title: Multispecies age-structured stock assessment framework
Author: 
- name: Kelli Faye Johnson
  email: kfjohns@uw.edu
  affiliation: School of Aquatic and Fishery Sciences, University of Washington
date: August 07, 2015
tags: [multispecies, stock assessment]
output: pdf_document
bibliography: 'c:\\users\\kelli\\Google Drive\\references\\references.bib'
csl: 'c:\\users\\kelli\\Google Drive\\references\\ices-journal-of-marine-science.csl'
---

Selectivity
-----------

### @kinzey2009

Included age-specific selectivity [@maunder2003] for each species x fleet combination.
Time-varying selectivity was accommodated using blocks, which allowed age-specific 
selectivity parameters to change between periods of years. 
Above a certain age ($A_{k,m}$) selectivity was assumed to be independent of age.

### Stock assessments

Each stock assessment estimates selectivity using an age-specific function, where
there is typically one parameter per age and deviations between ages are discouraged
using a penalty.

# References
