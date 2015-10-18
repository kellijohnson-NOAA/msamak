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

# Multispecies age-structured stock assessment model

## Running model

Prior to running the model, one must download the latest version of
[ADMB](https://github.com/admb-project/admb "ADMB github").
Navigate to the admb folder and run `make` from the command line.
After compiling from source one should navigate to the examples folder
and test whether or not the simple example can be ran.
Prior to running the example ADMB may need to be added to the path,
where the binary will be located in `admb\build\dist`.

The msamak can then be downloaded from its own
[github](https://github.com/kellijohnson/msamak.git "msamak github") repository.
The model then must be built using ADMB.

## Time series

* 1960 - predation begins
* 1979 - model begins
* 1979 - mackerel recruitment estimation begins
* 1983 - cod recruitment estimation begins
* 1983 - pollock recruitment estimation begins
* 2003 - model ends

## Selectivity

The relative proportion of the population at age that is available to the sampling method.

### Logistic

A logistic function is a sigmoid curve dictated by the following equation:

$$f(x) = \frac{L}{1 + e^{-k(x - x_0)}},$$

where
$L$ is the curves maximum value,
$k$ is the steepness of the curve or the rate of increase at $x_0$, and
$x_0$ is the midpoint of the sigmoid.
In stock assessments, logistic selectivity uses the above equation with $L = 1$, such
that all values of selectivity range between zero and one.

### @Kinzey2009

Included age-specific selectivity [@maunder2003] for each species x fleet combination.
Time-varying selectivity was accommodated using blocks, which allowed age-specific
selectivity parameters to change between periods of years.
Above a certain age ($A_{k,m}$) selectivity was assumed to be independent of age.

$$L = \sum_{species}{\sum_{f}{\sum_{a}{\lambda(n_{k,a+2}^{f} + n_{k,a}^{f} - 2n_{k,a+1}^{f})}}}$$

Penalties were placed on selectivity where the squared mean selectivity across ages
was multiplied by 20.

$$\lambda_{selectivity} = \sum_{species}{20\sum_{f}{(\bar{n}_{species}^{f})^2}}$$

### Stock assessments

Each stock assessment estimates selectivity using an age-specific function, where
there is typically one parameter per age and deviations between ages are discouraged
using a penalty.

Cod selectivity is modelled using a random walk with respect to age where there is one
parameter per age greater than or equal to age 1. Age zero fish have a selectivity of
zero. For each age the selectivity parameter is the logarithm of the ratio of selectivity
at that age to selectivity at the previous age
(i.e., the backward first difference on the log scale). The resulting selectivity curves
are modelled using the Stock Synthesis selectivity-at-age pattern 17.

Pollock selectivity is age dependent and conditioned such that the mean value over all
ages is equal to one. Differences between ages are penalized using the sum of squared
second differences (log-scale).
Selectivity is assumed to be equal for all ages greater than or
equal to 8 (i.e., constant selectivity for ages 8 - 15). Four temporal blocks allow
selectivity to change over time:
(a) 1978-1989, foreign fishery;
(b) 1990-1998, domestic fishery;
(c) 1999-2008, closed fishery; and
(d) 2009-2014, bycatch in arrowtooth fishery.
Survey selectivity is also age-based with a single parameter up to age eight, though
there is no time-varying parameters included in the relationship.

Atka mackerel fishery selectivity is parameterized similar to the pollock fishery,
except that parameters are assumed to be constant for all ages ten and older.
The age at which selectivity is constant is based on the fact that Atka mackerel
reach asymptotic selectivity at an age of approximately nine years old.
Additionally, a moderate penalty of 0.3 is used to limit the flexibility on the degree
of declining selectivity at age that is allowed in the model. Penalty is implemented
through the likelihood using:

$$L = \sum_{fleet}{\lambda_{fleet}\sum_{age}^{a_{max}}{I_a (ln(s_a^{fleet}) - ln(s_{a-1}^{fleet}))}}$$

where $I_{fleet}$ is equal to one if the selectivity component is greater than zero and
zero if the selectivity component is less than zero or dome-shaped.
Time-varying selectivity is implemented using iteratively estimated constraints and
time blocks which correspond to:
(a) foreign fishery;
(b) joint venture fishery;
(c) domestic fishery; and
(d) domestic fishery with Steller sea lion regulations.
Survey selectivity is conditioned to have a mean value of one for ages between four
and ten.

## Initial conditions


## Survey biomass

### @Kinzey2009

@Kinzey2009 used a normal distribution to model the survey biomass rather than a lognormal
distribution which is what is used in AMAK.
Additionally, $q$ is fixed at one in the model for each survey. Prior to the last phase
if the estimate of $q$ is different than one the likelihood is weighted heavily such that
the survey fits are increased in subsequent phases.

## Change log

* 2015-10-06: Fixed bug in $R_0$.

# Task list

* add log normal distribution to survey biomass
* investigate estimation of $q$ for a relative index of abundance rather than absolute

# References
