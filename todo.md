# To do list for msamak

## Description
msamak is a multi-species stock assessment model for Alaska.
The software was built off of the single-species model known as amak.
amak was built and is maintained by Dr. James Ianelli of the 
Alaska Fisheries Science Center for walleye pollock.

## To do
### Programming
* use age and length composition data instead of just age or length
* Remove `styr_sp` in exchange for `styr_rec` because they are the same.
* Remove `endyr_sp` which is later used for `endyr_all`.
* Check if there is ageing error in the model.
* Maturity is adjusted by dividing the age-specific vector of maturity by
2.0 if the maximum maturity-at-age is > 0.9 and I am not sure why.
* Determine how `omega_vB` (von Bertalanffy ration is calculated)
* Data weighting for the omega is assumed to be 0.0005 for each species
and is set via `omega_sigma`
* Determine where the maximum sample size stomach weights `max_SS_w` and
maximum sample size stomach lengths `max_SS_l` values come from, right now
they are set at 2000.
* looks like `cv_catchbiomass` 
(penalty on catch biomass; 
`catchbiomass_pen = 1.0 / (2 * square(cv_catchbiomass))`) 
and `sd_ration` 
are weighting factors for the catch and ration data. 
Need to investigate these values.

### Future programming
* Utilize logistic selectivity `srv_sel_opt(isrv) == 2`, but prior to its
use the code needs to be checked.

### Discussion
* Inclusion of age zero fish in a stock assessment model is not typical.
Instead, stock assessment models typically only include the smallest age
sampled by the fishery, and all others are ignored until they reach this age.
The multi-species model has to include age-zero fish because predators consume
fish at younger ages than what they are sampled by the fishery.
* Weight-at-age for each fishery is read in as fixed values.
Atka mackerel and Pacific cod assume weight-at-age is stationary, whereas
walleye pollock has time-varying weight-at-age.
