# To do list for msamak

## Description
msamak is a multi-species stock assessment model for Alaska.
The software was built off of the single-species model known as amak.
amak was built and is maintained by Dr. James Ianelli of the
Alaska Fisheries Science Center for walleye pollock.

## To do
### Programming
* Change survey index to lognormal from normal.
* Maturity is adjusted by dividing the age-specific vector of maturity by
2.0 if the maximum maturity-at-age is > 0.9 and I am not sure why.
AEP did not know why either, but mentioned that it should not matter
because maturity is just a scalar anyway.
* Determine how `omega_vB` (von Bertalanffy ration is calculated)
* Data weighting for the omega is assumed to be 0.0005 for each species
and is set via `omega_sigma`
* looks like `cv_catchbiomass`
(penalty on catch biomass;
`catchbiomass_pen = 1.0 / (2 * square(cv_catchbiomass))`)
and `sd_ration`
are weighting factors for the catch and ration data.
AEP said they should be used to multiply the norm2(log()-log())
because the above represents the bottom of lognormal likelihood.
* Investigate the effects of removing exp(natmortprior) from the final divisor
of R_guess. Amak does not do this, and you are just dividing by 1.2
* Look at IyrPred and how it goes from iyr to endyr
* Recruitment penalty is not used in amak.
It does not appear that there is a constraint to make the recruitment deviations
sum to one within a species. Instead,
$$
  int rec_adv = 0;
  for (isp = 1; isp <= nspp; isp++)
   for (iyr = styr_rec(isp); iyr <= endyr; iyr++)
    {
     rec_adv += 1;
     rec_dev_spp(isp,iyr) = rec_dev(rec_adv);
     Temp = (rec_dev(rec_adv) + 6.5) / 8.5;
     Temp1 = 10;
     for (ii = 1; ii <= 10; ii++)
      Temp1 *= Temp;
     penal_rec_dev += Temp1;
    }
$$
AEP thought the above was retarded and did not know why it was included.
* Why does predation start in 1960?
* Remove 'N_pred_yr = constant;' in AltStart, because N_pred_yr is initialized
in a different function.
* Major changes to AltStart, so that the initial age structure is based on
the terminal age of the oldest individuals. Lots of math involved.

### Future programming
* use age and length composition data instead of just age or length
* Check if there is ageing error in the model.

### Data
* Maximum ages == 15, 15, 12 yet the stock assessments use 14, 11, 11

### Discussion
* Inclusion of age zero fish in a stock assessment model is not typical.
Instead, stock assessment models typically only include the smallest age
sampled by the fishery, and all others are ignored until they reach this age.
The multi-species model has to include age-zero fish because predators consume
fish at younger ages than what they are sampled by the fishery.
* Weight-at-age for each fishery is read in as fixed values.
Atka mackerel and Pacific cod assume weight-at-age is stationary, whereas
walleye pollock has time-varying weight-at-age.
