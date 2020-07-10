# rough_rt
Code for rough estimates from data

## General approach

* Shift observations back in time by the mean delay
* Estimate Rt using the method of Cori et al

This is rough because we're not adjusting for day of week effects or really trying to infer the unobserved time series.

## Contents

* `code` - contains code used to generate estimates.
* `examples_syndat` - notebooks show tests and example estimates from synthetic data.
* `estimates` - notebooks use IDPH data on new deaths to estimate Rt. 
* `figs` - results from `estimates`.
* `data` - intentionally excluded from repo. Lives on Midway.
