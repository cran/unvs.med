# Package "unvs.med"

Version: 1.0.0

<!-- badges: start -->

<!-- badges: end -->

## Authorship

Copyrights reserved by:

First author (creator): Zhou Tianbao (Michael Zhou) [michaelzhou\@buaa.edu.cn](mailto:michaelzhou@buaa.edu.cn){.email}, \<ORCID: 0000-0001-6782-626X\>;

Second author (advisor): Li Xinghao [lixinghao\@bjfu.edu.cn](mailto:lixinghao@bjfu.edu.cn){.email};

Corresponding author (supervisor): Liu Lin\*, [liulin\@pku.edu.cn](mailto:liulin@pku.edu.cn){.email}

Institution: School of Government, Peking University, Beijing, China.

## Introduction

This package realizes a universal estimation approach that accommodates multi-category variables and effect scales, making up for the deficiencies of the existing approaches when dealing with non-binary exposures and complex models. The estimation via bootstrapping can simultaneously provide results of causal mediation on RD, OR and RR scales with tests of the effects' difference. The estimation is also applicable to many other settings, e.g., moderated mediation, inconsistent covariates, panel data, etc. The high flexibility and compatibility make it possible to apply for any type of model, greatly meeting the needs of current empirical researches.

## Installation

Users can install the development version of package "unvs.med" through R code:

``` r
install.packages("unvs.med")
```

## Example

This is an example based on the test data which shows you how to estimate causal mediation effects through this package:

``` r
library(unvs.med)
data(testdata)
# Fitting mediator's model
med_model=glm(med~exp+C1+C2+C3, data=testdata, family=binomial)

# Fitting outcome's model
out_model=lm(out~med*exp+C1+C2+C3, data=testdata) 

# Running formal estimation via bootstrapping
r11 = FormalEstmed (med_model=med_model, out_model=out_model,
data=testdata, exposure = "exp") 

# Viewing results in short form and on RD scales
summary(r11) 

## Plot of PNED and TNDE on RD scales
plot(r11)
```
