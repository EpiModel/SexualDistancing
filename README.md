# HIV/STI Incidence Following COVID-Related Sexual Distancing and Clinical Service Interruption

This repository holds the source to code to reproduce the analysis featured in our HIV/STI transmission model evaluating the impact of COVID-related impacts on the  dynamics of HIV, gonorrhea, and chlamydia.

## Citation

> Jenness SM, Le Guillou A, Chandra C, Mann L, Sanchez T, Westreich D, Marcus JL. Projected HIV and Bacterial STI Incidence Following COVID-Related Sexual Distancing and Clinical Service Interruption. Journal of Infectious Diseases. 2021; 223(6): 1019–28. [[LINK]](https://doi.org/10.1093/infdis/jiab051)

<img src="https://github.com/EpiModel/SexualDistancing/raw/master/analysis/Fig1.jpg">

## Abstract

#### Background
The global COVID-19 pandemic has the potential to indirectly impact the transmission dynamics and prevention of HIV and other sexually transmitted infections (STI). Studies have already documented reductions in sexual activity (“sexual distancing”) and interruptions in HIV/STI services, but it is unknown what combined impact these two forces will have on longer-term HIV/STI epidemic trajectories.

#### Methods 	
We adapted a network-based model of co-circulating HIV, gonorrhea, and chlamydia for a population of men who have sex with men (MSM) in the Atlanta area. Model scenarios varied the timing, overlap, and relative extent of COVID-related sexual distancing in casual and one-time partnership networks and service interruption within four service categories (HIV screening, HIV PrEP, HIV ART, and STI treatment).

#### Results 	
A 50% relative decrease in sexual partnerships and interruption of all clinical services, both lasting 18 months, would generally offset each other for HIV (total 5-year population impact for Atlanta MSM: -227 cases), but have net protective effect for STIs (-23,800 cases). Greater relative reductions and longer durations of service interruption would increase HIV and STI incidence, while greater relative reductions and longer durations of sexual distancing would decrease incidence of both. If distancing lasted only 3 months but service interruption lasted 18 months, the total 5-year population impact would be an additional 890 HIV cases and 57,500 STI cases.

#### Conclusions 	
The counterbalancing impact of sexual distancing and clinical service interruption depends on the infection and the extent and durability of these COVID-related changes. If sexual behavior rebounds while service interruption persists, we project an excess of hundreds of HIV cases and thousands of STI cases just among Atlanta MSM over the next 5 years. Immediate action to limit the impact of service interruptions is needed to address the indirect effects of the global COVID pandemic on the HIV/STI epidemic.

<img src="https://github.com/EpiModel/SexualDistancing/raw/master/analysis/Fig2.jpg">

## Model Code

These models are written and executed in the R statistical software language. To run these files, it is necessary to use the correct version of our epidemic modeling software, [EpiModel](http://epimodel.org/), and our extension package specifically for modeling HIV/STI transmission dynamics among MSM,
[EpiModelHIV](http://github.com/statnet/EpiModelHIV).

In R, load the necessary packages with the following command:
```
install.packages("renv")
renv::init()
```

Once `renv` has finished initializing, restart R.

### ARTnet Data Access 

To use this model, you will need a GitHub Private Access Token to install packages from private GitHub repositories (EpiModelHIV-p, ARTNetData). It should be set either in "~/.bashrc":
```
export GITHUB_PAT="<your github private access token>"
```

or in "~/.Renviron":
```
GITHUB_PAT="<your github private access token>"
```
