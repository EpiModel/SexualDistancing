# Opportunities for HIV/STI Control During Sexual Distancing of the COVID-19 Global Pandemic

Description

## Citation

> Citation

<img src="https://github.com/EpiModel/SexualDistancing/raw/master/analysis/Fig1.jpg">

## Abstract

#### Background
Text

#### Methods 	
Text

#### Results 	
Text

#### Conclusions 	
Text

<img src="https://github.com/EpiModel/SexualDistancing/raw/master/analysis/Fig2.jpg">

## Model Code

These models are written and executed in the R statistical software language.
To run these files, it is necessary to use the correct version of our epidemic 
modeling software, [EpiModel](http://epimodel.org/), and our extension package 
specifically for modeling HIV/STI transmission dynamics among MSM,
[EpiModelHIV](http://github.com/statnet/EpiModelHIV).


In R:

```
install.packages("renv")
renv::init()
```

Once `renv` has finished initializing, restart R.

### Warning 

To use this model, you will need a GitHub Private Access Token to install 
packages from private GitHub repositories (EpiModelHIV-p, ARTNetData).

It should be set either in "~/.bashrc":

```
export GITHUB_PAT="<your github private access token>"
```

or in "~/.Renviron":

```
GITHUB_PAT="<your github private access token>"

```
