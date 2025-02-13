# Reanalysis of Iowa Gambling Task data
**Original paper:** \
[Killgore WD, Lipizzi EL, Kamimori GH, Balkin TJ. Caffeine effects on risky decision making after 75 hours of sleep deprivation. Aviation, space, and environmental medicine. 2007;78(10):957-62.](https://pubmed.ncbi.nlm.nih.gov/17955944/)

---

## Directory
1. [Data files](#data-files)
2. [Scripts](#scripts)
3. [Note on data simulations](#note-on-data-simulations)

---

## Data files
**! NOTE: DATA FILES FOR KILLGORE ET AL. (2007) WILL BE MADE AVAILABLE UPON MANUSCRIPT PUBLICATION.**

All data files are located under `data` subfolder

For real data from Killgore et al. (2007), see `data\actual\Killgore2007_GUM4` 
|File/folder|Description|
|---|---|
|`...\raw`| Folder contains individual participant data files in .txt format.|
| `Killgore2007_GUM4_merged`|Merged usable data across all participants|
|`Killgore2007_GUM4_with_parameters.csv`|Merged usable data with computational modelling estimates appended|

For simulated data using ocmputational models, see `data\simulated`
|File/folder|Description|
|---|---|
|`simualted_data_PVLD_123`|simulated data using the PVL-D model using seed 123|
|`simualted_data_VSE_123`|simulated data using the VSE model using seed 123|

## Scripts
All scripts are located under `scripts` subfolder

**Data wrangling** \
`scripts\1a_datawrangling` contains code to
* Merge across all individual participant .txt data files
* Inspect and reformat data where needed
* Compute Iowa Gambling Task scores, by blocks
* Save merged data as Rdata file

**Computational Modelling** \
Stan scripts for computational models can be located under `scripts\stan-files`. 
|File/folder|Description|
|---|---|
|`1b_fit_model_loop`|Loops to fit computational models|
|`stan-files\PVL-D_sep_session.stan`|PVL-D model stan script|
|`stan-files\VSE_sep_session.stan`|VSE model stan script|
|`model_libary.R`|Definition of corresponding stan files and parameters for PVL-D & VSE models|

---
## Note on data simulations

For our data simulation scripts, we included a single offset in the `eta` parameters in each model (i.e., Learning Rate parameters, or "Exploration Update" for VSE). These offsets create a "pre-" and "post-" value, assuming *some* sort of experimental manipulation.

See lines 42-43 for PVL-D simulation script and lines 45-46 for VSE simulation scripts. To simulate data with post-manipulation "changes" in other model parameters, you may replicate these lines accordingly with your desired effect size. In our scripts, we simulated a medium effect size (*d* = .40) using a mean of -.2 and SD of .5.
 
