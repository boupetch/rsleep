rsleep: A R package for sleep data analysis
================

[![CRAN](https://www.r-pkg.org/badges/version/rsleep)](https://cran.r-project.org/package=rsleep)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/rsleep)](https://cran.r-project.org/package=rsleep)
[![License:MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7416363.svg)](https://doi.org/10.5281/zenodo.7416363)

rsleep is a multiplatform open-source R package providing a toolbox for sleep data processing,
visualization and analysis. rsleep provides tools for state of the art automatic sleep stages scoring.

## Installation

Development version can be directly installed from
[Github](https://github.com/) using the `devtools` package :

``` r
devtools::install_github("boupetch/rsleep")
```

Stable version can be downloaded and installed from
[CRAN](https://cran.r-project.org/):

``` r
install.packages("rsleep")
```

## Usage

``` r
library(rsleep)
```

## Vignettes

- [Spectral analysis of sleep electroencephalography
  signals](https://rsleep.org/articles/Spectral_analysis_sleep_electroencephalography.html)
  
## Examples

### Detecting R peaks in ECG signal

[<img src="https://rsleep.org/reference/detect_rpeaks-1.png" width="400">](https://rsleep.org/reference/detect_rpeaks.html)

### Processing a hypnogram

[<img src="https://rsleep.org/reference/hypnogram-1.png" width="600">](https://rsleep.org/reference/hypnogram.html)

### Plotting a hypnodensity

[<img src="https://rsleep.org/reference/plot_hypnodensity-1.png" width="600">](https://rsleep.org/reference/plot_hypnodensity.html)

### Computing a transition matrix

[<img src="https://rsleep.org/reference/transitions-1.png" width="400">](https://rsleep.org/reference/transitions.html)

# Citation

```
@software{paul_bouchequet_2022_7474289,
  author       = {Paul Bouchequet and
                  Damien Léger},
  title        = {rsleep},
  month        = dec,
  year         = 2022,
  publisher    = {Zenodo},
  version      = {1.0.6},
  doi          = {10.5281/zenodo.7416363},
  url          = {https://doi.org/10.5281/zenodo.7416363}
}
```

## rsleep usage in scientific litterature

- Altınkaya Z, Öztürk L, Büyükgüdük İ, et al. [Non-invasive vagus nerve stimulation in a hungry state decreases heart rate variability.](https://www.sciencedirect.com/science/article/abs/pii/S0031938422003213) Physiology & Behavior. 2023;258:114016.

- Chang K-M, Liu P-T, Wei T-S. [Electromyography Parameter Variations with Electrocardiography Noise. Sensors.](https://www.mdpi.com/1424-8220/22/16/5948) 2022;22:5948. 

- Kragness HE, Eitel MJ, Anantharajan F, Gaudette-Leblanc A, Berezowska B, Cirelli L. [An itsy bitsy audience: Live performance facilitates infants’ attention and heart rate synchronization. psyarxiv.com/9s43u](https://psyarxiv.com/9s43u/) 10.31234/osf.io/9s43u 2022.

- Stucky B, Clark I, Azza Y, et al. [Validation of Fitbit Charge 2 Sleep and Heart Rate Estimates Against Polysomnographic Measures in Shift Workers: Naturalistic Study.](https://www.jmir.org/2021/10/e26476/) J Med Internet Res. 2021;23:e26476. 

- Arts F. [Predicting Subjective Team Performance Using Multimodal, Single-Modality and Segmented Physiological Data](https://arno.uvt.nl/show.cgi?fid=156733) Thesis, 2020.

- Andrillon T, Solelhac G, Bouchequet P, et al. [Revisiting the value of polysomnographic data in insomnia: more than meets the eye.](https://www.sciencedirect.com/science/article/abs/pii/S1389945719316442) Sleep Medicine. 2020;66:184-200.


