rsleep: A R package for sleep data analysis
================

[![CRAN](https://www.r-pkg.org/badges/version/rsleep)](https://cran.r-project.org/package=rsleep)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/rsleep)](https://cran.r-project.org/package=rsleep)
![License:MIT](https://img.shields.io/badge/License-MIT-blue.svg)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10507974.svg)](https://doi.org/10.5281/zenodo.10507974)
[![](https://img.shields.io/static/v1?label=Sponsor&message=%E2%9D%A4&logo=GitHub&color=%23fe8e86)](https://github.com/sponsors/boupetch)

rsleep: Open-source, multiplatform R package for advanced sleep data analysis. Features automatic sleep scoring and sophisticated visualization tools.

## Installation

Development version can be directly installed from
[Github](https://github.com/boupetch/rsleep) :

``` r
remotes::install_github("boupetch/rsleep@dev")
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

- [Spectral analysis of sleep electroencephalography signals](https://rsleep.org/articles/Spectral_analysis_sleep_electroencephalography.html)
- [Spindles detection and analysis](https://rsleep.org/articles/Spindles_detection_and_analysis.html)
- [Using Rsleep and SleepCycles Packages to Detect Sleep Cycles](https://rsleep.org/articles/Using_rsleep_and_SleepCycles_packages_to_detect_sleep_cycles.html)
  
## Examples

### Plotting a spectrogram

[<img src="https://rsleep.org/articles/Spectral_analysis_sleep_electroencephalography_files/figure-html/spectrogram-1.png" width="600">](https://rsleep.org/articles/Spectral_analysis_sleep_electroencephalography.html)

### Detecting R peaks in ECG signal

[<img src="https://rsleep.org/reference/detect_rpeaks-1.png" width="400">](https://rsleep.org/reference/detect_rpeaks.html)

### Processing a hypnogram

[<img src="https://rsleep.org/articles/Spectral_analysis_sleep_electroencephalography_files/figure-html/hypnogram-1.png" width="600">](https://rsleep.org/reference/hypnogram.html)

### Plotting a hypnodensity

[<img src="https://rsleep.org/reference/plot_hypnodensity-1.png" width="600">](https://rsleep.org/reference/plot_hypnodensity.html)

### Detecting spindles

[<img src="https://rsleep.org/articles/Spindles_detection_and_analysis_files/figure-html/unnamed-chunk-6-1.png" width="600">](https://rsleep.org/reference/a7.html)

### Computing a transition matrix

[<img src="https://rsleep.org/reference/transitions-1.png" width="400">](https://rsleep.org/reference/transitions.html)

# Citation

```
@software{paul_bouchequet_2024_10507974,
  author       = {Paul Bouchequet},
  title        = {rsleep},
  doi          = {10.5281/zenodo.7416363},
  url          = {https://doi.org/10.5281/zenodo.7416363}
}
```

## Publications using the rsleep package

- Lok, R., Duran, M., & Zeitzer, J. M. (2023). [Moving time zones in a flash with light therapy during sleep.](https://www.nature.com/articles/s41598-023-41742-w) In Scientific Reports (Vol. 13, Issue 1). Springer Science and Business Media LLC.

- Baur, D. M., Dornbierer, D. A., & Landolt, H.-P. [Concentration-effect relationships of plasma caffeine on EEG delta power and cardiac autonomic activity during human sleep.](https://www.medrxiv.org/content/10.1101/2023.10.14.23297036v1) Cold Spring Harbor Laboratory.

- Wolf, M.C., Klein, P., Kulau, U., Richter, C. and Wolf, K.H., [DR. BEAT: First Insights into a Study to Collect Baseline BCG Data with a Sensor-Based Wearable Prototype in Heart-Healthy Adults.](https://arinex.com.au/EMBC/pdf/full-paper_271.pdf)

- P. Bouchequet, T. Andrillon, G. Solelhac, A. Rouen, F. Sauvet, and D. Léger, [0424 Visualizing insomnia phenotypes using dimensionality reduction techniques,](https://academic.oup.com/sleep/article/46/Supplement_1/A188/7181658) SLEEP, vol. 46, no. Supplement_1. Oxford University Press (OUP), pp. A188–A189, May 01, 2023

- Santhiya P., JebaRajalakshmi J., S Siva Ranjani, Selvi S. ArunMozhi, [Detection of Epilepsy through Machine Learning Algorithms Using Brain Signals](https://www.proquest.com/openview/5244c8f8e90715c223df74f2487651dc/1?pq-origsite=gscholar&cbl=2035897), NeuroQuantology, Bornova Izmir Vol. 20, Iss. 8,  (2022): 6011 - 6018. 

- Rajalakshmi, J., Ranjani, S. S., Sugitha, G., & Prabanand, S. C. (2022). [Electroencephalogram Data Analysed Through the Lens of Machine Learning to Detect Signs of Epilepsy.](https://ieeexplore.ieee.org/document/9985641) In 2022 4th International Conference on Inventive Research in Computing Applications (ICIRCA). 2022 IEEE. 

- Altınkaya Z, Öztürk L, Büyükgüdük İ, et al. [Non-invasive vagus nerve stimulation in a hungry state decreases heart rate variability.](https://www.sciencedirect.com/science/article/abs/pii/S0031938422003213) Physiology & Behavior. 2023;258:114016.

- Munch Nielsen, J., Zibrandtsen, I. C., Masulli, P., Lykke Sørensen, T., Andersen, T. S., & Wesenberg Kjær, T. (2022). [Towards a wearable multi-modal seizure detection system in epilepsy: A pilot study.](https://www.sciencedirect.com/science/article/pii/S1388245722000219?via%3Dihub) In Clinical Neurophysiology (Vol. 136, pp. 40–48). Elsevier BV. https://doi.org/10.1016/j.clinph.2022.01.005 

- Rajalakshmi J, Ranjani SS, Sugitha G, Prabanand SC. [Electroencephalogram Data Analysed Through the Lens of Machine Learning to Detect Signs of Epilepsy.](https://ieeexplore.ieee.org/document/9985641) 2022 4th International Conference on Inventive Research in Computing Applications (ICIRCA). September 2022. 

- Andrillon T, Solelhac G, Bouchequet P, et al. [Leveraging machine learning to identify the neural correlates of insomnia with and without sleep state misperception.](https://www.sciencedirect.com/science/article/pii/S1389945722005378) Sleep Medicine. 2022;100:S129. 

- Chang K-M, Liu P-T, Wei T-S. [Electromyography Parameter Variations with Electrocardiography Noise.](https://www.mdpi.com/1424-8220/22/16/5948) Sensors. 2022;22:5948. 

- Kragness HE, Eitel MJ, Anantharajan F, Gaudette-Leblanc A, Berezowska B, Cirelli L. [An itsy bitsy audience: Live performance facilitates infants’ attention and heart rate synchronization.](https://osf.io/preprints/psyarxiv/9s43u/) psyarxiv.com/9s43u 10.31234/osf.io/9s43u 2022.

- Stucky B, Clark I, Azza Y, et al. [Validation of Fitbit Charge 2 Sleep and Heart Rate Estimates Against Polysomnographic Measures in Shift Workers: Naturalistic Study.](https://www.jmir.org/2021/10/e26476/) J Med Internet Res. 2021;23:e26476. 

- Arts F. [Predicting Subjective Team Performance Using Multimodal, Single-Modality and Segmented Physiological Data](https://arno.uvt.nl/show.cgi?fid=156733) Thesis, 2020.

- Andrillon T, Solelhac G, Bouchequet P, et al. [Revisiting the value of polysomnographic data in insomnia: more than meets the eye.](https://www.sciencedirect.com/science/article/abs/pii/S1389945719316442) Sleep Medicine. 2020;66:184-200.
