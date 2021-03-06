rsleep: A R package for sleep data analysis
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![CRAN](http://www.r-pkg.org/badges/version/rsleep)](https://cran.r-project.org/package=rsleep)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/rsleep)](https://cran.r-project.org/package=rsleep)
[![Build
Status](https://travis-ci.org/boupetch/rsleep.svg?branch=master)](https://travis-ci.org/boupetch/rsleep)
[![Docker
build](https://img.shields.io/docker/cloud/build/boupetch/rsleep)](https://cloud.docker.com/repository/docker/boupetch/rsleep/)
[![codecov](https://codecov.io/gh/boupetch/rsleep/branch/master/graph/badge.svg)](https://codecov.io/gh/boupetch/rsleep)
[![License:MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

rsleep is a R package providing tools for sleep data management,
visualization and analysis.

## Installation

Stable version can be downloaded and installed from
[CRAN](https://cran.r-project.org/) \[1\] :

``` r
install.packages("rsleep", repos = "https://cloud.r-project.org")
```

Development version can be directly installed from
[Github](https://github.com/) using the `devtools` package :

``` r
devtools::install_github("boupetch/rsleep")
```

## Usage

``` r
library(rsleep)
```

## Vignettes

  - [Managing Sleep Records
    Files](http://htmlpreview.github.io/?https://raw.githubusercontent.com/boupetch/rsleep/master/doc/Managing_Sleep_Records_Files.html)
  - [Spectral analysis of sleep electroencephalography
    signals](http://htmlpreview.github.io/?https://raw.githubusercontent.com/boupetch/rsleep/master/doc/Spectral_analysis_sleep_electroencephalography.html)
  - [Automatic Stages
    Classification](http://htmlpreview.github.io/?https://raw.githubusercontent.com/boupetch/rsleep/master/doc/Automatic_Stage_Classification.html)

## Examples

[![Hypnogram](man/figures/README-example_hypnogram-1.png)](http://htmlpreview.github.io/?https://raw.githubusercontent.com/boupetch/rsleep/master/doc/Managing_Sleep_Records_Files.html)

[![Hypnodensity](man/figures/README-example_hypnodensity-1.png)](http://htmlpreview.github.io/?https://raw.githubusercontent.com/boupetch/rsleep/master/doc/Automatic_Stage_Classification.html)

[![EEG Spectral
profiles](man/figures/README-example_spectral-profiles-1.png)](http://htmlpreview.github.io/?https://raw.githubusercontent.com/boupetch/rsleep/master/doc/Spectral_analysis_sleep_electroencephalography.html)

## Docker

The rsleep package can be used through Docker, with the automatic build
from
[Dockerhub](https://cloud.docker.com/repository/docker/boupetch/rsleep/)
or by building the image using the provided
[Dockerfile](https://github.com/boupetch/rsleep/blob/master/Dockerfile).

### Example

Automatic sleep stages scoring using rsleep Docker image:

    wget -O 15012016HD.edf https://osf.io/57j2u/download 
    
    docker run --rm  -v $(pwd):/shared boupetch/rsleep R -e "write.csv(rsleep::score_stages_edf('/shared/15012016HD.edf'),'/shared/hypnodensity.csv')"

## References

<div id="refs" class="references">

<div id="ref-hornik2012comprehensive">

\[1\] K. Hornik, The comprehensive r archive network, Wiley
Interdisciplinary Reviews: Computational Statistics. 4 (2012) 394–398.
<https://cran.r-project.org/>.

</div>

</div>
