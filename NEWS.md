# rsleep 1.0.11

  * Added `create_xts()` function to create `xts` objects from signal.
  * Added `clean_oximetry()` function to remove oximetry signals outliers.
  * Added a `CONTRIBUTING.md` file.
  * Updated `read_events_compumedics()` to take labels bindings as parameter.
  * Added `Spindles detection and analysis` vignette.
  * Added `bandpass()` function as it is useful.
  * Added `a7()` implemented from Lacourse & Al 2019.

# rsleep 1.0.10

  * Added `adanorm()` implemented from Choi & Al 2018.
  * Added balanced accuracy custom metric to deep learning model.
  * Corrected 302 link to PsyArxiv.

# rsleep 1.0.9

  * Removed 302 link to Resmed website.
  * Updated rsleep.org certificates.

# rsleep 1.0.8

  * Added `NREM` to `transitions()` included stages.
  * Corrected `SleepCycles` directory lifecycle to stick to CRAN guidelines.
  * `epochs()` padding use empty matrixes when padding exceed signal boundaries.

# rsleep 1.0.7

  * `periods()` function returns a dataframe of sleep periods from a hypnogram, continuous or splitted by stages.
  * `transitions()` function now use a `format` parameter to return a vector, a dataframe or a plot of counted transitions.
  * Added `segmentation()` function to split signals into consecutive overlapping (or not) chunks.
  * Added `ckappa()` to compute Cohen's Kappa, a coefficient of agreement for nominal scales.
  * Added `choi2018()` function, a Keras implementation of the deep learning architecture described by Choi & Al in "Real-time apnea-hypopnea event detection during sleep by convolutional neural network".
  * `read_events_profusion()` bug in scored events reading fixed.
  * `plot_hypnogram()` can take colors to use as a parameter.
  * Added `read_events_ndb()` function to read `.ndb` files from Resmed Noxturnal software. 
  * Added `plot_event()` function to plot a scored event over a signal.

# rsleep 1.0.6

  * Following efforts towards merge between Github and CRAN versions.
  * Renamed `read_stages_compumedics` function to `read_events_compumedics`.
  * Added `read_events_profusion` function to read annotations from NSRR.
  * Added `xml2` package to dependencies.
  * Added `tst90` function.
  * `hypnogram` function can now take hypnodensity to convert to hypnogram and plot it if asked with the `plot` parameter.
  * Added `transitions` function to compute matrices of stages transitions.
  
# rsleep 1.0.5

  * This release adds a few functions already available in the devel version on Github.

# rsleep 1.0.2

  * `epochs` function can take a single vector containing a single signal. 
  `resample` default to the max of the sample rates.
  * Added padding parameter to `epochs` function. Pads epochs with previous and next epochs of the record.
  * `bands_power` renamed to `bands_psd`.
  * `bands_psd` does not normalize by band size anymore.
  * In `bands_psd`, broadband argument renamed to normalize. Can be set up to FALSE.
  * Spectral analysis & File management vignettes.
  * Write function `write_mdf` & `write_channel` use platform endian by default to write binary files.
  * Added automatic sleep staging functions and vignette.
  * Added `read_stages_compumedics()` and `write_hypnogram_compumedics()` functions.
  * Removed `examples example_ecg_200hz` and `example_hypnogram_30s`.
  * Added `smooth_hypnogram()` and `smooth_liang2012()`
