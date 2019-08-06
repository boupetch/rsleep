# rsleep 1.0.2

## Minor changes

  * `epochs` function can take a single vector containing a single signal. `resample` default to the max of the sample rates.
  * Added padding parameter to `epochs` function. Pads epochs with previous and next epochs of the record.
  * `bands_power` does not normalize by band size anymore.
  * Added HRV example in README.
  * In `bands_power`, broadband argument renamed to normalize. Can be set up to FALSE.
  * Write function `write_mdf` & `write_channel` use platform endian by default to write binary files.
  * Added automatic sleep staging functions.
