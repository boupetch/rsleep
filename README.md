# `rsleep`

## Installation

`rsleep` latest version can be directly installed from Github using the `devtools` package.

```
devtools::install_github("boupetch/rsleep")
```

## Usage

### File manipulation

In `rsleep`, `write_mdf()` and `read_mdf()` functions are used to write and read records on disk. Files are converted from the European Data Format (EDF) to Morpheo Data Format<sup>1</sup> (MDF). MDF is a simple, efficient and interoperable hierarchical file format for biological timeseries. The format supports raw signal and metadata storage. MDF uses binary files for signals and JSON for metadata.

### Statistics computing

#### Stages & scoring

`stages_stats` function computes various statistics from the hypnogram.

  * `rem_duration`: Total duration of REM sleep in minutes.
  * `n1_duration`: Total duration of N1 sleep in minutes.
  * `n2_duration`: Total duration of N2 sleep in minutes.
  * `n3_duration`: Total duration of N3 sleep in minutes.
  * `awa_duration`: Total duration of wake in minutes.
  * `tts`: Time To Sleep (N1+N2+N3+REM durations) in minutes.
  * `rem_tts`: REM over TTS duration ratio.
  * `n3_tts`: N3 over TTS duration ratio.
  * `n2_tts`: N2 over TTS duration ratio.
  * `n1_tts`: N1 over TTS duration ratio.
  * `tsp`: Total Sleep Period.
  * `sleep_efficiency`: Sleep Efficiency.
  * `sleep_latency`: Sleep Latency.
  * `rem_latency`: REM Sleep Latency.
  * `waso`: Wake After Sleep Onset.
  
## References

1. P. Bouchequet, D. Jin, G. Solelhac, M. Chennaoui, D. Leger, [«*Morpheo Data Format (MDF), un nouveau format de données simple, robuste et performant pour stocker et analyser les enregistrements de sommeil*»](https://www.sciencedirect.com/science/article/pii/S1769449318301304), Médecine du Sommeil, vol. 15, n 1, p. 48‑49, march 2018.

