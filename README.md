# `rsleep`

## Installation

`rsleep` latest version can be directly installed from Github using the `devtools` package.

```
devtools::install_github("boupetch/rsleep")
```

## Usage

### File manipulation

In `rsleep`, `write_mdf()` and `read_mdf()` functions are used to write and read records on disk. Files are converted from the European Data Format (EDF) to Morpheo Data Format<sup>1</sup> (MDF). MDF is a simple, efficient and interoperable file format for biological timeseries. The format supports raw signal and metadata storage. MDF uses binary files for signals and JSON for metadata.

## References

1. P. Bouchequet, D. Jin, G. Solelhac, M. Chennaoui, D. Leger, [«*Morpheo Data Format (MDF), un nouveau format de données simple, robuste et performant pour stocker et analyser les enregistrements de sommeil*»](https://www.sciencedirect.com/science/article/pii/S1769449318301304), Médecine du Sommeil, vol. 15, n 1, p. 48‑49, march 2018.
