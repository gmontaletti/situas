# situas - R Client for SITUAS API

[![Version](https://img.shields.io/badge/version-0.2.0-blue.svg)](https://github.com/gmontaletti/situas)

R package for accessing the SITUAS (Sistema Informativo Territoriale delle Unità Amministrative e Statistiche) API - a set of APIs built by ISTAT (Italian National Institute for Statistics) to retrieve territorial codes and classifications.

Website: https://situas.istat.it/

## Overview

The scope of this R package is:

- provide a set of functions that retrieve the appropriate tables;
- make possible a reclassification process: when an old code is present in a dataset, recode it to the new one
- provide batch detection of territorial codes in datasets and batch cleaning / recoding.

The package is structured as a standard R library with CRAN standards:
- use “testthat” library  to develop unit tests 
- use “devtools” for testing and developemnt
- use “roxygen” for documentation
- use “renv” for installation of the environment and dependencies
- use "data.table" for data management and wrangling
- use vectorized code for speed and avoid for loops
- use caching for off line use

## Citation

If you use this package in your research, please cite it as:

```
Montaletti, G. (2025). situas: Client for the SITUAS API - Italian Territorial Codes and Classifications.
R package version 0.2.0. https://github.com/gmontaletti/situas
```

## Author

Giampaolo Montaletti
Email: giampaolo.montaletti@gmail.com
GitHub: https://github.com/gmontaletti
ORCID: https://orcid.org/0009-0002-5327-1122

