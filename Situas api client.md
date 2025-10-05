Situas api client

The web site https://situas.istat.it/web/#/apiDetail reports a list of APIs built by the national institute for statistics in Italy (ISTAT) to facilitate the download and use of territorial codes and classifications.

The scope if the R package is:

- provide a set of functions that retrieve the appropriate tables;
- make possible a reclassification process: when an old code is present in a dataset, recode it to the new one
- provide batch detection of territorial codes in datasets and batch cleaning / recoding.

The package is structured as a standard R library with CRAN standards:
- use “testthat” library  to develop unit tests 
- use “devtools” for testing and developemnt
- use “roxygen” for documentation
- use “renv” for installation of the environment and dependencies
- use “data.table” for data management and wrangling 
- use vectorized code for speed and avoid for loops
- use caching for off line use

Always use thinking and agents for the different development phases, testing and documentation.

