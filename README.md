# ktools: An R Package With Useful Functions

## Summary

This repository will host both the development and the production version of the package. There are no plans to commit to CRAN. It implements some useful functions and is mainly for my personal use.

## Installation Instructions
Assuming you already have R installed (if not see http://www.r-project.org/),
to installed, you can install the most recent development version using the devtools package.

```
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kudusch/ktools")
```

Note that this will install the `devtools` package and the `ktools` package. 

## Documentation

See help files for detailed documentation.

### get_rank_diff – Calculate rank diff

Implementation of a simple algorithm to find the difference between two corpora. The relative importance of the terms in `tidy_x` are compared to the importance `tidy_y`.

### is.unique – Does a list contain duplicates.
Check if `length(x) == length(unique(x))` and optionally print table.
### library_pls – Library please! Easier Package Installation
If the last error was raised due to calling `library()` on a missing package, install from CRAN and load the package.
![](https://media.giphy.com/media/fxYyiD7BBgwprOsiNI/giphy.gif)
### modus – Calculate mode.
Generic function for mode of a sample.
### ptable – Faster 'prop.table(table())'
Wrapper for `table()` and `prop.table()`. Returns APA compliant percentage values from table data.
### readRDS.gz – Read RDS w/ pigz support
This function uses pigz to read RDS files. pigz is a fully functional replacement for gzip that exploits multiple processors and multiple cores to the hilt when compressing data. [pigz](https://zlib.net/pigz/) must be installed. 
### saveRDS.gz – Save RDS w/ pigz support
See `readRDS.gz`
### readPasteboard - Read from the macOS pasteboard
`readPasteboard()` and its counterpart `writePasteboard()` are implementations of the [clipboard-functions](https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/clipboard) on macOS.
### writePasteboard - Write to the macOS pasteboard
See `readPasteboard`