# BiocCertificate

## Overview

`BiocCertificate` helps conference attendees generate a PDF certificate of
attendance.

## Usage

The shiny application can be started with the `Run App` button in RStudio or
using the `BiocCertificate()` main function.

<a href="https://github.com/Bioconductor/BiocCertificate">
<img src="https://raw.githubusercontent.com/Bioconductor/BiocCertificate/devel/inst/images/BiocCertificateForm.png"
width="600" height="439"/>
</a>

## Supported Events

A `yml` file of events is located in `inst/resources/events.yml`. The file
contains metadata that is to be updated by event id (`eid`).

## Installation

``` r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
    
BiocManager::install("Bioconductor/BiocCertificate")
```
