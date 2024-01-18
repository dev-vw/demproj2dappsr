# `demproj2dappsr` Tutorial

This package contains a wrapper function `convert_dp_to_dappsr(...)` that takes a Spectrum DemProj file and converts it to a R list data structure usable for `dappsr` projections.

## Installation

To install this package, simply run `devtools::install_github("dev-vw/demproj2dappsr")`. Please note that you will need the `devtools` package installed.

## Usage

The main wrapper function is `convert_dp_to_dappsr(...)`. Simply provide the file path to the Spectrum DemProj `.dp` file. Only works with `<General1>` DemProj files.
