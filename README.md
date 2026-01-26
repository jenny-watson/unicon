# Unit conversion working project

*Alasdair Sykes, 23/01/26*

## Overview

This project is a very quick and dirty extract of some unit conversion functionality we (Trinity Natural Capital Engineering) have been working on in order to meet the needs of various ongoing projects.

The basic design intent behind the approach is to create a simple, succinct set of schemas that provide the information required to perform unit conversion, offer minimum required functionality, avoid duplication of information and provide for ease of maintenance/update/addition.

The intent of this project is to provide a reprex for this core functionality and concept, so that we (Alasdair and Jenny) can work together on and convert into a proper, self-contained R package.

## Repo contents/design

The `units` sub-directory contains JSON schemas which contain all required information for the conversion functionality.

The files at `units/base/*/*.json` provide the core schemas, with:

1. a unit category,
2. a standard index unit (consistent across categories),
3. a conversion model in the form of slope and intercept to convert to the standard index unit, and
4. a unit alias array, providing other ways of naming the unit

The files at `units/derived/*.json` provide a definition of derived units as a function of their base unit components (e.g. rate-type units like tonnes per hectare, or density-type units like kg per litre).

The files at `units/operators/*.json` provide definitions for the operators that are inherent in the derived units.

The file `si.json` provides a checkable directory for the standard index units required by the base models.

Finally, the R script `unit-conv-build.R` reads in the files from the `units` directory and combines their information into a verbose, calculation-friendly set of files for conducting conversions. Amongst other things, the script serves to mathematically combine the conversion models for base units to create new models for derived units. The end results are written to .rds files in the root directly.

## Suggested project direction

The following is suggested for our joint efforts on this project:

1. Convert the repo/its contents into a package format and get it versioned on GitHub.
2. Write a wrapper function to export from the package to perform conversion for a user based on the core data/models.
3. Work through the package development process, complete documentation, and ensure all checks etc are passing.
4. Implement Continuous Integration to run checks etc. via GitHub Actions whenever development occurs.
5. Consider vulnerabilities of the approach and write unit tests or structural improvements to address them.
6. Stretch goal: Write an integrated `plumber` API endpoint for this functionality.
