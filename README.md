
# The `unicon` R Package <img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/jenny-watson/unit_conversion/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jenny-watson/unit_conversion/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

*Reliable, transparent unit conversions in a `tidyverse` environment*

Unit conversion is theoretically simple and practically complex. Despite
being a skill we all learn and apply as schoolchildren, the requirement
to convert between units of different types is a source of friction,
frustration and errors in many data analysis workflows. In the authors’
software team, a developer tasked with the implementation of a
platform-wide unit conversion system observed: *“This is the worst task
anyone could have given me. Get it right and no-one notices; get it
wrong, or try and explain how difficult it is, and you wind up looking
like a right idiot.”*

He would not be the first to [stumble
here](https://en.wikipedia.org/wiki/Gimli_Glider); nor would his errors
be the [most
expensive](https://en.wikipedia.org/wiki/Mars_Climate_Orbiter).

Inspired by his plight and the very real real-world challenges of
maintaining a living, breathing unit conversion library, the `unicon`
package was designed on the principle of maximum information density and
minimum margin for inconsistency to provide a reliable, maintainable,
`tidyverse` style interface for easy unit conversions in R data
analysis.

The package is designed to provide:

1.  A set of commonly used units, codified by their relation to one
    another and their many human-readable aliases.
2.  A mimimal set of functions for easily and transparently converting
    one unit to another.
3.  A simple, safe system for adding to and testing the conversion
    library to maximise maintainability and usefulness.

This package was imagined and written by [Dr Jenny
Watson](https://github.com/jenny-watson) and [Dr Alasdair
Sykes](https://github.com/aj-sykes92). If you use this package in your
work, please cite it as:

Watson, J. & Sykes, A. J. (2026) `unicon`: Reliable, transparent unit
conversions in a tidyverse environment. Version 0.0.0.9000. Available at
<https://github.com/jenny-watson/unicon/>.

> Zenodo DOI badge here following release

## Installation

This package is hosted on Github and can be installed using the
`remotes` package:

``` r
# install.packages("remotes")
remotes::install_github("jenny-watson/unicon@*release")
```

## A brief note on design and methodology

If your use of `unicon` falls firmly into the “casual” category, you can
probably skip this section. However, we recommend reading it (or
returning to it later) if you plan to rely on `unicon`’s functions in
any kind of higher stakes setting, or on developing/modifying your own
conversion protocols.

# Mermaid example

``` mermaid
graph TD
  A[Start] --> B{Decision}
  B -->|Yes| C[Do thing]
  B -->|No| D[Do other thing]
  C --> E[End]
  D --> E
```

``` json
{
  "category": "area",
  "si": "ha",
  "model": {
    "slope": 0.4047,
    "intercept": 0
  },
  "alias": [
    "ac",
    "acre",
    "acres"
  ]
} 
```

## Usage — `unicon_full`

The core user function of the `unicon` package is `unicon_full`. Use it
like this:

``` r
library(unicon)

raw_values <- c(54.21, 71.24, 55.81, 11.33, 70.59)
raw_units <- c("tonnes / ha", "tons per acre", "t/ha", "kg /Hectare", "g/m2")
unit_out <- "tonnes / ha"

unicon_full(
  value_in = raw_values,
  unit_in = raw_units,
  unit_out = "tonnes / ha",
  pull = TRUE
)
```

    ## [1]  54.21000 159.69320  55.81000   0.01133   0.70590

The function uses `unicon`’s extensive library of unit aliases to match
the highly varied raw input units to their corresponding unit IDs, and
subsequently converts them to the specified output unit.

A number of features are built into the function to help keep
conversions safe and transparent. Firstly, the user may wish to see the
full calculation thread which leads to this outcome, which can be
achieved by setting the `pull` argument to FALSE:

``` r
unicon_full(
  value_in = raw_values,
  unit_in = raw_units,
  unit_out = "tonnes / ha",
  pull = FALSE
)
```

    ## # A tibble: 5 × 13
    ##   unit_in       unit_out alias_in alias_out id_in id_si id_out error_in error_si
    ##   <chr>         <chr>    <chr>    <chr>     <chr> <chr> <chr>  <lgl>    <lgl>   
    ## 1 tonnes / ha   tonnes … tonnes/… tonnes/ha t__ha g__ha t__ha  FALSE    FALSE   
    ## 2 tons per acre tonnes … tonsper… tonnes/ha st__… g__ha t__ha  FALSE    FALSE   
    ## 3 t/ha          tonnes … t/ha     tonnes/ha t__ha g__ha t__ha  FALSE    FALSE   
    ## 4 kg /Hectare   tonnes … kg/hect… tonnes/ha kg__… g__ha t__ha  FALSE    FALSE   
    ## 5 g/m2          tonnes … g/m2     tonnes/ha g__m… g__ha t__ha  FALSE    FALSE   
    ## # ℹ 4 more variables: error_out <lgl>, value_in <dbl>, value_si <dbl>,
    ## #   value_out <dbl>

This returns the following information, representing the calculation
process in rough left –\> right chronology:

- `unit_in` The user-supplied `unit_in` argument.
- `unit_out` The user-supplied `unit_out` argument.
- `alias_in` The derived alias associated to the `unit_in` argument,
  generated by stripping whitespace and converting all characters to
  lowercase.
- `alias_out` The derived alias associated to the `unit_out` argument,
  derived similarly.
- `id_in` The ID acquired for the `alias_in` variable.
- `id_si` The ID associated with the “SI” (standard reference point)
  unit for the type class of the input and output units.
- `id_out` The ID acquired for the `alias_out` variable.
- `error_in` Logical; were there errors in processing the input unit?
- `error_si` Logical; were there errors in deriving the SI unit?
- `error_out` Logical; were there errors in deriving the output unit?
- `value_in` The user-supplied `value_in` argument.
- `value_si` The values as converted to their SI (standard reference
  point) unit.
- `value_out` The

All the core input arguments are vectorised and will attempt to
replicate if supplied as scalar values.

## Usage — `unicon_lite`

## Usage — `unicon_catrel`

## Usage — `unicon_help`

## Acknowledgements

## Contribute

If you would like to contribute to this package, please file an issue,
make a pull request on GitHub, or email the authors at **TBC**.
