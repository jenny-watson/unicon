
# The `unicon` R Package <img src="man/figures/logo_trinity.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/Agxiata/unicon/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Agxiata/unicon/actions/workflows/R-CMD-check.yaml)
[![lint](https://github.com/Agxiata/unicon/actions/workflows/lint.yaml/badge.svg)](https://github.com/Agxiata/unicon/actions/workflows/lint.yaml)
[![test-and-snapshots](https://github.com/Agxiata/unicon/actions/workflows/test-and-snapshots.yaml/badge.svg)](https://github.com/Agxiata/unicon/actions/workflows/test-and-snapshots.yaml)
[![test-coverage](https://github.com/Agxiata/unicon/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/Agxiata/unicon/actions/workflows/test-coverage.yaml)
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
2.  A minimal set of functions for easily and transparently converting
    one unit to another.
3.  A simple, safe system for adding to and testing the conversion
    library to maximise maintainability and usefulness.

Why `unicon`? Because the authors are too lazy to type
**un**it\_**con**vert the number of times that developing this package
would otherwise require, and because this simple expediency
serendipitously lands us a consonant away from a legendary mythical
creature :unicorn:

This package was imagined and written by [Dr Jenny
Watson](https://github.com/jenny-watson) and [Dr Alasdair
Sykes](https://github.com/aj-sykes92). If you use this package in your
work, please cite it as:

Watson, J. & Sykes, A. J. (2026) `unicon`: Reliable, transparent unit
conversions in a tidyverse environment. Version 0.0.0.9000. Available at
<https://github.com/Agxiata/unicon/>.

## Installation

This package is hosted on GitHub and can be installed using the
`remotes` package:

``` r
# install.packages("remotes")
remotes::install_github("Agxiata/unicon@*release")
```

## Acknowledgements

The authors would like to thank Tom Watson for designing our hex
sticker. If you would like to use his services, please contact him via
[Instagram](https://www.instagram.com/tom_watson_art).

The package is based around tidyverse ideas and functions, so thanks go
also to Hadley Wickham and the tidyverse team for building and
maintaining this incredible environment.

## Contribute

If you would like to contribute to this package, please file an issue or
make a pull request on GitHub.
