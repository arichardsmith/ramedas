# ramedas

Historical AMEDAS data files downloaded from the [Japan Meteorological Association website](https://www.data.jma.go.jp/gmd/risk/obsdl/) have multiple header rows, which stops `read_csv` from loading them properly.
This package reads the files and coerces them into a format that plays nicely with tidy data principles. The package also provides convenience functions for working with the data.

## Installation

Install the package directly from github using `devtools`
```r
# install.packages("devtools")
devtools::install_github("tidyverse/readxl")
```

## Usage

```r
library(ramedas)
```

Calling `read_amedas_csv` with the path of your file will give you a long data frame with various metadata attached.

```{r}
data <- read_amedas_csv("amedas_file.csv")
```

You can use `pivot_measurements` to pivot each measurement to a column. This requires you to decide the minimum acceptable level for `品質情報` (quality).

```{r}
pivot_measurements(data, min_quality = 8)
```
The package also includes some helper functions to inspect `品質情報`, `summarise_quality` and `visualize_quality_over_time`. For a more detailed introduction check out the [vignette](vignettes/reading-a-file.Rmd).

## Contributing

I only use AMEDAS data to satisfy my own personal interest in Japan's weather. Feedback and contributions from any others users would be greatly appreciated.
Specifically, I will eventually have a go at writing Japanese documentation for this package. But, as Japanese isn't my native language, any input from a native speaker would be good. I also plan to add translation functions to make the data easier to work with for non-Japanese speakers (or people who hate typing Japanese in RStudio...)
