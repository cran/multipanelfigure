[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active)
[![Is the package on CRAN?](http://www.r-pkg.org/badges/version/multipanelfigure)](http://www.r-pkg.org/pkg/multipanelfigure)
[![Build Status](https://semaphoreci.com/api/v1/richierocks/multipanelfigure/branches/master/badge.svg)](https://semaphoreci.com/richierocks/multipanelfigure)
[![Build status](https://ci.appveyor.com/api/projects/status/rvbkefi5rinsudlu/branch/master?svg=true)](https://ci.appveyor.com/project/richierocks/multipanelfigure/branch/master)

# multipanelfigure

Tools to create a layout for figures made of multiple panels, and to fill the panels with base,
lattice and ggplot2 plots, grobs, and bitmap images as supported by `ImageMagick` (through the
package `magick`).

## Installation

To install the package, you first need the 
[*devtools*](https://github.com/hadley/devtools) package.

```{r}
install.packages("devtools")
```

Then you can install the *multipanelfigure* package using

```{r}
devtools::install_bitbucket("graumannlabtools/multipanelfigure")
```

## Specifying a layout

Layouts are matrices of panels that contains plots, grobs or images.  Create them with the `multi_panel_figure` function.

You can specify the total width and height of the figure (the default unit is millimetres):

```{r}
library(multipanelfigure)
figure1 <- multi_panel_figure(
  width = 180, height = 180,
  columns = 3, rows = 3)
```

Or you can specify widths and heights of individual columns and rows.

```{r}
figure2 <- multi_panel_figure(
  width = c(20, 30, 40),
  height = c(10, 20, 30))
```

When no panels are filled, printing the figure (by typing its name) displays the layout.  Notice that by default a 5 mm spacer is included between rows and between columns.  (This can be adjusted using `multi_panel_figure`'s `row_spacing` and `column_spacing` arguments.)

```{r}
figure1
```

![An image of the layout of an empty multi-panel figure.](https://bitbucket.org/graumannlabtools/multipanelfigure/downloads/readme_figure_empty.png)

## Adding panels

Plots and images are added into the figure by filling panels using `fill_panel`.

*lattice* plot variables are added directly. The syntax is cleanest using *magrittr* pipes:

```{r}
library(lattice)
library(magrittr)
a_lattice_plot <- xyplot(height ~ age, Loblolly)
figure1 %<>% fill_panel(a_lattice_plot)
```

*ggplot2* plots variables are also added directly.  In this case, to put it in the second column from the left (top row), specify the `left_panel` argument.

```{r}
library(ggplot2)
library(magrittr)
a_ggplot <- ggplot(Loblolly, aes(age, height)) + geom_point()
figure1 %<>% fill_panel(a_ggplot, column = 2)
```

Plots created using base graphics must be converted to grid-based plots and captured using `capture_base_plot`.

```{r}
a_base_plot <- capture_base_plot(
  with(Loblolly, plot(age, height))
)
figure1 %<>% fill_panel(a_base_plot, column = 3)
```

*grid* grobs are also added directly.  Plots and images can be made to span multiple panels by defining starting and stopping columns using `column` and/or starting and stopping rows using `row`. The following example adds the grob to the second and third rows from the top (first column).

```{r}
library(grid)
a_grob <- linesGrob(arrow = arrow())
figure1 %<>% fill_panel(a_grob, row = 2:3)
```


Bitmap images (such as JPEG, PNG, and TIFF images) are added via a string giving their
location: either a path to a location on disk, or a URL.  

```{r}
figure1 %<>% fill_panel(
  "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1b/Persian_Cat_%28kitten%29.jpg/657px-Persian_Cat_%28kitten%29.jpg",
  column = 2:3,
  row = 2:3)
```

Once panels have been added to the figure, printing it displays the figure.

```{r}
figure1
```

![An image of the filled multi-panel figure, containing several plots and images.](https://bitbucket.org/graumannlabtools/multipanelfigure/downloads/readme_figure_filled.png)