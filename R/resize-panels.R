
# debugonce(multipanelfigure:::get_png_raster_grob)
# multipanelfigure:::get_png_raster_grob("inst/extdata/Rlogo.png", "mm")

to_mm <- function(x)
{
  convertUnit(x, "mm", valueOnly = TRUE)
}

resizeImage <- function(scaling, imageSize, panelSize)
{
  if(is.numeric(scaling))
  {
    return(scaling * imageSize)
  }
  # else scaling is character
  switch(
    scaling,
    none    = imageSize,
    stretch = unit(c(1, 1), "npc"),
    fit     = {
      sf <- min(
        to_mm(panelSize[1]) / to_mm(imageSize[1]),
        to_mm(panelSize[2]) / to_mm(imageSize[2])
      )
      sf * imageSize
    },
    shrink  = {
      sf <- min(
        1,
        to_mm(panelSize[1]) / to_mm(imageSize[1]),
        to_mm(panelSize[2]) / to_mm(imageSize[2])
      )
      sf * imageSize
    }
  )
}
