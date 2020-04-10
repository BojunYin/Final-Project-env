#plot data followed by the chapter
#chapter3  Graphics displayed by the DATA distribution
library(tidyverse)
#3-2
stripchart(chorizon$Sc)
stripchart(chorizon$Sc,method = "stack",add = FALSE,at = 0)
stripchart(chorizon$Sc,method = "jitter",add = FALSE,at = 1)
hist(chorizon$Ba,breaks = 50 )




