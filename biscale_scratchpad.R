library(tidyverse)
library(cowplot)
library(biscale)

# https://cran.r-project.org/web/packages/biscale/vignettes/biscale.html

# inspect initial sf data
stl_race_income

# add bi_class variable, which is just a string listing the x/y color palette pairing for each value
# if NA values, will need to manually set bi_class variable to NA, and then na.translate = FALSE arg in bi_scale_fill
data <- bi_class(stl_race_income, x = pctWhite, y = medInc, style = "quantile", dim = 3)
data 
data %>% glimpse()

# create map
map <- ggplot() +
        geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
        bi_scale_fill(pal = "DkBlue", dim = 3, na.translate = FALSE) +
        bi_theme()
map

# create legend
legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Higher % White ",
                    ylab = "Higher Income ",
                    size = 8)

# add legend
ggdraw() +
        draw_plot(map, 0, 0, 1, 1) +
        draw_plot(legend, 0.2, .65, 0.2, 0.2)





