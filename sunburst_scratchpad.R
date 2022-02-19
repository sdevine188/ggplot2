library(tidyverse)
library(RColorBrewer)
library(scales)
library(colorspace)

# http://www.sdinnovation.com.au/blog/create-sunburst-diagram-in-r-with-ggplot-and-dplyr
# https://plotly.com/r/sunburst-charts/
# https://www.data-imaginist.com/2017/ggraph-introduction-layouts/
# https://towardsdatascience.com/visualize-nested-data-with-sunburst-plots-in-r-a65710388379
# https://www.pipinghotdata.com/posts/2021-06-01-custom-interactive-sunbursts-with-ggplot-in-r/
# https://ggplot2.tidyverse.org/reference/coord_polar.html

df <- structure(
        list( genre = c("Arts & Photo", "Children", "Children", "Children", "Children", "Children", "Children", "Technology", "Mystery", "Mystery", "Mystery", "Mystery", "Nonfiction", "Nonfiction", "Nonfiction", "Magazine", "Magazine", "Magazine", "Magazine", "Magazine", "Magazine", "Romance", "Sci-Fi"), subgenre = c(NA, "Baby Books", "Age 3-5", "Age 3-5", "Age 3-5", "Age 6-8", "Teen", "Troubleshoot", "Crime", "Crime", "Spy", "Spy", "Health", "Health", "History", "Fashion", "Fashion", "Home", "Other", "Sports", "Sports", NA, NA), revenue = c(5200, 16092, 24514, 17771, 13295, 14046, 18046, 4527, 11186, 8790, 6516, 3809, 3293, 6891, 1131, 7315, 2222, 2612, 3140, 8009, 4257, 18980, 19200)), .Names = c("genre", "subgenre", "revenue"), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -23L)
)
df %>% glimpse()

bardata <- df %>%
        group_by( genre ) %>%
        summarise( tot_rev = sum( revenue ) )
bardata
ggplot( bardata ) +
        geom_bar(
                aes( x = 2.5, y = tot_rev, fill = genre ),
                stat = "identity"
        )
ggplot( bardata ) +
        geom_bar(
                aes( x = 2.5, y = tot_rev, fill = genre ),
                stat = "identity"
        ) +
        coord_polar( theta = "y" )
rectdata <- df %>%
        group_by( genre ) %>%
        summarise( tot_rev = sum( revenue ) ) %>%
        ungroup() %>%
        mutate(
                ymax = cumsum( tot_rev ),
                ymin = lag( ymax, n = 1, default = 0 )
        )
rectdata
ggplot( rectdata ) +
        geom_rect(
                aes( xmin = 2, xmax = 3, ymin = ymin, ymax = ymax, fill = genre ),
                color = "white"
        ) +
        xlim( 0, 4 ) + theme_bw()
innerCircle <- ggplot( rectdata ) +
        geom_rect(
                aes( xmin = 2, xmax = 3, ymin = ymin, ymax = ymax, fill = genre ),
                color = "white"
        ) +
        xlim( 0, 4 )
innerCircle
innerCircle + coord_polar( theta = "y" )
outerCircleData <- df %>%
        group_by( genre, subgenre ) %>%
        summarise( tot = sum( revenue ) ) %>%
        left_join( rectdata %>% select( genre, tot_rev) ) %>%
        ungroup() %>%
        mutate(
                ymax = cumsum(tot),
                ymin = lag( ymax, n = 1, default = 0 )
        ) %>%
        filter( !is.na( subgenre ) )
outerCircleData
outerCircle <-
        geom_rect(
                data = outerCircleData,
                aes( xmin = 3, xmax = 4, ymin = ymin, ymax = ymax, fill = subgenre ),
                color = "white"
        )
outerCircle
innerCircle + outerCircle
innerCircle + outerCircle + coord_polar( theta = "y" )


pbi_pal <-
        c( "#FE9666", "#A66999", "#FD625E", "#F2C80F",
           "#5F6B6D", "#8AD4EB", "#01B8AA", "#374649" )

rectdata$color <- pbi_pal
rectdata

rectdata <- rectdata %>%
        mutate(
                colorL = coords(as(hex2RGB(color), "polarLUV"))[,1],
                colorC = coords(as(hex2RGB(color), "polarLUV"))[,2],
                colorH = coords(as(hex2RGB(color), "polarLUV"))[,3]
        )
rectdata

innerCircle <- ggplot( rectdata ) +
        geom_rect(
                aes( xmin = 2, xmax = 3, ymin = ymin, ymax = ymax ),
                color = "white",
                fill = rectdata$color
        ) +
        xlim( 0, 4 )
innerCircle
outerCircleData <- outerCircleData %>%
        left_join(
                rectdata %>% select( genre, colorL, colorC, colorH ),
                by = "genre"
        ) %>%
        mutate( outerColor = hex( polarLUV( colorL, colorC * 0.3, colorH ) ) )
outerCircleData
outerCircle <-
        geom_rect(
                data = outerCircleData,
                aes( xmin = 3, xmax = 4, ymin = ymin, ymax = ymax ),
                color = "white",
                fill = outerCircleData$outerColor
        )
outerCircle
allCircles <- innerCircle + outerCircle + coord_polar( theta = "y" )
allCircles

theme_blank <- theme_bw() +
        theme(
                panel.grid = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                panel.border = element_blank(),
                legend.title = element_blank(),
                plot.title = element_text( hjust = 0.5 )
        )

allCircles +
        theme_blank +
        labs( title = "Book Sales Summary" )


#///////////////


rectdata2 <- rectdata %>% mutate(genre = case_when(genre == "Technology" ~ "", TRUE ~ genre),
                                color_bin = genre,
                                color = case_when(color_bin == "" ~ "#ffffff", TRUE ~ color))
rectdata2

outerCircleData2 <- df %>%
        group_by( genre, subgenre ) %>%
        summarise( tot = sum( revenue ) ) %>%
        left_join( rectdata %>% select( genre, tot_rev) ) %>%
        ungroup() %>%
        mutate(
                ymax = cumsum(tot),
                ymin = lag( ymax, n = 1, default = 0 )
        ) %>%
        filter( !is.na( subgenre ) ) %>%
        filter(genre != "Technology") %>%
        left_join(
                rectdata2 %>% select( genre, colorL, colorC, colorH ),
                by = "genre"
        ) %>%
        mutate( outerColor = hex( polarLUV( colorL, colorC * 0.3, colorH ) ) )
outerCircleData2

outerCircleData %>% ggplot(data = .) + 
        geom_rect(
                # data = .,
                aes( xmin = 3, xmax = 4, ymin = ymin, ymax = ymax ),
                color = "white",
                fill = outerCircleData$outerColor
        )

outerCircleData2 %>% ggplot(data = .) + 
        geom_rect(
                # data = .,
                aes( xmin = 3, xmax = 4, ymin = ymin, ymax = ymax ),
                color = "white",
                fill = outerCircleData2$outerColor
        )

chart_data_color_list <- rectdata2 %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- rectdata2 %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list

rectdata2 %>% 
        mutate(label_x = ((3 - 2) / 2) + 2,
               label_y = ((ymax - ymin) / 2) + ymin) %>%
        ggplot(data = .) +
        geom_rect(
                aes( xmin = 2, xmax = 3, ymin = ymin, ymax = ymax, fill = color_bin),
                color = "white"
        ) +
        geom_text(mapping = aes(x = label_x, y = label_y, label = genre), size = 3) +
        scale_fill_manual(values = chart_data_color_list) +
        geom_rect(
                data = outerCircleData2,
                aes( xmin = 3, xmax = 4, ymin = ymin, ymax = ymax ),
                color = "white",
                fill = outerCircleData2$outerColor
        ) +
        xlim( 0, 4 ) +
        coord_polar( theta = "y" ) + theme_blank
