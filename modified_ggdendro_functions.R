# # load modified ggdendro functions
# current_wd <- getwd()
# setwd("C:/Users/sdevine/Desktop/R/clustering")
# source("modified_ggdendro_functions.R")
# setwd(current_wd)


# https://atrebas.github.io/post/2019-06-08-lightweight-dendrograms/
# http://andrie.github.io/ggdendro/

# load modified ggdendro functions

dendro_data_k <- function(hc, k) {
        
        hcdata    <-  ggdendro::dendro_data(hc, type = "rectangle")
        seg       <-  hcdata$segments
        labclust  <-  cutree(hc, k)[hc$order]
        segclust  <-  rep(0L, nrow(seg))
        heights   <-  sort(hc$height, decreasing = TRUE)
        height    <-  mean(c(heights[k], heights[k - 1L]), na.rm = TRUE)
        
        for (i in 1:k) {
                xi      <-  hcdata$labels$x[labclust == i]
                idx1    <-  seg$x    >= min(xi) & seg$x    <= max(xi)
                idx2    <-  seg$xend >= min(xi) & seg$xend <= max(xi)
                idx3    <-  seg$yend < height
                idx     <-  idx1 & idx2 & idx3
                segclust[idx] <- i
        }
        
        idx                    <-  which(segclust == 0L)
        segclust[idx]          <-  segclust[idx + 1L]
        hcdata$segments$clust  <-  segclust
        hcdata$segments$line   <-  as.integer(segclust < 1L)
        hcdata$labels$clust    <-  labclust
        
        hcdata
}

set_labels_params <- function(nbLabels,
                              direction = c("tb", "bt", "lr", "rl"),
                              fan       = FALSE) {
        if (fan) {
                angle       <-  360 / nbLabels * 1:nbLabels + 90
                idx         <-  angle >= 90 & angle <= 270
                angle[idx]  <-  angle[idx] + 180
                hjust       <-  rep(0, nbLabels)
                hjust[idx]  <-  1
        } else {
                angle       <-  rep(0, nbLabels)
                hjust       <-  0
                if (direction %in% c("tb", "bt")) { angle <- angle + 45 }
                if (direction %in% c("tb", "rl")) { hjust <- 1 }
        }
        list(angle = angle, hjust = hjust, vjust = 0.5)
}

plot_ggdendro <- function(hcdata,
                          direction   = c("lr", "rl", "tb", "bt"),
                          fan         = FALSE,
                          scale.color = NULL,
                          branch.size = 1,
                          label.size  = 3,
                          nudge.label = 0.01,
                          expand.y    = 0.1) {
        
        direction <- match.arg(direction) # if fan = FALSE
        ybreaks   <- pretty(segment(hcdata)$y, n = 5)
        ymax      <- max(segment(hcdata)$y)
        
        ## branches
        p <- ggplot() +
                geom_segment(data         =  segment(hcdata),
                             aes(x        =  x,
                                 y        =  y,
                                 xend     =  xend,
                                 yend     =  yend,
                                 linetype =  factor(line),
                                 colour   =  factor(clust)),
                             lineend      =  "round",
                             show.legend  =  FALSE,
                             size         =  branch.size)
        
        ## orientation
        if (fan) {
                p <- p +
                        coord_polar(direction = -1) +
                        scale_x_continuous(breaks = NULL,
                                           limits = c(0, nrow(label(hcdata)))) +
                        scale_y_reverse(breaks = ybreaks)
        } else {
                p <- p + scale_x_continuous(breaks = NULL)
                if (direction %in% c("rl", "lr")) {
                        p <- p + coord_flip()
                }
                if (direction %in% c("bt", "lr")) {
                        p <- p + scale_y_reverse(breaks = ybreaks)
                } else {
                        p <- p + scale_y_continuous(breaks = ybreaks)
                        nudge.label <- -(nudge.label)
                }
        }
        
        # labels
        labelParams <- set_labels_params(nrow(hcdata$labels), direction, fan)
        hcdata$labels$angle <- labelParams$angle
        
        p <- p +
                geom_text(data        =  label(hcdata),
                          aes(x       =  x,
                              y       =  y,
                              label   =  label,
                              colour  =  factor(clust),
                              angle   =  angle),
                          fontface = "bold",
                          vjust       =  labelParams$vjust,
                          hjust       =  labelParams$hjust,
                          nudge_y     =  ymax * nudge.label,
                          size        =  label.size,
                          show.legend =  FALSE)
        
        # colors and limits
        if (!is.null(scale.color)) {
                p <- p + scale_color_manual(values = scale.color)
        }
        
        ylim <- -round(ymax * expand.y, 1)
        p    <- p + expand_limits(y = ylim)
        
        p
}



#////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# # example
# mtc <- scale(mtcars)
# D   <- dist(mtc)
# hc  <- hclust(D)
# 
# hcdata <- dendro_data_k(hc, 3)
# 
# 
# #///////////////////////
# 
# 
# # basic dendrogram
# p <- plot_ggdendro(hcdata,
#                    direction   = "lr",
#                    expand.y    = 0.2)
# p
# 
# 
# #///////////////////////
# 
# 
# # customized
# 
# get tree colors
# note that colors are passed in a vector that is k + 1 in length
# the first value is the neutral color for branches above the point at which the tree has been cut up to the root
# the second/third/fourth etc values are indexed to the cluster number; 
# so cluster 1 is second value in colors vector, cluster 2 is third value in colors vector, cluster 3 is fourth value in colors vector
# hcdata$labels
# cols <- c("#a9a9a9", "#1f77b4", "#ff7f0e", "#2ca02c")
# 
# p <- plot_ggdendro(hcdata,
#                    direction   = "tb",
#                    scale.color = cols,
#                    label.size  = 2.5,
#                    branch.size = 0.5,
#                    expand.y    = 0.2)
# 
# p <- p + theme_void() + expand_limits(x = c(-1, 32))
# p
# 
# 
# #///////////////////
# 
# 
# # alternate customization
# 
# get tree colors
# note that colors are passed in a vector that is k + 1 in length
# the first value is the neutral color for branches above the point at which the tree has been cut up to the root
# the second/third/fourth etc values are indexed to the cluster number; 
# so cluster 1 is second value in colors vector, cluster 2 is third value in colors vector, cluster 3 is fourth value in colors vector
# hcdata$labels
# cols <- c("#a9a9a9", "#1f77b4", "#ff7f0e", "#2ca02c")
# 
# p <- plot_ggdendro(hcdata,
#                    direction   = "tb",
#                    scale.color = cols,
#                    label.size  = 2.5,
#                    branch.size = 0.5,
#                    expand.y    = 0.2) +
#         theme(axis.text = element_text(color = "#50505030"),
#               panel.grid.major.y = element_line(color = "#50505030",
#                                                 size  = 0.25))
# p