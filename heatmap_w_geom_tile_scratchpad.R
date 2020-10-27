library(tidyverse)
library(RColorBrewer)

# https://www.royfrancis.com/a-guide-to-elegant-tiled-heatmaps-in-r-2019/

# heatmaps with geom_tile()

# read csv file
setwd("C:/Users/Stephen/Desktop/R/ggplot2")
m <- read.csv("measles_lev1.csv",header=T,stringsAsFactors=F,skip=2)

# inspect data
head(m)
str(m)
table(m$YEAR)
table(m$WEEK)


#/////////////////////////////


# clean data
m2 <- m %>%
        # convert data to long format
        gather(key="state",value="value",-YEAR,-WEEK) %>%
        # rename columns
        setNames(c("year","week","state","value")) %>%
        # convert year to factor
        mutate(year=factor(year)) %>%
        # convert week to factor
        mutate(week=factor(week)) %>%
        # convert value to numeric (also converts '-' to NA, gives a warning)
        mutate(value=as.numeric(value))

fn_tc <- function(x) paste(str_to_title(unlist(strsplit(x,"[.]"))),collapse=" ")
m2$state <- sapply(m2$state,fn_tc)

# custom sum function returns NA when all values in set are NA,
# in a set mixed with NAs, NAs are removed and remaining summed.
na_sum <- function(x)
{
        if(all(is.na(x))) val <- sum(x,na.rm=F)
        if(!all(is.na(x))) val <- sum(x,na.rm=T)
        return(val)
}

# sum incidences for all weeks into one year
m3 <- m2 %>%
        group_by(year,state) %>%
        summarise(count=na_sum(value)) %>%
        as.data.frame()

m4 <- m3 %>%
        # convert state to factor and reverse order of levels
        mutate(state=factor(state,levels=rev(sort(unique(state))))) %>%
        # create a new variable from count
        mutate(countfactor=cut(count,breaks=c(-1,0,1,10,100,500,1000,max(count,na.rm=T)),
                               labels=c("0","0-1","1-10","10-100","100-500","500-1000",">1000"))) %>%
        # change level order
        mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))

m4 %>% glimpse()


#/////////////////////////////


# plot
# assign text colour
textcol <- "grey40"

# further modified ggplot
ggplot(m4,aes(x=year,y=state,fill=countfactor))+
        geom_tile(colour="white",size=0.2)+
        guides(fill=guide_legend(title="Cases per\n100,000 people"))+
        labs(x="",y="",title="Incidence of Measles in the US")+
        scale_y_discrete(expand=c(0,0))+
        scale_x_discrete(expand=c(0,0),breaks=c("1930","1940","1950","1960","1970","1980","1990","2000"))+
        scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#ddf1da"),na.value = "grey90")+
        #coord_fixed()+
        theme_grey(base_size=10)+
        theme(legend.position="right",legend.direction="vertical",
              legend.title=element_text(colour=textcol),
              legend.margin=margin(grid::unit(0,"cm")),
              legend.text=element_text(colour=textcol,size=7,face="bold"),
              legend.key.height=grid::unit(0.8,"cm"),
              legend.key.width=grid::unit(0.2,"cm"),
              axis.text.x=element_text(size=10,colour=textcol),
              axis.text.y=element_text(vjust=0.2,colour=textcol),
              axis.ticks=element_line(size=0.4),
              plot.background=element_blank(),
              panel.border=element_blank(),
              plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
              plot.title=element_text(colour=textcol,hjust=0,size=14,face="bold"))


#///////////////////////////////////////


# different color palette
ggplot(m4,aes(x=year,y=state,fill=countfactor))+
        geom_tile(colour="white",size=0.2)+
        guides(fill=guide_legend(title="Cases per\n100,000 people"))+
        labs(x="",y="",title="Incidence of Measles in the US")+
        scale_y_discrete(expand=c(0,0))+
        scale_x_discrete(expand=c(0,0),breaks=c("1930","1940","1950","1960","1970","1980","1990","2000"))+
        scale_fill_manual(values=rev(brewer.pal(7,"YlGnBu")),na.value="grey90")
        #coord_fixed()+
        theme_grey(base_size=10)+
        theme(legend.position="right",legend.direction="vertical",
              legend.title=element_text(colour=textcol),
              legend.margin=margin(grid::unit(0,"cm")),
              legend.text=element_text(colour=textcol,size=7,face="bold"),
              legend.key.height=grid::unit(0.8,"cm"),
              legend.key.width=grid::unit(0.2,"cm"),
              axis.text.x=element_text(size=10,colour=textcol),
              axis.text.y=element_text(vjust=0.2,colour=textcol),
              axis.ticks=element_line(size=0.4),
              plot.background=element_blank(),
              panel.border=element_blank(),
              plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
              plot.title=element_text(colour=textcol,hjust=0,size=14,face="bold"))







