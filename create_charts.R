library(tidyverse)
library(lubridate)
# library(janitor)
library(readxl)
# library(naniar)
library(testthat)
library(scales)
library(openxlsx)
# library(UpSetR)
# library(fs)
library(extrafont)
library(officer)
library(devEMF)
library(RColorBrewer)
library(scales)
library(grid)


# setwd
setwd("X:/Asylum_EAD_study")

options(scipen=999)


# load validate_anumbers function
source("code/helper_scripts/validate_anumbers.R")

# load get_invalid_anumbers function
source("code/helper_scripts/get_invalid_anumbers.R")

# load as_percent function
source("code/helper_scripts/as_percent.R")

# load round_to_digits()
source("code/helper_scripts/round_to_digits.R")

# load add_group_index()
source("code/helper_scripts/add_group_index.R")

# load get_variation_functions()
source("code/helper_scripts/get_variation_functions.R")

# load fy()
source("code/helper_scripts/fy.R")

# load add_dummies()
source("code/helper_scripts/add_dummies.R")

# load prd_format function
source("code/helper_scripts/prd_format/prd_format.R")

# load add_linebreaks function
source("code/helper_scripts/add_linebreaks.R")


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# read in joined data
setwd("C:/Users/sjdevine/Work Folders/Desktop/asylum_ead")
data <- read_csv("data/joined_data_20200408.csv", col_types = cols(reopen_date = col_date())) 

# inspect
data %>% glimpse()
data %>% nrow() # 672403
data %>% distinct(anumber) %>% nrow() # 672403


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# note for blues color palette (n = 9)
# when 1 group: 7
# when 2 groups: 5, 9
# when 3 groups: 9, 7, 5
# when 4 groups: 9, 7, 5, 3
# when 5 groups: 9, 8, 7, 5, 3


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# raio_outcome_rates_by_fy_cohort_chart ####


# create chart_data
chart_data <- data %>% add_dummies(outcome_bucket) %>% group_by(filing_date_fy) %>%
        summarize(adj_count = sum(adjudicated_case_flag),
                  grant_count = sum(outcome_bucket.grant),
                  denial_count = sum(outcome_bucket.deny),
                  referral_count = sum(outcome_bucket.referral_w_interview, outcome_bucket.referral_w_one_year_limit)) %>% 
        ungroup() %>%
        mutate(grant_pct = grant_count / adj_count, 
               denial_pct = denial_count / adj_count, 
               referral_pct = referral_count / adj_count) %>%
        select(filing_date_fy, grant_pct, denial_pct, referral_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "rate", values_to = "value") 


#///////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(rate == "denial_pct" ~ "Denials",
                                                               rate == "grant_pct" ~ "Grants",
                                                               rate == "referral_pct" ~ "Referrals"),
                                  color = case_when(rate == "denial_pct" ~ color_palette %>% slice(4) %>% pull(hex),
                                                         rate == "grant_pct" ~ color_palette %>% slice(6) %>% pull(hex),
                                                         rate == "referral_pct" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////


# create title

# title <- expression(atop(bold(paste("I-589 decisions as share of adjudicated"^"1", " cases,")),
#                          bold(paste("by I-589 FY filing cohort"))))
title <- "I-589 RAIO outcomes as a share of those adjudicated by RAIO,\nby I-589 FY filing cohort\n(for FY 2009-2018 filing cohorts)"

# create footnotes

# note that using expression for foonotes requires atop() because expression can't handle newlines, but atop can't be left justified, so use cowplot
# footnotes <- expression(atop(paste(""^"1", "Adjudicated cases are those referred, granted, or denied; cases administratively closed or pending are excluded."),
#                         paste("Source: USCIS Refugee, Asylum, & International Operations; USCIS Office of Policy & Strategy")))

# footnotes <- str_c(add_linebreaks(" Adjudicated cases are those referred, granted, or denied; cases administratively closed or pending are excluded."), 
#                    "\n",
#                    add_linebreaks("Source: USCIS RAIO; USCIS OP&S"), 
#                    sep = "")

footnotes <- c("Source: USCIS RAIO; USCIS OP&S")


#////////////////////////////////////////


# create raio_outcome_rates_by_fy_cohort_chart
raio_outcome_rates_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, color = fct_reorder2(.f = color_bin, .x = filing_date_fy, .y = value))) + 
        geom_line(size = 2) + geom_point(size = 4) +
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, expand = c(0, 0), limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share of\nadjudicated cases", 
             title = title,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2) +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 20, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 10, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 8, r = 0, b = -10, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
raio_outcome_rates_by_fy_cohort_chart


#////////////////////////////////////////


# draw superscript for footnotes
# raio_outcome_rates_by_fy_cohort_chart_formatted <- ggdraw(raio_outcome_rates_by_fy_cohort_chart) + 
#         draw_label(label = "1", x = .132, y = .276, size = 7, fontfamily = "Calibri", fontface = "plain", color = "#000000")
# 

#//////////////////////////////////////



# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(raio_outcome_rates_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/raio_outcome_rates_by_fy_cohort_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# raio_outcome_rates_by_fy_chart ####


# create chart_data
chart_data <- data %>% 
        add_dummies(outcome_bucket) %>% 
        group_by(outcome_date_fy) %>%
        summarize(adj_count = sum(adjudicated_case_flag),
                  grant_count = sum(outcome_bucket.grant),
                  denial_count = sum(outcome_bucket.deny),
                  referral_count = sum(outcome_bucket.referral_w_interview, outcome_bucket.referral_w_one_year_limit)) %>% 
        ungroup() %>%
        filter(!is.na(outcome_date_fy)) %>% 
        mutate(grant_pct = grant_count / adj_count, 
               denial_pct = denial_count / adj_count, 
               referral_pct = referral_count / adj_count) %>%
        select(outcome_date_fy, grant_pct, denial_pct, referral_pct) %>%
        pivot_longer(cols = -outcome_date_fy, names_to = "rate", values_to = "value")

# inspect
chart_data


#///////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(rate == "denial_pct" ~ "Denials",
                                                          rate == "grant_pct" ~ "Grants",
                                                          rate == "referral_pct" ~ "Referrals"),
                                    color = case_when(rate == "denial_pct" ~ color_palette %>% slice(4) %>% pull(hex),
                                                      rate == "grant_pct" ~ color_palette %>% slice(6) %>% pull(hex),
                                                      rate == "referral_pct" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "I-589 RAIO outcomes as a share of those adjudicated by RAIO, by FY\n(for FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- "Source: USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create raio_outcome_rates_by_fy_chart
raio_outcome_rates_by_fy_chart <- chart_data %>%
        ggplot(data = ., aes(x = outcome_date_fy, y = value, color = fct_reorder2(.f = color_bin, .x = outcome_date_fy, .y = value))) + 
        geom_line(size = 2) + geom_point(size = 4) +
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2020, by = 2),limits = c(2008, 2021)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "FY", y = "Share of\n adjudicated cases", 
             title = title,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2) +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 20, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 10, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 8, r = 0, b = -10, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
raio_outcome_rates_by_fy_chart


#//////////////////////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(raio_outcome_rates_by_fy_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/raio_outcome_rates_by_fy_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# raio_filed_adj_pending_by_fy_chart ####

# get filings_by_fy
filings_by_fy <- data %>% 
        group_by(filing_date_fy) %>% count() %>% rename(fy = filing_date_fy, filing_count = n) %>% ungroup()
filings_by_fy

# get new_pending_by_fy
new_pending_by_fy <- data %>% 
        mutate(new_pending_flag = case_when((filing_date_fy != outcome_date_fy) | is.na(outcome_date_fy) ~ 1, TRUE ~ 0)) %>%
        group_by(filing_date_fy) %>% count(new_pending_flag) %>% ungroup() %>% filter(new_pending_flag == 1) %>%
        rename(fy = filing_date_fy, new_pending_count = n) %>% select(-new_pending_flag)
new_pending_by_fy

# get chart_data
chart_data <- data %>% 
        add_dummies(outcome_bucket) %>% 
        group_by(outcome_date_fy) %>%
        summarize(adj_count = sum(adjudicated_case_flag),
                  grant_count = sum(outcome_bucket.grant),
                  denial_count = sum(outcome_bucket.deny),
                  referral_count = sum(outcome_bucket.referral_w_interview, outcome_bucket.referral_w_one_year_limit),
                  admin_closed_count = sum(outcome_bucket.admin_closed)) %>% 
        ungroup() %>%
        filter(!is.na(outcome_date_fy), outcome_date_fy <= 2018) %>% 
        mutate(grant_pct = grant_count / adj_count, 
               denial_pct = denial_count / adj_count, 
               referral_pct = referral_count / adj_count) %>%
        rename(fy = outcome_date_fy) %>%
        left_join(., filings_by_fy, by = "fy") %>%
        left_join(., new_pending_by_fy, by = "fy") %>%
        mutate(cum_filing_count = cumsum(replace_na(filing_count, replace = "0")),
               cum_adj_count = cumsum(replace_na(adj_count, replace = "0")),
               cum_admin_closed_count = cumsum(replace_na(admin_closed_count, replace = "0")),
               cum_pending_count = cum_filing_count - cum_adj_count - cum_admin_closed_count) %>%
        select(-c(cum_filing_count, cum_adj_count, cum_admin_closed_count)) %>%
        select(fy, filing_count, adj_count, new_pending_count, cum_pending_count) %>%
        pivot_longer(cols = -fy, names_to = "var", values_to = "value")

# inspect
chart_data


#///////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "filing_count" ~ "Filed",
                                                          var == "adj_count" ~ "Adjudicated",
                                                          var == "new_pending_count" ~ "Added to pending",
                                                          var == "cum_pending_count" ~ "Cumulative pending"),
                                    color = case_when(var == "filing_count" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "adj_count" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      var == "new_pending_count" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "cum_pending_count" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "I-589 RAIO filings, adjudications, and pending cases, by FY\n(for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- "Source: USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create raio_filed_adj_pending_by_fy_chart
raio_filed_adj_pending_by_fy_chart <- chart_data %>%
        ggplot(data = ., aes(x = fy, y = value, color = fct_reorder2(.f = color_bin, .x = fy, .y = value))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(labels = unit_format(unit = "", scale = .001)) +
        labs(x = "FY", y = "Case count (thousands)", 
             title = title,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/50000) +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 10, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 8, r = 0, b = -10, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 8, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
raio_filed_adj_pending_by_fy_chart


#//////////////////////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(raio_filed_adj_pending_by_fy_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/raio_filed_adj_pending_by_fy_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# eoir_outcome_rates_by_fy_cohort_chart ####


# get referral_received_by_filing_fy
referral_received_by_filing_fy <- data %>% group_by(filing_date_fy) %>% summarize(referral_received_count = sum(eoir_received_flag)) %>%
        rename(fy = filing_date_fy) 
referral_received_by_filing_fy

# get chart_data
chart_data <- data %>% add_dummies(eoir_outcome) %>%
        group_by(filing_date_fy) %>%
        summarize(adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure)) %>%
        rename(fy = filing_date_fy) %>%
        left_join(., referral_received_by_filing_fy, by = "fy") %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               relief_granted_rate = relief_granted_count / adj_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               removal_vol_dep_rate = removal_vol_dep_count / adj_count,
               terminated_rate = terminated_count / adj_count) %>%
        select(fy, relief_granted_rate,
               removal_vol_dep_rate,
               terminated_rate) %>%
        pivot_longer(cols = -fy, names_to = "var", values_to = "value")

# inspect
chart_data


#///////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "terminated_rate" ~ "Terminated",
                                                          var == "removal_vol_dep_rate" ~ "Removal / Voluntary departure",
                                                          var == "relief_granted_rate" ~ "Relief granted"),
                                    color = case_when(var == "terminated_rate" ~ color_palette %>% slice(4) %>% pull(hex),
                                                      var == "removal_vol_dep_rate" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      var == "relief_granted_rate" ~ color_palette %>% slice(6) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////


# create title
title <- "I-589 EOIR outcomes for as a share of those adjudicated by EOIR,\nby I-589 FY filing cohort\n(for FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- c("Source: DOJ EOIR; USCIS OP&S")


#////////////////////////////////////////


# create eoir_outcome_rates_by_fy_cohort_chart
eoir_outcome_rates_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = fy, y = value, color = fct_reorder2(.f = color_bin, .x = fy, .y = value))) + 
        geom_line(size = 2) + geom_point(size = 4) +
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, expand = c(0, 0), limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share of\nadjudicated cases", 
             title = title,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2) +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 20, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 10, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 8, r = 0, b = -10, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
eoir_outcome_rates_by_fy_cohort_chart


#////////////////////////////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(eoir_outcome_rates_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/eoir_outcome_rates_by_fy_cohort_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# eoir_outcome_rates_by_fy_chart ####


# get referral_received_by_fy
referral_received_by_fy <- data %>% group_by(eoir_case_received_date_fy) %>% summarize(referral_received_count = sum(eoir_received_flag)) %>%
        rename(fy = eoir_case_received_date_fy)
referral_received_by_fy

# get new_pending_by_fy
new_pending_by_fy <- data %>% 
        mutate(new_pending_flag = case_when((eoir_case_received_date_fy != eoir_outcome_date_fy) | is.na(eoir_outcome_date_fy) ~ 1, TRUE ~ 0)) %>%
        group_by(eoir_case_received_date_fy) %>% count(new_pending_flag) %>% ungroup() %>% 
        filter(new_pending_flag == 1, eoir_case_received_date_fy >= 2009) %>%
        rename(fy = eoir_case_received_date_fy, new_pending_count = n) %>% select(-new_pending_flag)
new_pending_by_fy


# create chart_data
chart_data <- data %>% add_dummies(eoir_outcome) %>%
        group_by(eoir_outcome_date_fy) %>%
        summarize(adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure)) %>%
        filter(!is.na(eoir_outcome_date_fy)) %>%
        rename(fy = eoir_outcome_date_fy) %>%
        left_join(., referral_received_by_fy, by = "fy") %>%
        left_join(., new_pending_by_fy, by = "fy") %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               relief_granted_rate = relief_granted_count / adj_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               removal_vol_dep_rate = removal_vol_dep_count / adj_count,
               terminated_rate = terminated_count / adj_count) %>%
        select(fy, relief_granted_rate,
               removal_vol_dep_rate,
               terminated_rate) %>%
        pivot_longer(cols = -fy, names_to = "var", values_to = "value")

# inspect
chart_data


#///////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "relief_granted_rate" ~ "Relief granted",
                                                          var == "removal_vol_dep_rate" ~ "Removal / Voluntary departure",
                                                          var == "terminated_rate" ~ "Terminated"),
                                    color = case_when(var == "relief_granted_rate" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      var == "removal_vol_dep_rate" ~ color_palette %>% slice(6) %>% pull(hex),
                                                      var == "terminated_rate" ~ color_palette %>% slice(4) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "I-589 EOIR outcomes as a share of those adjudicated by EOIR, by FY\n(for FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- "Source: DOJ EOIR; USCIS OP&S"


#////////////////////////////////////


# create eoir_outcome_rates_by_fy_chart
eoir_outcome_rates_by_fy_chart <- chart_data %>%
        ggplot(data = ., aes(x = fy, y = value, color = fct_reorder2(.f = color_bin, .x = fy, .y = value))) + 
        geom_line(size = 2) + geom_point(size = 4) +
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2020, by = 2), limits = c(2008, 2021)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, expand = c(0, 0), limits = c(-.05, 1)) +
        labs(x = "FY", y = "Share of\n adjudicated cases", 
             title = title,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2) +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 20, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 10, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 8, r = 0, b = -10, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000", 
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
eoir_outcome_rates_by_fy_chart


#//////////////////////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(eoir_outcome_rates_by_fy_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/eoir_outcome_rates_by_fy_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# eoir_referred_adj_pending_by_fy_chart ####

# get referral_received_by_fy
referral_received_by_fy <- data %>% group_by(eoir_case_received_date_fy) %>% summarize(referral_received_count = sum(eoir_received_flag)) %>%
        rename(fy = eoir_case_received_date_fy)
referral_received_by_fy

# get new_pending_by_fy
new_pending_by_fy <- data %>% 
        mutate(new_pending_flag = case_when((eoir_case_received_date_fy != eoir_outcome_date_fy) | is.na(eoir_outcome_date_fy) ~ 1, TRUE ~ 0)) %>%
        group_by(eoir_case_received_date_fy) %>% count(new_pending_flag) %>% ungroup() %>% 
        filter(new_pending_flag == 1, eoir_case_received_date_fy >= 2009) %>%
        rename(fy = eoir_case_received_date_fy, new_pending_count = n) %>% select(-new_pending_flag)
new_pending_by_fy


# get chart_data
chart_data <- data %>% add_dummies(eoir_outcome) %>%
        group_by(eoir_outcome_date_fy) %>%
        summarize(adj_count = sum(eoir_adjudicated_flag),
                  relief_granted_count = sum(eoir_outcome.relief_granted),
                  removal_count = sum(eoir_outcome.removal),
                  admin_closed_count = sum(eoir_outcome.admin_closed),
                  cancellation_of_removal_count = sum(eoir_outcome.cancellation_of_removal),
                  terminated_count = sum(eoir_outcome.terminated),
                  voluntary_departure_count = sum(eoir_outcome.voluntary_departure),
                  other_count = sum(eoir_outcome.other)) %>%
        filter(!is.na(eoir_outcome_date_fy)) %>%
        rename(fy = eoir_outcome_date_fy) %>%
        left_join(., referral_received_by_fy, by = "fy") %>%
        left_join(., new_pending_by_fy, by = "fy") %>%
        mutate(relief_granted_count = relief_granted_count + cancellation_of_removal_count,
               relief_granted_rate = relief_granted_count / adj_count,
               removal_vol_dep_count = removal_count + voluntary_departure_count,
               removal_vol_dep_rate = removal_vol_dep_count / adj_count,
               terminated_rate = terminated_count / adj_count,
               admin_closed_other_count = admin_closed_count + other_count,
               cum_referral_received_count = cumsum(replace_na(referral_received_count, replace = "0")),
               cum_adj_count = cumsum(replace_na(adj_count, replace = "0")),
               cum_admin_closed_count = cumsum(replace_na(admin_closed_count, replace = "0")),
               cum_other_count = cumsum(replace_na(other_count, replace = "0")),
               cum_pending_count = cum_referral_received_count - cum_adj_count - cum_admin_closed_count - cum_other_count) %>%
        select(fy, referral_received_count, adj_count, new_pending_count, cum_pending_count) %>%
        pivot_longer(cols = -fy, names_to = "var", values_to = "value")

# inspect
chart_data


#///////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "referral_received_count" ~ "Referral received",
                                                          var == "adj_count" ~ "Adjudicated",
                                                          var == "new_pending_count" ~ "Added to pending",
                                                          var == "cum_pending_count" ~ "Cumulative pending"),
                                    color = case_when(var == "referral_received_count" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "adj_count" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      var == "new_pending_count" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "cum_pending_count" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "I-589 EOIR referrals received, adjudications, and pending cases, by FY\n(for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- "Source: DOJ EOIR; USCIS OP&S"


#////////////////////////////////////


# create eoir_referred_adj_pending_by_fy_chart
eoir_referred_adj_pending_by_fy_chart <- chart_data %>%
        ggplot(data = ., aes(x = fy, y = value, color = fct_reorder2(.f = color_bin, .x = fy, .y = value))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2020, by = 2), limits = c(2008, 2021)) +
        scale_y_continuous(labels = unit_format(unit = "", scale = .001)) +
        labs(x = "FY", y = "Case count (thousands)", 
             title = title,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/15000) +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 15, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 10, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 8, r = 0, b = -10, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 8, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
eoir_referred_adj_pending_by_fy_chart


#//////////////////////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(eoir_referred_adj_pending_by_fy_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/eoir_referred_adj_pending_by_fy_chart.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# ewi_cor_1yr_filing_shares_by_fy_cohort_chart ####

# create chart_data
chart_data <- data %>% add_dummies(vars(status_at_entry, outcome_bucket, eoir_outcome)) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), 
                ewi_count = sum(status_at_entry.EWI),
                referral_w_one_year_limit_count = sum(outcome_bucket.referral_w_one_year_limit),
                eoir_cancellation_applied_count = sum(eoir_cancellation_applied)) %>%
        ungroup() %>%
        mutate(ewi_pct = ewi_count / filed_count,
               referral_w_one_year_limit_pct = referral_w_one_year_limit_count / filed_count,
               eoir_cancellation_applied_pct = eoir_cancellation_applied_count / filed_count) %>% 
        select(filing_date_fy, ewi_pct, referral_w_one_year_limit_pct, eoir_cancellation_applied_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "ewi_pct" ~ "Entered without\ninspection",
                                                          var == "referral_w_one_year_limit_pct" ~ "Referred based on\n1-year filing deadline",
                                                          var == "eoir_cancellation_applied_pct" ~ "Applied for\ncancellation of removal"),
                                    color = case_when(var == "ewi_pct" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      var == "referral_w_one_year_limit_pct" ~ color_palette %>% slice(6) %>% pull(hex),
                                                      var == "eoir_cancellation_applied_pct" ~ color_palette %>% slice(4) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Share of I-589 filings from selected groups,\n by I-589 FY filing cohort (for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create ewi_cor_1yr_filing_shares_by_fy_cohort_chart
ewi_cor_1yr_filing_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, color = fct_reorder2(.f = color_bin, .x = filing_date_fy, .y = value))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share of I-589 filings", 
             title = title,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 10, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 8, r = 0, b = -10, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
ewi_cor_1yr_filing_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(ewi_cor_1yr_filing_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 5, height = 5) %>% 
        print(target = "output/ewi_cor_1yr_filing_shares_by_fy_cohort_chart.docx")



#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# ewi_cor_1yr_overall_relief_granted_shares_by_fy_cohort_chart ####

# create chart_data
chart_data <- data %>% add_dummies(vars(status_at_entry, outcome_bucket, eoir_outcome)) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), terminal_adj_count = sum(terminal_adjudicated_flag),
                  overall_relief_granted_count = sum(overall_relief_granted_flag),
                  ewi_adj_count = sum(status_at_entry.EWI[terminal_adjudicated_flag == 1]),
                  ewi_overall_relief_granted_count = sum(status_at_entry.EWI[overall_relief_granted_flag == 1]),
                  referral_w_one_year_limit_adj_count = sum(outcome_bucket.referral_w_one_year_limit[terminal_adjudicated_flag == 1]),
                  referral_w_one_year_limit_overall_relief_granted_count = sum(outcome_bucket.referral_w_one_year_limit[overall_relief_granted_flag == 1]),
                  eoir_cancellation_applied_adj_count = sum(eoir_cancellation_applied[terminal_adjudicated_flag == 1]),
                  eoir_cancellation_applied_overall_relief_granted_count = sum(eoir_cancellation_applied[overall_relief_granted_flag == 1])) %>%
        ungroup() %>%
        mutate(overall_relief_granted_pct = overall_relief_granted_count / terminal_adj_count,
               ewi_overall_relief_granted_pct = ewi_overall_relief_granted_count / ewi_adj_count,
               referral_w_one_year_limit_overall_relief_granted_pct = referral_w_one_year_limit_overall_relief_granted_count / 
                       referral_w_one_year_limit_adj_count,
               eoir_cancellation_applied_overall_relief_granted_pct = eoir_cancellation_applied_overall_relief_granted_count / 
                       eoir_cancellation_applied_adj_count) %>% 
        select(filing_date_fy, overall_relief_granted_pct, ewi_overall_relief_granted_pct, 
               referral_w_one_year_limit_overall_relief_granted_pct, eoir_cancellation_applied_overall_relief_granted_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "overall_relief_granted_pct" ~ "All I-589\nfilings",
                                                          var == "ewi_overall_relief_granted_pct" ~ "Entered without\ninspection",
                                                          var == "referral_w_one_year_limit_overall_relief_granted_pct" ~ "Referred based on\n1-year filing deadline",
                                                          var == "eoir_cancellation_applied_overall_relief_granted_pct" ~ "Applied for\ncancellation of removal"),
                                    color = case_when(var == "overall_relief_granted_pct" ~ "#B2B2B2",
                                                      var == "ewi_overall_relief_granted_pct" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "referral_w_one_year_limit_overall_relief_granted_pct" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "eoir_cancellation_applied_overall_relief_granted_pct" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Overall relief granted share (from RAIO or EOIR) within selected groups,\nby I-589 FY filing cohort (for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create ewi_cor_1yr_overall_relief_granted_shares_by_fy_cohort_chart
ewi_cor_1yr_overall_relief_granted_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, color = fct_reorder2(.f = color_bin, .x = filing_date_fy, .y = value))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Overall relief granted share", 
             title = title,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 20, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 10, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 8, r = 0, b = -10, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
ewi_cor_1yr_overall_relief_granted_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(ewi_cor_1yr_overall_relief_granted_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 5, height = 5) %>% 
        print(target = "output/ewi_cor_1yr_overall_relief_granted_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# income_group_filing_shares_by_fy_cohort_chart ####

# create chart_data
chart_data <- data %>% add_dummies(income_group_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), 
                  low_income_count = sum(`income_group_bucket.Low income`),
                  lower_middle_income_count = sum(`income_group_bucket.Lower middle income`),
                  upper_middle_income_count = sum(`income_group_bucket.Upper middle income`),
                  high_income_count = sum(`income_group_bucket.High income`)) %>%
        ungroup() %>%
        mutate(low_income_pct = low_income_count / filed_count,
               lower_middle_income_pct = lower_middle_income_count / filed_count,
               upper_middle_income_pct = upper_middle_income_count / filed_count,
               high_income_pct = high_income_count / filed_count) %>% 
        select(filing_date_fy, low_income_pct, lower_middle_income_pct, upper_middle_income_pct, high_income_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "low_income_pct" ~ "Low\nincome",
                                                          var == "lower_middle_income_pct" ~ "Lower middle\nincome",
                                                          var == "upper_middle_income_pct" ~ "Upper middle\nincome",
                                                          var == "high_income_pct" ~ "High\nincome"),
                                    color = case_when(var == "low_income_pct" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      var == "lower_middle_income_pct" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "upper_middle_income_pct" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "high_income_pct" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Share of I-589 filings from country income groupings,\n by I-589 FY filing cohort (for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- str_c(add_linebreaks("Note: Shares depicted do not sum to 100% because income data was unavailable for some countries in some years.", nchar_limit = 100),
                   "\n",
                   "Source: World Bank; USCIS RAIO; USCIS OP&S",
                   sep = "")


#////////////////////////////////////


# create income_group_filing_shares_by_fy_cohort_chart
income_group_filing_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("Low\nincome", "Lower middle\nincome",
                                                                  "Upper middle\nincome", "High\nincome")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share of I-589 filings", 
             title = title,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 15, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 10, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 8, r = 0, b = -10, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
income_group_filing_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(income_group_filing_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 5, height = 5) %>% 
        print(target = "output/income_group_filing_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# income_group_overall_relief_granted_shares_by_fy_cohort_chart ####

# create chart_data
chart_data <- data %>% add_dummies(income_group_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), terminal_adj_count = sum(terminal_adjudicated_flag),
                  overall_relief_granted_count = sum(overall_relief_granted_flag),
                  low_income_adj_count = sum(`income_group_bucket.Low income`[terminal_adjudicated_flag == 1]),
                  low_income_overall_relief_granted_count = sum(`income_group_bucket.Low income`[overall_relief_granted_flag == 1]),
                  lower_middle_income_adj_count = sum(`income_group_bucket.Lower middle income`[terminal_adjudicated_flag == 1]),
                  lower_middle_income_overall_relief_granted_count = sum(`income_group_bucket.Lower middle income`[overall_relief_granted_flag == 1]),
                  upper_middle_income_adj_count = sum(`income_group_bucket.Upper middle income`[terminal_adjudicated_flag == 1]),
                  upper_middle_income_overall_relief_granted_count = sum(`income_group_bucket.Upper middle income`[overall_relief_granted_flag == 1]),
                  high_income_adj_count = sum(`income_group_bucket.High income`[terminal_adjudicated_flag == 1]),
                  high_income_overall_relief_granted_count = sum(`income_group_bucket.High income`[overall_relief_granted_flag == 1])) %>%
        ungroup() %>%
        mutate(overall_relief_granted_pct = overall_relief_granted_count / terminal_adj_count,
               low_income_overall_relief_granted_pct = low_income_overall_relief_granted_count / low_income_adj_count,
               lower_middle_income_overall_relief_granted_pct = lower_middle_income_overall_relief_granted_count / 
                       lower_middle_income_adj_count,
               upper_middle_income_overall_relief_granted_pct = upper_middle_income_overall_relief_granted_count / 
                       upper_middle_income_adj_count,
               high_income_overall_relief_granted_pct = high_income_overall_relief_granted_count / 
                       high_income_adj_count) %>% 
        select(filing_date_fy, overall_relief_granted_pct, low_income_overall_relief_granted_pct, 
               lower_middle_income_overall_relief_granted_pct, upper_middle_income_overall_relief_granted_pct,
               high_income_overall_relief_granted_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "overall_relief_granted_pct" ~ "All I-589\nfilings",
                                                          var == "low_income_overall_relief_granted_pct" ~ "Low\nincome",
                                                          var == "lower_middle_income_overall_relief_granted_pct" ~ "Lower middle\nincome",
                                                          var == "upper_middle_income_overall_relief_granted_pct" ~ "Upper middle\nincome",
                                                          var == "high_income_overall_relief_granted_pct" ~ "High\nincome"),
                                    color = case_when(var == "overall_relief_granted_pct" ~ "#B2B2B2",
                                                      var == "low_income_overall_relief_granted_pct" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      var == "lower_middle_income_overall_relief_granted_pct" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "upper_middle_income_overall_relief_granted_pct" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "high_income_overall_relief_granted_pct" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Overall relief granted share (from RAIO or EOIR)\nwithin country income groupings,\nby I-589 FY filing cohort (for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- "Source: World Bank; DOJ EOIR; USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create income_group_overall_relief_granted_shares_by_fy_cohort_chart
income_group_overall_relief_granted_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("Low\nincome", "Lower middle\nincome",
                                                                  "Upper middle\nincome", "High\nincome", "All I-589\nfilings")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Overall relief granted share", 
             title = title,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 30, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 10, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 8, r = 0, b = -10, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
income_group_overall_relief_granted_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(income_group_overall_relief_granted_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 5, height = 5) %>% 
        print(target = "output/income_group_overall_relief_granted_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# homicide_bucket_filing_shares_by_fy_cohort_chart ####

# create chart_data
chart_data <- data %>% add_dummies(homicide_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), 
                  low_homicide_count = sum(`homicide_bucket.= 1`),
                  lower_middle_homicide_count = sum(`homicide_bucket.1.1 to 10`),
                  middle_homicide_count = sum(`homicide_bucket.10.1 to 20`),
                  upper_middle_homicide_count = sum(`homicide_bucket.20.1 to 40`),
                  high_homicide_count = sum(`homicide_bucket.> 40`)) %>%
        ungroup() %>%
        mutate(low_homicide_pct = low_homicide_count / filed_count,
               lower_middle_homicide_pct = lower_middle_homicide_count / filed_count,
               middle_homicide_pct = middle_homicide_count / filed_count,
               upper_middle_homicide_pct = upper_middle_homicide_count / filed_count,
               high_homicide_pct = high_homicide_count / filed_count) %>% 
        select(filing_date_fy, low_homicide_pct, lower_middle_homicide_pct, middle_homicide_pct, 
               upper_middle_homicide_pct, high_homicide_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "low_homicide_pct" ~ "\u2264 1",
                                                          var == "lower_middle_homicide_pct" ~ "1.1 to 10",
                                                          var == "middle_homicide_pct" ~ "10.1 to 20",
                                                          var == "upper_middle_homicide_pct" ~ "20.1 to 40",
                                                          var == "high_homicide_pct" ~ "> 40"),
                                    color = case_when(var == "low_homicide_pct" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      var == "lower_middle_homicide_pct" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "middle_homicide_pct" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "upper_middle_homicide_pct" ~ color_palette %>% slice(8) %>% pull(hex),
                                                      var == "high_homicide_pct" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Share of I-589 filings from country homicide groupings,\n by I-589 FY filing cohort (for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- str_c(add_linebreaks("Note: Shares depicted do not sum to 100% because homicide data was unavailable for some countries in some years.", nchar_limit = 100),
                   "\n",
                   "Source: United Nations; USCIS RAIO; USCIS OP&S",
                   sep = "")

#////////////////////////////////////


# create homicide_bucket_filing_shares_by_fy_cohort_chart
homicide_bucket_filing_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("\u2264 1", "1.1 to 10", "10.1 to 20", 
                                                                  "20.1 to 40", "> 40")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share of I-589 filings", 
             title = title,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 10, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 8, r = 0, b = -10, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
homicide_bucket_filing_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(homicide_bucket_filing_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 5, height = 5) %>% 
        print(target = "output/homicide_bucket_filing_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# homicide_bucket_overall_relief_granted_shares_by_fy_cohort_chart ####

# create chart_data
chart_data <- data %>% add_dummies(homicide_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), terminal_adj_count = sum(terminal_adjudicated_flag),
                  overall_relief_granted_count = sum(overall_relief_granted_flag),
                  low_homicide_adj_count = sum(`homicide_bucket.= 1`[terminal_adjudicated_flag == 1]),
                  low_homicide_overall_relief_granted_count = sum(`homicide_bucket.= 1`[overall_relief_granted_flag == 1]),
                  lower_middle_homicide_adj_count = sum(`homicide_bucket.1.1 to 10`[terminal_adjudicated_flag == 1]),
                  lower_middle_homicide_overall_relief_granted_count = sum(`homicide_bucket.1.1 to 10`[overall_relief_granted_flag == 1]),
                  middle_homicide_adj_count = sum(`homicide_bucket.10.1 to 20`[terminal_adjudicated_flag == 1]),
                  middle_homicide_overall_relief_granted_count = sum(`homicide_bucket.10.1 to 20`[overall_relief_granted_flag == 1]),
                  upper_middle_homicide_adj_count = sum(`homicide_bucket.20.1 to 40`[terminal_adjudicated_flag == 1]),
                  upper_middle_homicide_overall_relief_granted_count = sum(`homicide_bucket.20.1 to 40`[overall_relief_granted_flag == 1]),
                  high_homicide_adj_count = sum(`homicide_bucket.> 40`[terminal_adjudicated_flag == 1]),
                  high_homicide_overall_relief_granted_count = sum(`homicide_bucket.> 40`[overall_relief_granted_flag == 1])) %>%
        ungroup() %>%
        mutate(overall_relief_granted_pct = overall_relief_granted_count / terminal_adj_count,
               low_homicide_overall_relief_granted_pct = low_homicide_overall_relief_granted_count / low_homicide_adj_count,
               lower_middle_homicide_overall_relief_granted_pct = lower_middle_homicide_overall_relief_granted_count / 
                       lower_middle_homicide_adj_count,
               middle_homicide_overall_relief_granted_pct = middle_homicide_overall_relief_granted_count / 
                       middle_homicide_adj_count,
               upper_middle_homicide_overall_relief_granted_pct = upper_middle_homicide_overall_relief_granted_count / 
                       upper_middle_homicide_adj_count,
               high_homicide_overall_relief_granted_pct = high_homicide_overall_relief_granted_count / 
                       high_homicide_adj_count) %>% 
        select(filing_date_fy, overall_relief_granted_pct, low_homicide_overall_relief_granted_pct, 
               lower_middle_homicide_overall_relief_granted_pct, middle_homicide_overall_relief_granted_pct, 
               upper_middle_homicide_overall_relief_granted_pct, high_homicide_overall_relief_granted_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "low_homicide_overall_relief_granted_pct" ~ "\u2264 1",
                                                          var == "lower_middle_homicide_overall_relief_granted_pct" ~ "1.1 to 10",
                                                          var == "middle_homicide_overall_relief_granted_pct" ~ "10.1 to 20",
                                                          var == "upper_middle_homicide_overall_relief_granted_pct" ~ "20.1 to 40",
                                                          var == "high_homicide_overall_relief_granted_pct" ~ "> 40",
                                                          var == "overall_relief_granted_pct" ~ "All I-589 filings"),
                                    color = case_when(var == "low_homicide_overall_relief_granted_pct" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      var == "lower_middle_homicide_overall_relief_granted_pct" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "middle_homicide_overall_relief_granted_pct" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "upper_middle_homicide_overall_relief_granted_pct" ~ color_palette %>% slice(8) %>% pull(hex),
                                                      var == "high_homicide_overall_relief_granted_pct" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      var == "overall_relief_granted_pct" ~ "#B2B2B2"))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Overall relief granted share (from RAIO or EOIR)\nwithin country homicide groupings,\nby I-589 FY filing cohort (for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- "Source: United Nations; DOJ EOIR; USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create homicide_bucket_overall_relief_granted_shares_by_fy_cohort_chart
homicide_bucket_overall_relief_granted_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("\u2264 1", "1.1 to 10", "10.1 to 20", 
                                                                  "20.1 to 40", "> 40", "All I-589 filings")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Overall relief granted share", 
             title = title,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 30, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 10, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 8, r = 0, b = -10, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
homicide_bucket_overall_relief_granted_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(homicide_bucket_overall_relief_granted_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 5, height = 5) %>% 
        print(target = "output/homicide_bucket_overall_relief_granted_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# prcl_bucket_filing_shares_by_fy_cohort_chart ####

# create chart_data
chart_data <- data %>% add_dummies(prcl_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), 
                  low_prcl_count = sum(`prcl_bucket.< 25`),
                  lower_middle_prcl_count = sum(`prcl_bucket.25 to 49`),
                  upper_middle_prcl_count = sum(`prcl_bucket.50 to 74`),
                  high_prcl_count = sum(`prcl_bucket.75 to 100`)) %>%
        ungroup() %>%
        mutate(low_prcl_pct = low_prcl_count / filed_count,
               lower_middle_prcl_pct = lower_middle_prcl_count / filed_count,
               upper_middle_prcl_pct = upper_middle_prcl_count / filed_count,
               high_prcl_pct = high_prcl_count / filed_count) %>% 
        select(filing_date_fy, low_prcl_pct, lower_middle_prcl_pct, 
               upper_middle_prcl_pct, high_prcl_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "low_prcl_pct" ~ "< 25",
                                                          var == "lower_middle_prcl_pct" ~ "25 to 49",
                                                          var == "upper_middle_prcl_pct" ~ "50 to 74",
                                                          var == "high_prcl_pct" ~ "75 to 100"),
                                    color = case_when(var == "low_prcl_pct" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      var == "lower_middle_prcl_pct" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "upper_middle_prcl_pct" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "high_prcl_pct" ~ color_palette %>% slice(9) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Share of I-589 filings from country political rights / civil liberties groupings,\n by I-589 FY filing cohort (for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- str_c(add_linebreaks("Note: Shares depicted do not sum to 100% because political rights / civil liberties data was unavailable for some countries in some years.", nchar_limit = 110),
                   "\n",
                "Source: Freedom House; USCIS RAIO; USCIS OP&S",
                sep = "")


#////////////////////////////////////


# create prcl_bucket_filing_shares_by_fy_cohort_chart
prcl_bucket_filing_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("< 25", "25 to 49", "50 to 74", 
                                                                  "75 to 100")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Share of I-589 filings", 
             title = title,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 10, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 8, r = 0, b = -10, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
prcl_bucket_filing_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(prcl_bucket_filing_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 5, height = 5) %>% 
        print(target = "output/prcl_bucket_filing_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# prcl_bucket_overall_relief_granted_shares_by_fy_cohort_chart ####

# create chart_data
chart_data <- data %>% add_dummies(prcl_bucket) %>%
        group_by(filing_date_fy) %>%
        summarize(filed_count = n(), terminal_adj_count = sum(terminal_adjudicated_flag),
                  overall_relief_granted_count = sum(overall_relief_granted_flag),
                  low_prcl_adj_count = sum(`prcl_bucket.< 25`[terminal_adjudicated_flag == 1]),
                  low_prcl_overall_relief_granted_count = sum(`prcl_bucket.< 25`[overall_relief_granted_flag == 1]),
                  lower_middle_prcl_adj_count = sum(`prcl_bucket.25 to 49`[terminal_adjudicated_flag == 1]),
                  lower_middle_prcl_overall_relief_granted_count = sum(`prcl_bucket.25 to 49`[overall_relief_granted_flag == 1]),
                  upper_middle_prcl_adj_count = sum(`prcl_bucket.50 to 74`[terminal_adjudicated_flag == 1]),
                  upper_middle_prcl_overall_relief_granted_count = sum(`prcl_bucket.50 to 74`[overall_relief_granted_flag == 1]),
                  high_prcl_adj_count = sum(`prcl_bucket.75 to 100`[terminal_adjudicated_flag == 1]),
                  high_prcl_overall_relief_granted_count = sum(`prcl_bucket.75 to 100`[overall_relief_granted_flag == 1])) %>%
        ungroup() %>%
        mutate(overall_relief_granted_pct = overall_relief_granted_count / terminal_adj_count,
               low_prcl_overall_relief_granted_pct = low_prcl_overall_relief_granted_count / low_prcl_adj_count,
               lower_middle_prcl_overall_relief_granted_pct = lower_middle_prcl_overall_relief_granted_count / 
                       lower_middle_prcl_adj_count,
               upper_middle_prcl_overall_relief_granted_pct = upper_middle_prcl_overall_relief_granted_count / 
                       upper_middle_prcl_adj_count,
               high_prcl_overall_relief_granted_pct = high_prcl_overall_relief_granted_count / 
                       high_prcl_adj_count) %>% 
        select(filing_date_fy, overall_relief_granted_pct, low_prcl_overall_relief_granted_pct, 
               lower_middle_prcl_overall_relief_granted_pct, 
               upper_middle_prcl_overall_relief_granted_pct, high_prcl_overall_relief_granted_pct) %>%
        pivot_longer(cols = -filing_date_fy, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "low_prcl_overall_relief_granted_pct" ~ "< 25",
                                                          var == "lower_middle_prcl_overall_relief_granted_pct" ~ "25 to 49",
                                                          var == "upper_middle_prcl_overall_relief_granted_pct" ~ "50 to 74",
                                                          var == "high_prcl_overall_relief_granted_pct" ~ "75 to 100",
                                                          var == "overall_relief_granted_pct" ~ "All I-589 filings"),
                                    color = case_when(var == "low_prcl_overall_relief_granted_pct" ~ color_palette %>% slice(3) %>% pull(hex),
                                                      var == "lower_middle_prcl_overall_relief_granted_pct" ~ color_palette %>% slice(5) %>% pull(hex),
                                                      var == "upper_middle_prcl_overall_relief_granted_pct" ~ color_palette %>% slice(7) %>% pull(hex),
                                                      var == "high_prcl_overall_relief_granted_pct" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      var == "overall_relief_granted_pct" ~ "#B2B2B2"))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Overall relief granted share (from RAIO or EOIR)\nwithin country political rights / civil liberties groupings,\nby I-589 FY filing cohort (for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- str_c(add_linebreaks("Note: Shares depicted do not sum to 100% because political rights / civil liberties data was unavailable for some countries in some years.", nchar_limit = 110),
                   "\n",
                   "Source: Freedom House; USCIS RAIO; USCIS OP&S",
                   sep = "")


#////////////////////////////////////


# create prcl_bucket_overall_relief_granted_shares_by_fy_cohort_chart
prcl_bucket_overall_relief_granted_shares_by_fy_cohort_chart <- chart_data %>%
        ggplot(data = ., aes(x = filing_date_fy, y = value, 
                             color = factor(color_bin, levels = c("< 25", "25 to 49", "50 to 74", 
                                                                  "75 to 100", "All I-589 filings")))) + 
        geom_line(size = 2) + geom_point(size = 4) + 
        scale_color_manual(values = chart_data_color_list) +
        scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(-.05, 1)) +
        labs(x = "I-589 FY filing cohort", y = "Overall relief granted share", 
             title = title,
             caption = footnotes, color = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 30, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 10, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 8, r = 0, b = -10, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "bottom",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
prcl_bucket_overall_relief_granted_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(prcl_bucket_overall_relief_granted_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 5, height = 5) %>% 
        print(target = "output/prcl_bucket_overall_relief_granted_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# country_filing_shares_by_fy_cohort_chart ####

# get total_filings
total_filings <- data %>% nrow()
total_filings

# get chart_data
chart_data <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(citizenship_output_country_name) %>%
        summarize(filing_count = n()) %>%
        ungroup() %>%
        mutate(filing_share = filing_count / total_filings) %>%
        select(citizenship_output_country_name, filing_share) %>%
        arrange(desc(filing_share)) %>%
        mutate(cum_filing_share = cumsum(filing_share)) %>%
        pivot_longer(cols = -citizenship_output_country_name, names_to = "var", values_to = "value")

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = case_when(var == "filing_share" ~ "Share of I-589 filings",
                                                          var == "cum_filing_share" ~ "Cumulative share of I-589 filings"),
                                    color = case_when(var == "filing_share" ~ color_palette %>% slice(9) %>% pull(hex),
                                                      var == "cum_filing_share" ~ color_palette %>% slice(5) %>% pull(hex)))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Share of I-589 filings for top I-589 filing countries\n(for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create country_filing_shares_by_fy_cohort_chart
country_filing_shares_by_fy_cohort_chart <- chart_data %>%
        mutate(citizenship_output_country_name = factor(citizenship_output_country_name, 
                                levels = chart_data %>% filter(var == "filing_share") %>% arrange(value) %>% pull(citizenship_output_country_name))) %>%
        ggplot(data = ., aes(x = citizenship_output_country_name, y = value, fill = color_bin)) + 
        geom_col(width = .6) +
        scale_fill_manual(values = chart_data_color_list) +
        # scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(0, 1), expand = c(0, 0)) +
        labs(x = "Top I-589 filing countries", y = "Share of I-589 filings", 
             title = title,
             caption = footnotes, fill = "") +
        coord_fixed(ratio = 1/.2, clip = "off") +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 10, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0),
                                           angle = 45, vjust = 1.2, hjust = 1.2),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "none",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
country_filing_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(country_filing_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 5, height = 5) %>% 
        print(target = "output/country_filing_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# country_overall_relief_granted_shares_by_fy_cohort_chart ####

# get chart_data
chart_data <- data %>% count(citizenship_output_country_name) %>% arrange(desc(n)) %>% slice(1:10) %>%
        distinct(citizenship_output_country_name) %>% left_join(., data, by = "citizenship_output_country_name") %>%
        group_by(citizenship_output_country_name) %>%
        summarize(terminal_adj_count = sum(terminal_adjudicated_flag),
                  overall_relief_granted_count = sum(overall_relief_granted_flag)) %>%
        ungroup() %>%
        mutate(overall_relief_granted_pct = overall_relief_granted_count / terminal_adj_count) %>%
        select(citizenship_output_country_name, overall_relief_granted_pct) %>%
        arrange(desc(overall_relief_granted_pct)) %>%
        rename(value = overall_relief_granted_pct)

# inspect
chart_data


#//////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add color_bin and color
chart_data <- chart_data %>% mutate(color_bin = "Overall relief granted share",
                                    color = color_palette %>% slice(7) %>% pull(hex))

# create color_list for to pass to scale_color_manual
chart_data_color_list <- chart_data %>% count(color_bin, color) %>% pull(color)
names(chart_data_color_list) <- chart_data %>% count(color_bin, color) %>% pull(color_bin)
chart_data_color_list


#///////////////////////////////////////


# create title
title <- "Overall relief granted share for top I-589 filing countries\n(for the FY 2009-2018 filing cohorts)"

# create footnotes
footnotes <- "Source: DOJ EOIR; USCIS RAIO; USCIS OP&S"


#////////////////////////////////////


# create country_overall_relief_granted_shares_by_fy_cohort_chart
country_overall_relief_granted_shares_by_fy_cohort_chart <- chart_data %>%
        mutate(citizenship_output_country_name = factor(citizenship_output_country_name, 
                                                        levels = chart_data %>% arrange(value) %>% pull(citizenship_output_country_name))) %>%
        ggplot(data = ., aes(x = citizenship_output_country_name, y = value, fill = color_bin)) + 
        geom_col(width = .6) +
        scale_fill_manual(values = chart_data_color_list) +
        # scale_x_continuous(breaks = seq(from = 2009, to = 2018, by = 2), limits = c(2008, 2019)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), labels = percent, limits = c(0, 1), expand = c(0, 0)) +
        labs(x = "Top I-589 filing countries", y = "Overall relief granted share", 
             title = title,
             caption = footnotes, fill = "") +
        # coord_fixed(ratio = 1/.2, clip = "off") +
        coord_flip() +
        theme_bw() +
        theme(
                # plot.background = element_rect(fill = "blue"),
                plot.margin = unit(c(0, 10, 0, 0), "mm"),
                plot.caption = element_text(hjust = 0, size = 10, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 4, r = 0, b = 0, l = 0)),
                # text = element_text(family = "Calibri", size = 46, face = "plain", color = "#000000"),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_line(color = "#DDDDDD"),
                panel.grid.major.y = element_blank(),
                # panel.grid.major.y = element_line(color = "#000000"),
                panel.border = element_blank(),
                # panel.grid = element_blank(),
                # line = element_blank(),
                # rect = element_blank(),
                # axis.ticks.y = element_blank(),
                # axis.ticks.x = element_blank(),
                axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.2, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 5, r = 0, b = 0, l = 0)),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#000000"),
                axis.line.y.left = element_line(color = "#000000"),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 8, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 14, color = "#000000", margin = margin(t = 0, r = 5, b = 0, l = 0)),
                plot.title = element_text(size = 16, face = "bold", hjust = .5, family = "Calibri", color = "#000000",
                                          margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
                legend.position = "none",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 14, family = "Calibri", face = "plain"),
                legend.text = element_text(size = 14, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.text.align = 0
                # legend.spacing.y = unit(5.5, "cm")
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

# inspect
country_overall_relief_granted_shares_by_fy_cohort_chart 


#//////////////////////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(country_overall_relief_granted_shares_by_fy_cohort_chart)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
read_docx() %>% 
        body_add_img(src = filename, width = 5, height = 5) %>% 
        print(target = "output/country_overall_relief_granted_shares_by_fy_cohort_chart.docx")


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

