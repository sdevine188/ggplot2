# # load run_loop()
# current_wd <- getwd()
# setwd("H:/R/assorted_helper_scripts")
# source("get_label_position.R")
# setwd(current_wd)

# create get_label_position() for use in facet charts ####

get_label_position <- function(chart_data, shift = 0, gap = .04) {
        
        # get initial label_position
        chart_data <- chart_data %>% arrange(year, values) %>% 
                group_by(year) %>%
                mutate(label_position = round(values, digits = 3),
                       dist_to_lower = round(label_position - lag(label_position, n = 1), digits = 3),
                       dist_to_higher = round(lead(label_position, n = 1) - label_position, digits = 3),
                       overlap_flag = case_when(dist_to_lower < gap ~ 1,
                                                TRUE ~ 0)) %>%
                ungroup()
        
        # set counter
        counter <- 0
        
        # run loop to update label_position
        while((chart_data %>% mutate(sum_overlap_flag = sum(overlap_flag)) %>% slice(1) %>% pull(sum_overlap_flag)) != 0) {
                
                counter <- counter + 1
                print(counter + 1)
                
                chart_data <- chart_data %>% arrange(year, values) %>% 
                        group_by(year) %>%
                        mutate(label_position = case_when(is.na(dist_to_lower) | 
                                                                  dist_to_lower >= gap ~ round(label_position, digits = 3),
                                                          TRUE ~ round(label_position + (gap - dist_to_lower), digits = 3)),
                               dist_to_higher = round(lead(label_position, n = 1) - label_position, digits = 3),
                               dist_to_lower = round(label_position - lag(label_position, n = 1), digits = 3),
                               overlap_flag = case_when(dist_to_lower < gap ~ 1,
                                                        TRUE ~ 0)) %>%
                        ungroup() %>%
                        arrange(desc(year), values)
        }
        
        # apply shift
        chart_data <- chart_data %>% mutate(label_position = label_position + shift)
        
        # return
        return(chart_data)
}


#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////


# # example
# set.seed(123)
# tibble(country = rep(c("U.S.", "France", "U.K.", "Germany", "Australia", "Japan"), times = 2), 
#        year = c(rep(2010, times = 6), rep(2020, times = 6)),
#        values = sample(seq(from = .40, to = .60, by = .01), size = 12)) %>%
#         arrange(country) %>%
#         get_label_position()
# 
# # specify shift arg
# set.seed(123)
# tibble(country = rep(c("U.S.", "France", "U.K.", "Germany", "Australia", "Japan"), times = 2), 
#        year = c(rep(2010, times = 6), rep(2020, times = 6)),
#        values = sample(seq(from = .40, to = .60, by = .01), size = 12)) %>%
#         arrange(country) %>%
#         get_label_position(shift = .10)
# 
# # specify shift arg
# set.seed(123)
# tibble(country = rep(c("U.S.", "France", "U.K.", "Germany", "Australia", "Japan"), times = 2), 
#        year = c(rep(2010, times = 6), rep(2020, times = 6)),
#        values = sample(seq(from = .40, to = .60, by = .01), size = 12)) %>%
#         arrange(country) %>%
#         get_label_position(shift = -.10)
# 
# # specify gap arg
# set.seed(123)
# tibble(country = rep(c("U.S.", "France", "U.K.", "Germany", "Australia", "Japan"), times = 2), 
#        year = c(rep(2010, times = 6), rep(2020, times = 6)),
#        values = sample(seq(from = .40, to = .60, by = .01), size = 12)) %>%
#         arrange(country) %>%
#         get_label_position(gap = .02)
# 
# # specify shift and gap arg
# set.seed(123)
# tibble(country = rep(c("U.S.", "France", "U.K.", "Germany", "Australia", "Japan"), times = 2), 
#        year = c(rep(2010, times = 6), rep(2020, times = 6)),
#        values = sample(seq(from = .40, to = .60, by = .01), size = 12)) %>%
#         arrange(country) %>%
#         get_label_position(shift = .10, gap = .02)
