library(tidyverse)

# create add_linebreaks() for use in ggplot2 footnotes
add_linebreaks <- function(string, nchar_limit = 85) {
        
        # get count_of_required_linebreaks 
        count_of_required_linebreaks <- floor(nchar(string) / nchar_limit)
        
        # return string if no linebreaks are needed
        if(count_of_required_linebreaks < 1) {
                return(string)
        }
        
        # get space_positions
        space_positions <- str_locate_all(string = string, pattern = " ")[[1]] %>% as_tibble() %>% select(start)
        
        # get ideal_linebreak_positions
        ideal_linebreak_positions <- seq(from = nchar_limit + 1, to = nchar(string), by = nchar_limit)
        
        # get practical_linebreak_positions
        practical_linebreak_positions <- map_dfr(.x = ideal_linebreak_positions, 
                                                 .f = ~ space_positions %>% mutate(distance_from_ideal_linebreak = .x - start) %>%
                                                         filter(distance_from_ideal_linebreak > 0) %>%
                                                         arrange(distance_from_ideal_linebreak) %>% slice(1)) %>%
                mutate(row_number = row_number())
        
        # get breaks_tbl
        breaks_tbl <- tibble(practical_linebreaks = practical_linebreak_positions %>% pull(start)) %>%
                mutate(row_number = row_number(), previous_row_number = row_number - 1) %>%
                left_join(., practical_linebreak_positions, by = c("previous_row_number" = "row_number")) %>%
                mutate(practical_linebreaks = as.numeric(practical_linebreaks),
                       str_sub_start_position = case_when(row_number == 1 ~ 1, TRUE ~ as.numeric(start)),
                       str_sub_end_position = practical_linebreaks) %>%
                select(practical_linebreaks, str_sub_start_position, str_sub_end_position)
        
        # add newlines at practical_linebreak_positions
        for(i in 1:nrow(breaks_tbl)) {
                
                # get pre_text
                pre_text <- str_sub(string = string, start = 1, 
                                    end = breaks_tbl %>% slice(i) %>% pull(str_sub_end_position)) 
                
                # get post_text
                post_text <- str_sub(string = string, start = breaks_tbl %>% slice(i) %>% pull(str_sub_end_position) + 1, 
                                     end = nchar(string)) 
                
                # update string with newline between pre/post_text
                string <- str_c(pre_text, "\n", post_text)
                
                # update breaks_tbl to account for additional new_line character
                breaks_tbl <- breaks_tbl %>% mutate(str_sub_start_position = str_sub_start_position + 1,
                                                    str_sub_end_position = str_sub_end_position + 1)
        }
        
        return(string)
        
}


###########################


# # test
# string <- "does is it captures a snapshot of the plot, so that the plot effectively turns into an image, and then it draws that image into a new ggplot2 canvas without visible axes or background grid"
# 
# add_linebreaks(string = string)
# add_linebreaks(string = string, nchar_limit = 30)
# add_linebreaks(string = "test")


