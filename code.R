### load packages
library(tidyverse)
library(gt)
library(paletteer)
library(Lahman)
library(webshot)

### generate dataset for visual example
top10_by_hits <- Batting %>% 
      filter(yearID == 2016) %>%
      arrange(desc(H)) %>%    # arrange by descending HR count
      slice(1:10) %>%
      left_join(Salaries %>% select(playerID, yearID, salary), 
                by=c("playerID", "yearID")) %>%
      left_join(Master %>% select(playerID, nameFirst, nameLast, height),
                by=c("playerID")) %>%
      mutate(BA = round(H/AB,3),
             salary_mil = round(salary/1000000,2),
             height = paste0(floor(height/12), " ft ", 
                                  height - floor(height/12)*12, " in"),
             name = paste0(nameFirst, " ", nameLast)) %>%
      select(name, height, salary_mil, AB, BA, H, HR, X2B, 
             X3B, SB, BB, SO, RBI, R)

columns_vars <- vars(salary_mil, AB, BA, H, HR, X2B, 
                     X3B, SB, BB, SO, RBI, R)

### generate conditional formatting table
top10_by_hits %>%
      gt() %>%
      data_color(
            columns = columns_vars,
            colors = scales::col_numeric(
                  palette = paletteer::paletteer_d(
                        palette = "RColorBrewer::Greens"
                  ) %>% as.character(),
                  domain = NULL
            )
      ) %>%
      tab_header(
            title = "Top 10 Players by 2016 Hits Count"
      ) %>%
      tab_spanner(
            label = "2016 Season Stats",
            columns = columns_vars
      ) %>%
      tab_options(
            table.font.size = 14,
            table.align="center"
      ) %>%
      cols_align(
            align = "center",
            columns = columns_vars
      ) %>%
gtsave(filename="top10_by_hits.png", vwidth = 650, vheight = 450)
