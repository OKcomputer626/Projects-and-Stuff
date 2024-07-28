library(tidyverse)
library(gt)
library(gtExtras)
library(sysfonts)

font_add("Font Awesome 6 Brands", "fonts/otfs/Font Awesome 6 Brands-Regular-400.otf")

df <- read_csv("Bayesian Leagues Cup/data/leagues cup predictions.csv")


df <- df %>%
  select(-n) %>%
  filter(Rank %in% c(1,2)) %>%
  pivot_wider(names_from = Rank, values_from = freq) %>%
  rename(Rank_1 = `1`,
         Rank_2 = `2`)

# Iterate over team pairs and calculate probabilities
for (i in length(df$Team)) {
  Team <- df$Team[i]
  
  # Append the results row by row
  results <- df %>%
    tibble(
      logo = paste0("C:/Users/gonza/OneDrive - csulb/Documents/Projects and Stuff/Bayesian Leagues Cup/images/logos/", Team, ".png"),
    )
}


table1 <- results %>%
  filter(Region %in% c("East 1", "East 2", "East 3", "East 4", "East 5", "East 6", "East 7")) %>%
  arrange(Region, desc(xP)) %>%
  gt(groupname_col = "Region") %>%
  tab_header(
    title = (
      "2024 LEAGUES CUP PROBABILITES"
    ),
    subtitle = "Projections as of July 27, 2024 - Data provided by fbref via Opta"
  ) %>%
  cols_label(
    Team = "TEAM",
    Rank_1 = "1ST",
    Rank_2 = "2ND",
    prob_advance = "MAKE L32",
    xP = "Points",
    logo = ""
  ) %>%
  cols_move_to_start(
    columns = logo
  ) %>%
  cols_move(
    columns = c(Rank_1, Rank_2),
    after = Team
  ) %>%
  gt_img_rows(columns = logo, img_source = "local", height = px(25)) %>%
  tab_style(
    style = cell_borders(
      sides = "b",
      color = "black",
      weight = px(3)
    ),
    locations = list(cells_column_labels(), cells_stubhead())
    ) %>%
  cols_align(align = "center") %>%
  opt_table_font(
    font = list(
      google_font(name = "Roboto")
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", color = "#26282A")
    ),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "#26282A", size = px(18))
    ),
    locations = cells_body(
      columns = c(Team, Rank_1, Rank_2, prob_advance, xP)
    )
  ) %>%
  tab_style(
    style = cell_text(color = "#26282A"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_text(color = "#26282A"),
    locations = cells_column_labels()
  ) %>%
  fmt_percent(
    columns = c(Rank_1, Rank_2, prob_advance),
    decimals = 0
  ) %>%
  fmt_number(
    columns = xP,
    decimals = 2
  ) %>%
  gt_color_rows(columns = c(prob_advance, Rank_1, Rank_2), palette = "RColorBrewer::Blues",
                domain = c(0,1)) %>%
  gt_color_rows(columns = xP, palette = "RColorBrewer::Reds",
                domain = c(0.86, 5.24)
                ) %>%
  tab_options(
    heading.title.font.size = px(25),
    heading.align = "left",
    table.border.top.style = "hidden",
    column_labels.border.top.style = "hidden"
    ) %>%
  cols_width(
    Rank_1 ~ px(100),
    Rank_2 ~ px(100),
    prob_advance ~ px(100),
    xP ~ px(100)
  ) %>%
  tab_style(
    style = list(cell_borders(sides = "top", style = "hidden")),
    locations = cells_row_groups(groups = c("East 2", "East 3", "East 4", "East 5", "East 6", "East 7"))
  ) %>%
  tab_source_note(source_note = md(
    paste0(
      "<span style='font-family: \"Font Awesome 6 Brands\";'>&#xe61b;</span> ",
      "@AndresAnalytics ",
      "<span style='color: white;'>..</span>",
      "<span style='font-family: \"Font Awesome 6 Brands\";'>&#xf09b;</span> ",
      "OKcomputer626"
    )
  )
  ) %>%
  tab_style(
    style = cell_text(
      color = "#3b444b",
    ),
    locations = cells_source_notes()
  ) %>%
  tab_footnote(
    footnote = "Probabilities estimated using 4000 runs of Markov chain Monte Carlo for accuracy.",
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_style(
    style = cell_text(
      color = "#3b444b",
    ),
    locations = cells_footnotes()
  )



table2 <- results %>%
  filter(Region %in% c("West 1", "West 2", "West 3", "West 4", "West 5", "West 6", "West 7", "West 8")) %>%
  arrange(Region, desc(xP)) %>%
  gt(groupname_col = "Region") %>%
  tab_header(
    title = add_text_img(
        "",
      url = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/Leagues_Cup_logo_white-on-black.svg/1024px-Leagues_Cup_logo_white-on-black.svg.png", height = px(45)
    )
  ) %>%
  cols_label(
    Team = "TEAM",
    Rank_1 = "1ST",
    Rank_2 = "2ND",
    prob_advance = "MAKE L32",
    xP = "Points",
    logo = ""
  ) %>%
  cols_move_to_start(
    columns = logo
  ) %>%
  cols_move(
    columns = c(Rank_1, Rank_2),
    after = Team
  ) %>%
  gt_img_rows(columns = logo, img_source = "local", height = px(25)) %>%
  tab_style(
    style = cell_borders(
      sides = "b",
      color = "black",
      weight = px(3)
    ),
    locations = list(cells_column_labels(), cells_stubhead())
  ) %>%
  cols_align(align = "center") %>%
  opt_table_font(
    font = list(
      google_font(name = "Roboto")
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "#26282A", size = px(18))
    ),
    locations = cells_body(
      columns = c(Team, Rank_1, Rank_2, prob_advance, xP)
    )
  ) %>%
  tab_style(
    style = cell_text(color = "#26282A"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_text(color = "#26282A"),
    locations = cells_column_labels()
  ) %>%
  fmt_percent(
    columns = c(Rank_1, Rank_2, prob_advance),
    decimals = 0
  ) %>%
  fmt_number(
    columns = xP,
    decimals = 2
  ) %>%
  gt_color_rows(columns = c(prob_advance, Rank_1, Rank_2), palette = "RColorBrewer::Blues",
                domain = c(0,1)) %>%
  gt_color_rows(columns = xP, palette = "RColorBrewer::Reds",
                domain = c(0.86, 5.24)
  ) %>%
  tab_options(
    heading.align = "right",
    table.border.top.style = "hidden",
    column_labels.border.top.style = "hidden"
  ) %>%
  cols_width(
    Rank_1 ~ px(100),
    Rank_2 ~ px(100),
    prob_advance ~ px(100),
    xP ~ px(100)
  ) %>%
  tab_style(
    style = list(cell_borders(sides = "top", style = "hidden")),
    locations = cells_row_groups(groups = c("West 2", "West 3", "West 4", "West 5", "West 6", "West 7", "West 8"))
  )

tables <- list(table1, table2)
gt_two_column_layout(tables, output = "save", filename = "Bayesian Leagues Cup/images/Table Predictions.png", vwidth = 1200, vheight = 2000) 

