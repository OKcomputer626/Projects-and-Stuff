library(tidyverse)
library(showtext)
library(ggtext)
library(ggbeeswarm)
library(patchwork)

font_add_google("Oswald", "Oswald")
font_add_google("Roboto", "Roboto")
showtext_auto()
showtext_opts(dpi = 300)


df <- read_csv("Boxing/data/Canelo.csv")

df <- df %>%
  separate(`Round, time`, into = c("Round", "Time"), sep = ", ", fill = "right")

df <- df %>%
  mutate(Round = gsub("\\(.*\\)", "", Round)) %>%
  mutate(Round = trimws(Round))  # Trim any extra spaces


# Function to convert time in mm:ss format to seconds
time_to_seconds <- function(time) {
  if (!is.na(time)) {
    time_parts <- unlist(strsplit(time, ":"))
    return(as.numeric(time_parts[1]) * 60 + as.numeric(time_parts[2]))
  } else {
    return(NA)
  }
}

# Add duration column
df <- df %>%
  mutate(Time_in_seconds = sapply(Time, time_to_seconds)) %>%
  mutate(Round = as.numeric(Round),
         # Assuming each round lasts 3 minutes (180 seconds)
         Duration = ifelse(is.na(Time_in_seconds), Round * 180, (Round - 1) * 180 + Time_in_seconds)) %>%
  mutate(x = 1)

win <- df %>%
  filter(Result %in% "Win")

df_cleaned <- df %>%
  select(Result, Duration, Type, Round, Time, x)

round_7 <- df_cleaned %>% 
  summarise(perc = round(sum(Duration <= 1080) / nrow(df) * 100, 2)) %>%
  pull(perc)

ko_perc_win <- df_cleaned %>%
  summarise(perc2 = round(sum(Type %in% c("KO", "TKO", "RTD")) / nrow(win) * 100, 2)) %>%
  pull(perc2)

df_axis <- tibble(seconds = c(60, 180, 540, 1080, 1800, 2160),
                  label = c("01:00", "03:00","09:00","18:00", "30:00", "36:00"))


plot_record <- df_cleaned %>%
  ggplot(aes(x = x, y = Duration)) +
  geom_text(data = df_axis, aes(x = 3, y = seconds + 25, label = label), family = "Roboto", size = 3, color = "#000000", hjust = "right", vjust = "bottom") +
  annotate("segment", x = -1, xend = 3, y = df_axis$seconds, color = "#000000", linewidth = 0.3) +
  annotate("richtext", x = -0.3, y = 380, label = glue::glue("<span style = 'font-size:14pt;'><b>{round_7}%</b></span><br><br>of fights<br>ended in<br>18 minutes<br>or less"), family = "Oswald", size = 3, color = "#FFFFFF", hjust = 0.5, vjust = "bottom",
           fill = "#000000", label.padding = unit(c(1, .5, 1, .5), "lines"), angle = 10, lineheight = 1.2) +
  annotate("richtext", x = 2.1, y = 1280, label = glue::glue("<span style = 'font-size:14pt;'><b>{ko_perc_win}%</b></span><br><br>of win fights<br>decided by<br><span style = 'color: #FAEd27;'><b>KO</b></span> or <span style = 'color: #FAEd27;'><b>TKO</b></span>"), family = "Oswald", size = 3, color = "#FFFFFF",
           hjust = 0.5, vjust = "bottom", fill = "#000000", label.padding = unit(c(1, .5, 1, .5), "lines"), angle = -10, lineheight = 1.2) +
  geom_beeswarm(aes(fill = ifelse(Type %in% c("KO", "TKO", "RTD"), "#FAEd27", "#FFFFFF")), size = 4.75, cex = 4.25, color = "#000000", shape = 21) +
  scale_fill_identity() +
  geom_beeswarm(aes(color = Result), size = 2.25, cex = 4.25) +
  scale_color_manual(values = c(Win = "#228B22", Loss = "#B80F0A", Draw = "#7CB9E8")) +
  theme_void() +
  theme(plot.title = element_text(family = "Oswald", size = 38, color = "#000000", face = "bold", hjust = 0.5, margin = margin(b = 1)),
        plot.subtitle = element_markdown(family = "Oswald", size = 9, color = "#000000", hjust = 0.5, margin = margin(b = 25)),
        plot.title.position = "plot",
        plot.caption = element_text(family = "Oswald", hjust = 0.5, size = 7.5, color = "#000000", margin = margin(t = 25)),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")
        ) +
  labs(title = "CANELO ALVAREZ",
       subtitle = "Fights by duration (mm:ss)",
       caption = "Source: boxrec.com • Graphic: Andres Gonzalez")


# Create a data frame with Canelo's updated record information
canelo_record <- tibble(
  x = c("1", "2", "3"),
  y = "1",
  label = c(
    "62",
    "2",
    "2"
    ),
  fill = c("#228B22", "#B80F0A", "#7CB9E8") # Colors matching the image
)

# Create the plot for the record
record_plot <- ggplot(canelo_record, aes(x, y, fill = fill, label = label)) +
  geom_tile(color = "white", lwd = 0.8) + # Blocks for the record
  geom_text(color = "white", size = 5, family = "Roboto") +
  scale_fill_identity() + # Use the specified colors
  scale_x_discrete(labels = c("39 KOs", "0 KOs", "")) +
  scale_y_discrete(expand = c(0,0)) +
  theme_void() +
  theme(axis.text.x = element_text(colour = canelo_record$fill, size = 7))

#set up legend for plot
df_legend<- tibble(
  image = "Boxing/pics/Canelo-modified.png",
  text = "**Age:** 34<br>**Nationality:** Mexico<br>**Stance:** Orthodox<br>**Height:** 5′ 7½″ / 171cm<br>**Reach:** 70½″ / 179cm",
)

plot_record +
  ggimage::geom_image(data = df_legend, aes(x = -0.25, y = 1650, image = image), size = 0.12) +
  geom_richtext(data= df_legend, aes(x = -0.25, y= 1325, label = text), size = 2.5, family = "Roboto") +
  inset_element(record_plot, left = 0.4, right = 0.6, bottom = 0.78, top = 0.83, align_to = "full")
  
  
ggsave(paste0("Boxing/output/Canelo_", format(Sys.time(), "%m%d%Y"), ".png"), width = 7, height = 7)
