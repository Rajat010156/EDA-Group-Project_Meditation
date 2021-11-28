# Setting ggplot theme
my_theme <- theme_bw() + theme(plot.title = element_text(hjust  = 0.5, vjust = 0.5),
                               panel.grid = element_blank(), 
                               panel.background = element_rect(fill = '#f7edf7'))