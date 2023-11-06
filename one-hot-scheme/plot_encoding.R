library(tidyverse)

# From MEME Suite
colors <-list("A" = '#109648', "C" = '#255C99', "G" = '#F7B32B', "T" = '#D62839')


# Initial Data Frame ------------------------------------------------------

loc <- data.frame(
  nt = c("A", "C", "G", "T"),
  loc = 1:4
)


make_seq_data_from_string <- function(str){
  nt <- strsplit(str, "")[[1]]
  
  data <- data.frame(nt = nt) %>% 
    left_join(loc, by = "nt") %>% 
    mutate(y = -row_number())
}

data <- make_seq_data_from_string("AATCGTTCGCTTTA")

# With SNPs ---------------------------------------------------------------

# Add two SNPs 
snps <- make_seq_data_from_string("TG")
snps$y <- c(-2, -6) 
  

# Make the fill values smaller to reflect uncertainty
data2 <- rbind(data, snps) 
data2



# Plotting code -----------------------------------------------------------

el_line_def <- element_line(color = "black", size = 0.3)
txt_margin <- margin(0.5, 0.5, 0.5, 0.5, "cm")

seq_theme <- theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = el_line_def,
        axis.line.x = el_line_def,
        axis.line.y = el_line_def,
        axis.text.y = element_text(hjust = 0.5, size = 16, margin = txt_margin),
        plot.background = element_rect(fill = "white", color = "white"),
        title = element_text(hjust = 0.5, size = 18, margin = txt_margin)
  )
  

plot_seq <- function(df, drop = c(-10, -11, -12)){
  
  df2 <- df %>% 
    group_by(y) %>% 
    mutate(
      fill = ifelse(n() > 1, "SNP", "REF"),
      encoding = ifelse(n() > 1, "0.5","1"),
      lab = paste(nt, collapse = "/")
    ) %>% ungroup() 
  
  # Grab the SNPs information to use as a y label
  # E.g. c("C", "C/T", ...)
  y_labs <- arrange(df2, y) %>% 
    group_by(y) %>% 
    slice(1) %>% 
    ungroup() %>% 
    dplyr::filter(!(y %in% drop))
  
  
  print(y_labs$lab)
  
  p <- df2 %>% 
      ggplot(aes(x = loc, y = y, fill = nt)) +
      geom_tile(aes(alpha = fill), color = "black") +
      geom_text(aes(label = encoding)) +
      scale_fill_manual(values = colors, guide = "none") +
      coord_equal(clip = "off") +
      scale_alpha_manual(values = c(1, 0.4), guide = "none") +
      scale_x_continuous(
        minor_breaks = seq(0.5, 4.5, by = 1),
        expand = c(0, 0)
        ) +
      scale_y_continuous(
        minor_breaks = seq(0.5, min(df2$y) - 0.5, by = -1),
        breaks = y_labs$y,
        labels = y_labs$lab,
        expand = c(0, 0)
        ) +
      seq_theme
  
  if (length(drop) > 0){
    p +
      geom_rect(
        aes(
          xmin = -Inf, 
          xmax = Inf, 
          ymin = min(drop) - 0.5, 
          ymax = max(drop) + 0.5
          ),
        inherit.aes = FALSE,
        fill = "white",
        color = NA
        ) +
      geom_text(aes(x = 2.5, y = mean(drop), label = "..."), 
                size = 14,
                hjust = 0.5,
                vjust = 0.2)
    
  } else {
    p
  }
}

p1 <- plot_seq(data) + ggtitle("No SNP encoding")
p2 <- plot_seq(data2) + ggtitle("SNP encoding")

p1
p2

plot_seq(data_bottom) + ylim(c(0, -10))

ggsave("one-hot-reference.png", p1, width = 4, height = 8)
ggsave("one-hot-SNPs.png", p2, width = 4, height = 8)




# Trapezoid ---------------------------------------------------------------

conv_data <- data.frame(
  x = c(0,0,3,3, 4,4, 6,6),
  y = c(0,6,4.5,1.5, 1.5,4.5, 3.75, 2.25),
  layer = c(rep("Conv 1", 4),
            rep("Conv 2", 4)
  ),
  type = rep("Convolutional", 8)
)


flt_data <- data.frame(
  x = c(7,7, 7.5,7.5),
  y = c(0,6, 6,0),
  layer = rep("Flatten 1", 4),
  type = rep("Flatten", 4)
)

fc_data <- data.frame(
  x = c(8,8, 8.5, 8.5, 9,9, 9.5, 9.5, 10,10, 10.5,10.5),
  y = c(0,6, 6,0, 1.5, 4.5, 4.5,1.5, 1.5, 4.5, 4.5,1.5),
  layer = c(rep("Dense 1", 4), 
            rep("Dense 2", 4),
            rep("Dense 2", 4)),
  type = rep("Fully connected", 12)
)

all_layers = rbind(conv_data, flt_data, fc_data)

z <- all_layers %>% 
  ggplot(aes(x, y, group = layer, fill = type)) +
  geom_polygon() +
  annotate("text", x = 12, y = 3.25, parse = TRUE,
           label = "hat(pi)", size = 18) +
  coord_equal() +
  theme_void() +
  theme(
    legend.position="bottom",
    legend.title = element_blank(),
    text = element_text(size=rel(24)),
    plot.background = element_rect(fill = "white", color = "white")
    ) +
  scale_fill_manual(values = c("#1D5D9B", "#75C2F6", "#F4D160", "#FBEEAC")) 

ggsave(z, filename = "architecture.png", width = 16, height = 8)
