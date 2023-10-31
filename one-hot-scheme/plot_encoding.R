library(tidyverse)


colors <-list("A" = '#109648', "C" = '#255C99', "G" = '#F7B32B', "T" = '#D62839')



# Initial Data Frame ------------------------------------------------------

loc <- data.frame(
  nt = c("A", "C", "G", "T"),
  loc = 1:4
)

data <- data.frame(nt = c("A", "A", "T", "C", "G", "T", "T", "C", "G")) %>% 
  left_join(loc, by = "nt") %>% 
  mutate(y = -row_number())




# With SNPs ---------------------------------------------------------------

# Add two SNPs 
snps <- data.frame(
  nt = c("T", "G"),
  y = -c(2, 6)) %>% left_join(loc, by = "nt")

# Make the fill values smaller to reflect uncertainty
data2 <- rbind(data, snps) 
  

data2



# Plotting code -----------------------------------------------------------

# alpha_vals <- list("SNP" = 0.5, "REF" = 1)

plot_seq <- function(df){
  df %>% 
    group_by(y) %>% 
    mutate(fill = ifelse(n() > 1, "SNP", "REF")) %>% 
    ungroup() %>% 
    ggplot(aes(x = loc, y = y, fill = nt)) +
    geom_tile(aes(alpha = fill)) +
    geom_text(aes(label = nt)) +
    scale_fill_manual(values = colors, guide = "none") +
    theme_void() +
    coord_equal() +
    scale_alpha_manual( values = c(1, 0.4), guide = "none") 
}


p1 <- plot_seq(data) + ggtitle("Standard method (no SNP encoding)")
p2 <- plot_seq(data2) + ggtitle("SNPs encoded")

p2

ggsave("one-hot-reference.png", p1)
ggsave("one-hot-SNPs.png", p2
)
