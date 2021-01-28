library(tidyverse)

setwd("~/GoogleDrive/ResearchProjects/[202101]SIRANO_JBO-GLM")

df <- read_csv("Data/VIM_combined.csv")
View(df)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

map_dfc(select(df, -X1), range01)

df_stdized <- 
    bind_cols(
        Feature = df$X1,
        mutate_all(select(df, "MI_mean", "RF_mean", "F_mean"), range01) 
    ) %>% 
    mutate(total_mean =(MI_mean + RF_mean + F_mean)/3) %>% 
    arrange(desc(total_mean))

write_csv(df_stdized, file="Data/VIM_mean_sorted.csv")


df_plot <- df_stdized
df_plot$Feature <- factor(df_plot$Feature, levels = df_plot$Feature[order(df_plot$total_mean)])

write_csv(filter(df_stdized, total_mean>=0.25)["Feature"], file="Data/selFeat1.csv")
