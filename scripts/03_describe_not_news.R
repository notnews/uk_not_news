"

a. Distribution of share of not news by outlet,
b. Prop. by Outlet, density and dotplot
c. Prop. by Domain, density
"

# Set working dir.
setwd(githubdir)

# Load libs
library(ggplot2)
library(tools)
library(dplyr)
library(goji)
library(xtable)

# Read in data
uk_pred <- read.csv("uk_not_news/tabs/uk_media_url_pred.csv")

# Prop. of articles that are soft news
mean(uk_pred$pred_prob < .5)

# Get Year 
uk_pred$year <- as.numeric(substr(uk_pred$date, 1, 4)) 

# Custom ggplot theme
cust_theme <- theme_minimal() +
    theme(panel.grid.major = element_line(color = "#e1e1e1",  linetype = "dotted"),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.key       = element_blank(),
      legend.key.width = unit(1, "cm"),
      axis.title   = element_text(size = 10, color = "#555555"),
      axis.text    = element_text(size = 10, color = "#555555"),
      axis.title.x = element_text(vjust = 1, margin = margin(10, 0, 0, 0)),
      axis.title.y = element_text(vjust = 1),
      axis.ticks   = element_line(color = "#e1e1e1", linetype = "dotted", size = .2),
      axis.text.x  = element_text(vjust = .3),
      plot.margin = unit(c(.5, .75, .5, .5), "cm"))

# Prop. soft news by outlet
uk_not_news_by_outlet <- uk_pred %>%
    group_by(src_name) %>%
    summarize(share_of_not_news = mean(1 - pred_prob), `Regex Based` = mean(train_label, na.rm = T))

# Average proportion
avg_outlet_prop <- mean(uk_not_news_by_outlet$share_of_not_news, na.rm = T)
summary(uk_not_news_by_outlet$share_of_not_news)
uk_not_news_by_outlet[uk_not_news_by_outlet$share_of_not_news > .7, ]

ggplot(uk_not_news_by_outlet, aes(share_of_not_news)) +
  geom_density(aes(y = ..scaled..), color = "#42C4C7", alpha = 0.35) +
  annotate("text", label = paste("Mean =", nolead0s(round(avg_outlet_prop, 2))), x = .54, y = 1, size = 3, colour = "black") + 
  geom_vline(xintercept = mean(uk_not_news_by_outlet$share_of_not_news, na.rm = T), col = "#cc0000", alpha = .75, linetype = "dotted") + 
  xlab("Share of Soft News in an Outlet") +
  ylab("") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  scale_x_continuous(breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  cust_theme

ggsave("uk_not_news/figs/uk_not_news_by_outlet.pdf")

# Dot plot for main outlets
main_outlets <- c("the daily mail",
                  "the daily telegraph",
                  "the guardian",
                  "the daily star",
                  "bbc news - uk",
                  "bolton news",
                  "glasgow evening times",
                  "bradford telegraph & argus",
                  "south wales evening post",
                  "hull daily mail",
                  "the independent uk",
                  "international business times",
                  "express and star",
                  "manchester evening news",
                  "birmingham mail",
                  "lancashire evening post",
                  "southern daily echo",
                  "yorkshire post",
                  "yorkshire evening post")

uk_not_news_by_outlet_main = subset(uk_not_news_by_outlet, src_name %in% main_outlets)
uk_not_news_by_outlet_main$src_name  <- droplevels(factor(uk_not_news_by_outlet_main$src_name))
uk_not_news_by_outlet_main$src_name = factor(uk_not_news_by_outlet_main$src_name, levels = uk_not_news_by_outlet_main$src_name[order(uk_not_news_by_outlet_main$share_of_not_news)])

ggplot(uk_not_news_by_outlet_main, aes(share_of_not_news, src_name, order = share_of_not_news)) +
  geom_point(aes(alpha = .8)) +
  ylab("") +
  xlab("") +
  scale_colour_manual(values = c("#dd3333", "#3333dd")) +
  scale_x_continuous("Proportion of Soft News Stories", breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1))) +
  cust_theme +
  theme(legend.position = "none")

ggsave("uk_not_news/figs/uk_not_news_by_outlet_main_dotplot.pdf")

# Produce summary table
print(
      xtable(uk_not_news_by_outlet, 
          caption = "Share of Not-News By Outlet", 
          align = c("p{0.1\\textwidth}",  "p{0.3\\textwidth}", "p{0.1\\textwidth}", "p{0.15\\textwidth}"), label = "tab:outlet_not_news"), 
        include.rownames = FALSE,
        floating = FALSE,
        include.colnames = TRUE,
        size = "\\tiny", 
        tabular.environment = "longtable",
        type = "latex",
        sanitize.text.function = function(x){x},
        caption.placement = "top",
        table.placement = "!htb",
        file = "not_news/tabs/uk_not_news_by_outlet.tex")

# Prop. soft news by domain
uk_not_news_by_domain <- uk_pred %>%
    group_by(domain) %>%
    summarize(share_of_not_news = mean(1 - pred_prob), `Regex Based` = mean(train_label, na.rm = T))

# Average proportion
avg_outlet_prop <- mean(uk_not_news_by_domain$share_of_not_news, na.rm = T)
summary(uk_not_news_by_domain$share_of_not_news)
uk_not_news_by_domain[uk_not_news_by_domain$share_of_not_news > .7, ]

ggplot(uk_not_news_by_domain, aes(share_of_not_news)) + 
  geom_density(aes(y = ..scaled..), color = "#42C4C7", alpha = 0.35) +
  geom_vline(xintercept = median(uk_not_news_by_domain$share_of_not_news), col = "#cc0000", alpha = .75, linetype = "dotted") + 
  annotate("text", label = paste("Mean =", nolead0s(round(avg_outlet_prop, 2))), x = .54, y = 1, size = 3, colour = "black") + 
  xlab("Proportion of Soft News Stories") +
  ylab("") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  scale_x_continuous(breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  cust_theme +
  theme(legend.position = "none")

ggsave("uk_not_news/figs/prop_soft_news_by_domain.pdf")

print(
      xtable(uk_not_news_by_domain, 
          caption = "Share of Soft News By Domain", 
          align = c("p{0.1\\textwidth}",  "p{0.3\\textwidth}", "p{0.1\\textwidth}", "p{0.15\\textwidth}"), label = "tab:outlet_not_news"), 
        include.rownames = FALSE,
        floating = FALSE,
        include.colnames = TRUE,
        size = "\\tiny", 
        tabular.environment = "longtable",
        type = "latex",
        sanitize.text.function = function(x){x},
        caption.placement = "top",
        table.placement = "!htb",
        file = "uk_not_news/tabs/uk_not_news_by_domain.tex")

## Difference Between Afternoon Editions and Morning Editions
uk_not_news_by_outlet$evening <- grepl("evening", uk_not_news_by_outlet$src_name)

# Prop. soft news by domain
uk_not_news_by_evening <- uk_not_news_by_outlet %>%
    group_by(evening) %>%
    summarize(share_of_not_news_pred = mean(share_of_not_news))

# Over time trend
set.seed(31415)
uk_pred_samp <- uk_pred[sample(1:nrow(uk_pred), 2000000), ]
with(uk_pred_samp, summary(lm(pred_label ~  zero1(year, 2003, 2015) + src_name)))

# By Outlet
library(tidyverse)
library(broom)

outlet_reg <- uk_pred %>% 
  group_by(src_name) %>%
  do(coofs = coef(lm(pred_label ~ zero1(year, 2003, 2015), data = .)))

mean(sapply(outlet_reg$coofs, "[[", 2), na.rm = T)
# [1] -0.00793982
