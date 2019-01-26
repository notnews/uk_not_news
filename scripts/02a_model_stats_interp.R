"

CSV to Latex:
a. Top 100 Coefs. for Soft/Hard
b. Generalization Error

"

# Set working dir.
setwd(githubdir)

# Load libs
library(xtable)
library(tools)
library(readr)
library(dplyr)

# Read in data
top100_hard <- read.csv("uk_not_news/tabs/url_uk_top100_hard.csv")
top100_soft <- read.csv("uk_not_news/tabs/url_uk_top100_soft.csv")

# Subset and rename for final output
top100_hard_tab <- cbind(top100_hard$term[1:34], top100_hard$term[35:68], c(top100_hard$term[69:100], "", "")) 
names(top100_hard_tab) <- c("", "", "") 

top100_soft_tab <- cbind(top100_soft$term[1:34], top100_soft$term[35:68], c(top100_soft$term[69:100], "", "")) 
names(top100_soft_tab) <- c("", "", "") 

# Output to .tex
print(
      xtable(top100_hard_tab, 
          caption = "Top 100 Predictors of Hard News", 
          align = c("p{0.10\\textwidth}", "p{0.3\\textwidth}",  "p{0.3\\textwidth}",  "p{0.3\\textwidth}"), label = "tab:top_100_hard"), 
        include.rownames = FALSE,
        floating = FALSE,
        include.colnames = FALSE,
        size = "\\small", 
        type = "latex",
        sanitize.text.function = function(x){x},
        caption.placement = "top",
        tabular.environment = "longtable",
        table.placement = "!htb",
        file = "uk_not_news/tabs/url_uk_top100_hard.tex")


print(
      xtable(top100_soft_tab, 
          caption = "Top 100 Predictors of Soft News", 
          align = c("p{0.10\\textwidth}", "p{0.3\\textwidth}",  "p{0.3\\textwidth}",  "p{0.3\\textwidth}"), label = "tab:top_100_soft"), 
        include.rownames = FALSE,
        floating = FALSE,
        include.colnames = FALSE,
        size = "\\small", 
        type = "latex",
        sanitize.text.function = function(x){x},
        caption.placement = "top",
        tabular.environment = "longtable",
        table.placement = "!htb",
        file = "uk_not_news/tabs/url_uk_top100_soft.tex")

# Generalization Error Using Hand Coded Articles
test_samp_manual <- read_csv("uk_not_news/tabs/test_sample_coded.csv")

uk_pred <- read_csv("uk_not_news/tabs/uk_media_url_pred.csv")
uk_pred$id <- 0:(nrow(uk_pred) - 1)

test <- test_samp_manual %>%
  left_join(uk_pred, by = "id")

# Confusion Matrix
table(test$pred_label, test$label.x)

1 - sum(diag(table(test$pred_label, test$label.x)))/sum(!is.na(test$label.x))
# 0.8770161
