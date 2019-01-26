"
CSV to Latex Media Summary Table
"

# Set working dir.
setwd(githubdir)

# Load libs
library(xtable)
library(dplyr)

# Read in data
uk_sum <- read.csv("uk_not_news/tabs/uk_media_clean_summarized_by_label_gt1k.csv")

# Convert date to date
uk_sum$from_date <- as.Date(uk_sum$from_date, format = '%Y-%m-%d')
uk_sum$to_date   <- as.Date(uk_sum$to_date, format = '%Y-%m-%d')

# Merge Source Names (We just have numeric Labels)

# Get Source Name Strings
uk_labs <- read.csv("not_news/tabs/uk_domain+source_label.csv")[, c("groupby_label", "string_label")]
uk_labs <- uk_labs[!duplicated(uk_labs$groupby_label), ]

uk_media <- uk_sum %>%
  inner_join(uk_labs, by = c("label" = "string_label"))

# Preparing for printing
uk_media$label  <- tools::toTitleCase(uk_media$label)

# Subset and rename for final output
uk_media <- uk_media[, c("label", "from_date", "to_date", "n_transcripts")]
names(uk_media) <- c("Name", "From", "To", "No. of Transcripts") 
uk_media$Name   <- gsub("&", "and", uk_media$Name)
uk_media$Name   <- gsub("_", "\\\\_", uk_media$Name)

print(
      xtable(uk_media, 
          caption = "Summary of the Media Data", 
          align = c("p{0.15\\textwidth}",  "p{0.45\\textwidth}", "p{0.15\\textwidth}", "p{0.15\\textwidth}", "p{0.1\\textwidth}"),
          label = "tab:summary",
          digits = 0), 
        include.rownames = FALSE,
        floating = FALSE,
        include.colnames = TRUE,
        size = "\\tiny", 
        tabular.environment = "longtable",
        type = "latex",
        sanitize.text.function = function(x){x},
        caption.placement = "top",
        table.placement = "!htb",
        file = "uk_not_news/tabs/media_summary.tex")
