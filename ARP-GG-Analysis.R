setwd("C:/Users/gk115/OneDrive/Desktop/Applied Research Project")

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(car)

store_colors <- c("Greengrocer" = "#7FE716",  # green
                  "Supermarket" = "#E7E716")  # blue

df <- read_excel("Sustain_Final edited.xlsx")
names(df) <- make.names(names(df))  # Ensure valid R column names


parse_num <- function(x){
  x <- gsub(",", "", as.character(x))
  m <- stringr::str_extract(x, "\\d*\\.?\\d+")
  as.numeric(m)
}
df$price_perkg_num <- parse_num(df$price.per.kg.on.reciept)

# Normalise store type
df$Store.type <- trimws(tolower(df$Store.type))
df$store_type <- case_when(
  str_detect(df$Store.type, "green") ~ "Greengrocer",
  str_detect(df$Store.type, "super|coles|woolworth") ~ "Supermarket",
  TRUE ~ "Other"
)

df$suburb  <- str_to_title(trimws(df$Suburb))
df$product <- str_squish(df$Product.Name)

# Keep only rows with valid price
df_clean <- df %>% filter(!is.na(price_perkg_num) & price_perkg_num > 0)

# Comparable set (products sold by both types)
comp_products <- df_clean %>%
  distinct(product, store_type) %>%
  count(product) %>%
  filter(n == 2) %>%
  pull(product)

df_comp <- df_clean %>% filter(product %in% comp_products)


# Product × Retailer statistics
item_stats <- df_comp %>%
  group_by(product, store_type) %>%
  summarise(
    n_suburbs = n_distinct(suburb),
    mean_price = mean(price_perkg_num, na.rm = TRUE),
    sd_price = sd(price_perkg_num, na.rm = TRUE),
    cv_price = sd_price / mean_price,
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = store_type,
    values_from = c(mean_price, sd_price, cv_price, n_suburbs)
  )

# Compare Coefficient of Variation (CV)
cv_compare <- df_comp %>%
  group_by(product, store_type) %>%
  summarise(cv = sd(price_perkg_num) / mean(price_perkg_num), .groups = "drop") %>%
  pivot_wider(names_from = store_type, values_from = cv) %>%
  mutate(
    cv_diff = Greengrocer - Supermarket,
    more_stable = case_when(
      cv_diff < 0 ~ "Greengrocer",   # lower CV => more stable
      cv_diff > 0 ~ "Supermarket",
      TRUE ~ "Tie"
    )
  )


# Brown–Forsythe (Levene) Test for each product
bf_results <- df_comp %>%
  mutate(log_price = log(price_perkg_num)) %>%
  group_by(product) %>%
  group_modify(~{
    fit <- try(leveneTest(log_price ~ store_type, data = .x, center = median), silent = TRUE)
    if(inherits(fit, "try-error")) return(tibble(bf_stat = NA, bf_p = NA))
    tibble(bf_stat = fit[1, "F value"], bf_p = fit[1, "Pr(>F)"])
  }) %>%
  ungroup()

stability_table <- cv_compare %>%
  left_join(bf_results, by = "product") %>%
  mutate(significant = if_else(!is.na(bf_p) & bf_p < 0.05, "Yes", "No"))

# Summary and Visualisations
# Top 20 items by absolute CV difference
topN <- stability_table %>% arrange(desc(abs(cv_diff))) %>% head(20)

# Map for more stable
stable_colors <- c("Greengrocer" = store_colors["Greengrocer"],
                   "Supermarket" = store_colors["Supermarket"],
                   "Tie" = "#9E9E9E")

ggplot(topN, aes(x = reorder(product, cv_diff), y = cv_diff, fill = more_stable)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = stable_colors, name = "More stable") +
  labs(title = "CV Difference (Greengrocer - Supermarket)",
       y = "CV Difference", x = "Product") +
  theme_minimal()

# Boxplots of selected items
sel <- topN$product
ggplot(df_comp %>% filter(product %in% sel),
       aes(x = store_type, y = price_perkg_num, fill = store_type)) +
  geom_boxplot() +
  facet_wrap(~product, scales = "free_y") +
  scale_fill_manual(values = store_colors, name = "StoreType") +
  labs(title = "Unit Price Spread by Retailer (Selected Items)",
       y = "Unit Price ($/kg)", x = NULL) +
  theme_minimal()

# Count how many items more stable per retailer
summary_counts <- stability_table %>%
  summarise(
    items_total = n(),
    greengrocer_more_stable = sum(more_stable == "Greengrocer", na.rm = TRUE),
    supermarket_more_stable = sum(more_stable == "Supermarket", na.rm = TRUE),
    ties = sum(more_stable == "Tie", na.rm = TRUE)
  )
print(summary_counts)

# Export results
writexl::write_xlsx(
  list(
    Item_Stats = item_stats,
    Stability_Table = stability_table,
    Top20 = topN,
    Summary_Counts = summary_counts
  ),
  "SUSTAIN_Stability_Analysis.xlsx"
)

# Suburb-level variation summary
summary_suburb <- df_clean %>%
  group_by(Suburb = suburb, store_type) %>%
  summarise(
    MeanPrice = mean(price_perkg_num, na.rm = TRUE),
    SDPrice   = sd(price_perkg_num, na.rm = TRUE),
    CV        = (SDPrice / MeanPrice) * 100,
    .groups = "drop"
  ) %>%
  mutate(Suburb = str_squish(str_to_title(Suburb)))

# Plot CV by Suburb and Store Type
ggplot(summary_suburb, aes(x = Suburb, y = CV, fill = store_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = store_colors, name = "StoreType") +
  labs(title = "Price Variation (CV%) by Suburb and Store Type",
       y = "Coefficient of Variation (%)", x = "Suburb") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


