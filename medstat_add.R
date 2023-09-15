

#devtools::install_github("mikkelkrogsholm/statsDK")

library(tidyverse)
library(readxl)
library(statsDK)
library(knitr)
library(kableExtra)
library(ggthemes)


# Datamanagement ----------------------------------------------------------

medstat <- map(.x = 1996:2022, .f = function(x) {
  prod_data <- read_delim(paste0("medstat_data/",x,"_product_name_data (1).txt"), ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
  names(prod_data) <- c("atc_code", "year", "sector", "product_id", "a1_sold_packs", "a1_sold_amount", "a1_turnover", "a2_sold_packs",
                        "a2_sold_packs_supp","a2_sold_amounts", "a2_turnover", "a2_regional_sup", "a3_sold_packs", "a3_sold_amounts")
  prod_data
}) |> bind_rows()

atc_codes <- read_delim("medstat_data/atc_code_text.txt", ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE) |> 
  rename(atc_code = X1, name = X2) |>
  select(atc_code, name) |> 
  mutate(name = if_else(str_detect(atc_code, "[*]"), atc_code, name)) |> 
  group_by(atc_code) |> 
  summarize(name = first(name)) |> 
  mutate(name = str_extract(name, "[^,]+"))

med_priser <- read_excel("medicinpriser_data/medicinpriser.xlsx", sheet = "lmpriser_eSundhed_230417") |> 
  select(ATC, Varenummer, Indikator, "20180226":"20230417") |> 
  pivot_longer(cols = "20180226":"20230417",names_to = "date", values_to = "price") |> 
  filter(Indikator == "AUP") |> 
  mutate(year=as.numeric(substr(date,1,4))) |> 
  group_by(year,Varenummer, ATC) |> 
  summarize(AIP = mean(price))

defl <- statsDK::sdk_retrieve_data("PRIS112") |> 
  filter(HOVED == "Average, yearly", between(TID, 1996, 2022)) |> 
  mutate(index = as.numeric(INDHOLD)/105.4)

med_data <- medstat |> 
  left_join(defl, by = c("year" = "TID")) |> 
  left_join(atc_codes, by = "atc_code") |> 
  left_join(med_priser, by = c("product_id" = "Varenummer", "year" = "year")) |> 
  mutate(cost_adj   = a1_turnover / index,
         SAIP       = (a1_turnover * 1000) / (a1_sold_packs * 1000),
         SAIP_lower = (a1_turnover * 1000) / ((a1_sold_packs + 0.1) * 1000),
         SAIP_upper = (a1_turnover * 1000) / ((a1_sold_packs - 0.1) * 1000),
         discount   = round(((SAIP / AIP) - 1) * 100, 1),
         discount_lower = round(((SAIP_lower / AIP) - 1) * 100, 1),
         discount_upper = round(((SAIP_upper / AIP) - 1) * 100, 1),
         AIP  = round(AIP),
         SAIP = round(SAIP))



# Data --------------------------------------------------------------------
top <- med_data |> filter(year==2021,sector == 200) |> arrange(desc(a1_turnover)) |> slice(1:20) |> pull(product_id)

tmp <- med_data |> 
  filter(sector == 200, 
         year<=2021,
         product_id %in% top) |> 
  arrange(year)

tmp3 <- tmp |> full_join(tmp |> expand(year, product_id), by=c("year","product_id"))

tmp2 <- split(tmp3$a1_turnover, tmp3$product_id)

tmp |> 
  arrange(desc(a1_turnover)) |> 
  filter(year==2021,
         !is.infinite(discount)) |> 
  mutate(hist = "") |> 
  select(product_id, atc_code,name, a1_turnover,hist, AIP, SAIP, discount, discount_lower, discount_upper) |> 
  mutate(a1_turnover = paste0(format(a1_turnover*1000, nsmall=0, big.mark=".",decimal.mark=",")," kr.")) |> 
  kbl(booktabs = TRUE) |> 
  kable_paper(full_width = FALSE)  %>%
    column_spec(5, image = spec_plot(tmp2,same_lim = F,polymin = NA))

medstat2 |> filter(name == "Pembrolizumab") |> view()
medstat2 |> filter(name == "Osimertinib") |> view()
medstat2 |> filter(name == "Lenalidomid") |> view()


med_data |> 
  filter(substr(atc_code, 1, 1) == "L",
         year %in% 2018:2021, 
         !is.infinite(discount),
         sector == 200,
         atc_code != "L01****") |> 
  group_by(year) |> 
  summarize(avg_discount = weighted.mean(x=discount, w=a1_turnover,na.rm = T))












pal <- ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]$`Classic 10`

# Download the following files from: https://medstat.dk/da/download

# 1. atc_code_text.txt
# 2. xxxx_product_name_data.txt

# First i start with the names of atc codes

atc_codes <- read_delim("medstat_data_2021/atc_code_text.txt", 
                        ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE) %>% 
  rename(atc_code = X1, name = X2) %>% 
  select(atc_code, name) %>% 
  mutate(name = if_else(str_detect(atc_code, "[*]"), atc_code, name)) %>% 
  group_by(atc_code) %>% 
  summarize(name = first(name)) %>% 
  mutate(name = str_extract(name, "[^,]+"))

# Next we read in all the other files
data_raw <- NULL
for (i in 1996:2021) {
  prod_data <- read_delim(paste0("medstat_data_2021/",i,"_product_name_data (1).txt"), 
                          ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
  names(prod_data) <- c("atc_code",
                        "year",
                        "sector",
                        "product_id",
                        "a1_sold_packs",
                        "a1_sold_amount",
                        "a1_turnover",
                        "a2_sold_packs",
                        "a2_sold_packs_supp",
                        "a2_sold_amounts",
                        "a2_turnover",
                        "a2_regional_sup",
                        "a3_sold_packs",
                        "a3_sold_amounts",
                        "unknown")
  data_raw <- rbind(data_raw, prod_data)
}

data <- data_raw %>% 
  full_join(atc_codes, by = "atc_code") %>% # join with names to convert atc code into generic name
  filter(sector == 200) %>% # Hospital medication
  filter(substr(atc_code, 1,3) %in% c("L01","L04")) # I subset to this group

# Now we have to select which drugs to highlight, from trial and error i find that a limit of 
# 1.3b will leave me with the 10 most expensive drugs
ttemp <- data %>% 
  group_by(name) %>% 
  summarize(sum = sum(a1_turnover, na.rm=T)) %>% 
  filter(sum > 1870000) %>% 
  select(name) %>% 
  mutate(ccode = "yes")

# Now i prepare data for plotting, 
temp <- data %>% 
  group_by(year, name) %>% 
  summarize(sum = sum(a1_turnover, na.rm=T)) %>% 
  group_by(year) %>% 
  mutate(rank = rank(-sum)) %>% 
  filter(rank < 22)  %>% 
  left_join(ttemp, by = "name") %>% 
  mutate(ccode = if_else(is.na(ccode), NA_character_, name))

temp %>% 
  ggplot(aes(year, rank, group = name, color = ccode)) + 
  geom_line(show.legend = F, size = 1.5, alpha = 0.6) + 
  geom_point(size = 2.5, shape = 21, stroke = 2.5, fill = "white", show.legend = F) +
  scale_color_manual(values = rev(pal$value), na.value = "grey") +
  scale_x_continuous(breaks = seq(1997, 2021, 3)) +
  geom_text(data = subset(temp, year == 2021), aes(label = name, x = year+0.5), 
            size = 4, show.legend = F, fontface = "bold", hjust = "left") +
  geom_text(data = subset(temp, year == 1997), aes(label = name, x = year-0.5), 
            size = 4, show.legend = F, fontface = "bold", hjust = "right") +
  labs(title = "Costly medication in Denmark", 
       subtitle = "Ranking of most expensive Antineoplastic agents and Immunosuppressants", 
       y = NULL, x = NULL, caption = "Source: Medstat") + 
  scale_y_reverse(breaks = c(1:21), limits = c(21, 1)) + 
  coord_cartesian(xlim = c(1995, 2022)) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(p, file="p.png", height = 5, width = 13, dpi = 500)






















# Everything --------------------------------------------------------------


data <- data_raw %>% 
  full_join(atc_codes, by = "atc_code") %>% # join with names to convert atc code into generic name
  filter(sector == 200) # Hospital medication

# Now we have to select which drugs to highlight, from trial and error i find that a limit of 
# 1.3b will leave me with the 10 most expensive drugs
ttemp <- data %>% 
  group_by(name) %>% 
  summarize(sum = sum(a1_turnover, na.rm=T)) %>% 
  filter(sum > 2300000) %>% 
  select(name) %>% 
  mutate(ccode = "yes")

# Now i prepare data for plotting, 
temp <- data %>% 
  group_by(year, name) %>% 
  summarize(sum = sum(a1_turnover, na.rm=T)) %>% 
  group_by(year) %>% 
  mutate(rank = rank(-sum)) %>% 
  filter(rank < 22)  %>% 
  left_join(ttemp, by = "name") %>% 
  mutate(ccode = if_else(is.na(ccode), NA_character_, name))

temp %>% 
  ggplot(aes(year, rank, group = name, color = ccode)) + 
  geom_line(show.legend = F, size = 1.5, alpha = 0.6) + 
  geom_point(size = 2.5, shape = 21, stroke = 2.5, fill = "white", show.legend = F) +
  scale_color_manual(values = rev(pal$value), na.value = "grey") +
  scale_x_continuous(breaks = seq(1997, 2021, 3)) +
  geom_text(data = subset(temp, year == 2021), aes(label = name, x = year+0.5), 
            size = 4, show.legend = F, fontface = "bold", hjust = "left") +
  geom_text(data = subset(temp, year == 1997), aes(label = name, x = year-0.5), 
            size = 4, show.legend = F, fontface = "bold", hjust = "right") +
  labs(title = "Costly medication in Denmark", 
       subtitle = "Ranking of most expensive Antineoplastic agents and Immunosuppressants", 
       y = NULL, x = NULL, caption = "Source: Medstat") + 
  scale_y_reverse(breaks = c(1:21), limits = c(21, 1)) + 
  coord_cartesian(xlim = c(1995, 2022)) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
