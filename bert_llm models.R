# Packages ====
pkg <- c("openxlsx", "readr", "tidyverse")
lapply(pkg, require, character.only = TRUE)
rm(pkg)

# Data ====
bert <- read.xlsx("bert_shrtkey_scores_13Sep23.xlsx")
#llm_ab <- read_csv("LLM_UNDP_results_v16_topp1 temp 0_antigua&barbuda_7Sep23.csv")
#llm_ml <- read_csv("LLM_UNDP_results_v16_topp1 temp 0_Maldives_7Sep23.csv")
#llm_sl <- read_csv("LLM_UNDP_results_v16_topp1 temp 0_Sri Lanka_7Sep23.csv")
llm_cb <- read_csv("UNDP LLM Policy Tool Results_Cambodia_24Sep23.csv")
llm_in <- read_csv("UNDP LLM Policy Tool Results_India_25Sep23.csv")
llm_id <- read_csv("UNDP LLM Policy Tool Results_Indonesia_24Sep23.csv")
llm_ml <- read_csv("UNDP LLM Policy Tool Results_Maldives_24Sep23.csv")
llm_pn <- read_csv("UNDP LLM Policy Tool Results_PNG_24Sep23.csv")
llm_sr <- read_csv("UNDP LLM Policy Tool Results_Sri Lanka_24Sep23.csv")
llm_cr <- read_csv("UNDP LLM Policy Tool Results_Costa Rica_25Sep23.csv")

# Tweaks ====
bert <- bert %>% 
  select(Country, category, GBF.GOAL.A, GBF.GOAL.B, GBF.GOAL.C, GBF.GOAL.D, 
         GBF.TARGET.1, GBF.TARGET.2, GBF.TARGET.3, GBF.TARGET.4, GBF.TARGET.5, 
         GBF.TARGET.6, GBF.TARGET.7, GBF.TARGET.8, GBF.TARGET.9, GBF.TARGET.10, 
         GBF.TARGET.11, GBF.TARGET.12, GBF.TARGET.13, GBF.TARGET.14, GBF.TARGET.15, 
         GBF.TARGET.16, GBF.TARGET.17, GBF.TARGET.18, GBF.TARGET.19, GBF.TARGET.20, 
         GBF.TARGET.21, GBF.TARGET.22, GBF.TARGET.23)
bert <- bert %>% 
  filter(Country != "global") %>% 
  pivot_longer(starts_with('GBF'), names_to = "GBF", values_to = "BERT Scores")

## normalize scores
min <- min(bert$"BERT Scores")
max <- max(bert$"BERT Scores")
bert$"BERT nScores" <- round(100*(bert$"BERT Scores" - min)/(max - min), 2)
##

# Cambodia, India, Indonesia, Maldives, Papua New Guinea, Sri Lanka
bert <- bert %>% 
  #filter(Country == "Antigua &amp; B." | Country == "Maldives" | Country == "Sri Lanka")
  filter(Country == "Cambodia" | Country == "India" | Country == "Indonesia" |Country == "Maldives" | Country == "Papua New Guinea" | Country == "Sri Lanka" | Country == "Costa Rica")

bert$GBF <- substr(bert$GBF, 5, nchar(bert$GBF)) %>% 
  tolower()
#bert$Country <- str_replace(bert$Country, "amp;", "")
bert$GBF <- gsub(".", " ", bert$GBF, fixed=TRUE)
bert <- bert %>% rename("NBT" = "category")

#llm_ab <- llm_ab %>% rename("NBT" = "NBT Name/Title", "NBT Text" = "NBT Text - English", "GBF" = "GBF Target...7", "GBF Text" = "GBF Target Text", "LLM Scores" = "Score", "LLM Results" = "Full results", "LLM Description" = "Description") %>% select(-"GBF Target...4")
#llm_ab$Country <- str_replace(llm_ab$Country, "and Barbuda", "& B.")
#llm_ab$GBF <- substr(llm_ab$GBF, 5, nchar(llm_ab$GBF))
#llm_ab$GBF <- str_replace(llm_ab$GBF, "target a", "goal a")
#llm_ab$GBF <- str_replace(llm_ab$GBF, "target b", "goal b")
#llm_ab$GBF <- str_replace(llm_ab$GBF, "target c", "goal c")
#llm_ab$GBF <- str_replace(llm_ab$GBF, "target d", "goal d")
#llm_ab$"LLM Results" <- substr(llm_ab$"LLM Results", str_locate(llm_ab$"LLM Results", " #T")[, 2], nchar(llm_ab$"LLM Results"))
#llm_ab$"GBF Text" <- substr(llm_ab$"GBF Text", str_locate(llm_ab$"GBF Text", ": ")[, 2], nchar(llm_ab$"GBF Text"))
    
#llm_ml <- llm_ml %>% rename("NBT" = "NBT Name/Title", "NBT Text" = "NBT Text - English", "GBF" = "GBF Target...7", "GBF Text" = "GBF Target Text", "LLM Scores" = "Score", "LLM Results" = "Full results", "LLM Description" = "Description") %>% select(-"GBF Target...4")
#llm_ml$NBT <- str_replace(llm_ml$NBT, "S2Target 4", "S2 Target 4")
#llm_ml$GBF <- substr(llm_ml$GBF, 5, nchar(llm_ml$GBF))
#llm_ml$GBF <- str_replace(llm_ml$GBF, "target a", "goal a")
#llm_ml$GBF <- str_replace(llm_ml$GBF, "target b", "goal b")
#llm_ml$GBF <- str_replace(llm_ml$GBF, "target c", "goal c")
#llm_ml$GBF <- str_replace(llm_ml$GBF, "target d", "goal d")
#llm_ml$"LLM Results" <- substr(llm_ml$"LLM Results", str_locate(llm_ml$"LLM Results", " #T")[, 2], nchar(llm_ml$"LLM Results"))
#llm_ml$"GBF Text" <- substr(llm_ml$"GBF Text", str_locate(llm_ml$"GBF Text", ": ")[, 2], nchar(llm_ml$"GBF Text"))

#llm_sl <- llm_sl %>% rename("NBT" = "NBT Name/Title", "NBT Text" = "NBT Text - English", "GBF" = "GBF Target...7", "GBF Text" = "GBF Target Text", "LLM Scores" = "Score", "LLM Results" = "Full results", "LLM Description" = "Description") %>% select(-"GBF Target...4")
#llm_sl$GBF <- substr(llm_sl$GBF, 5, nchar(llm_sl$GBF))
#llm_sl$GBF <- str_replace(llm_sl$GBF, "target a", "goal a")
#llm_sl$GBF <- str_replace(llm_sl$GBF, "target b", "goal b")
#llm_sl$GBF <- str_replace(llm_sl$GBF, "target c", "goal c")
#llm_sl$GBF <- str_replace(llm_sl$GBF, "target d", "goal d")
#llm_sl$"LLM Results" <- substr(llm_sl$"LLM Results", str_locate(llm_sl$"LLM Results", " #T")[, 2], nchar(llm_sl$"LLM Results"))
#llm_sl$"GBF Text" <- substr(llm_sl$"GBF Text", str_locate(llm_sl$"GBF Text", ": ")[, 2], nchar(llm_sl$"GBF Text"))

llm_cb <- llm_cb %>% rename("NBT" = "NBT Name/Title", "NBT Text" = "NBT Text - English", "GBF" = "GBF Target...7", "GBF Text" = "GBF Target Text", "LLM Scores" = "Score", "LLM Results" = "generated_sample (full results)", "LLM Description" = "Description") %>% select(-"GBF Target...4")
llm_cb$GBF <- substr(llm_cb$GBF, 5, nchar(llm_cb$GBF))
llm_cb$GBF <- str_replace(llm_cb$GBF, "target a", "goal a")
llm_cb$GBF <- str_replace(llm_cb$GBF, "target b", "goal b")
llm_cb$GBF <- str_replace(llm_cb$GBF, "target c", "goal c")
llm_cb$GBF <- str_replace(llm_cb$GBF, "target d", "goal d")
llm_cb$"LLM Results" <- substr(llm_cb$"LLM Results", str_locate(llm_cb$"LLM Results", " #T")[, 2], nchar(llm_cb$"LLM Results"))
llm_cb$"GBF Text" <- substr(llm_cb$"GBF Text", str_locate(llm_cb$"GBF Text", ": ")[, 2], nchar(llm_cb$"GBF Text"))

llm_in <- llm_in %>% rename("NBT" = "NBT Name/Title", "NBT Text" = "NBT Text - English", "GBF" = "GBF Target...7", "GBF Text" = "GBF Target Text", "LLM Scores" = "Score", "LLM Results" = "generated_sample (full results)", "LLM Description" = "Description") %>% select(-"GBF Target...4")
llm_in$GBF <- substr(llm_in$GBF, 5, nchar(llm_in$GBF))
llm_in$GBF <- str_replace(llm_in$GBF, "target a", "goal a")
llm_in$GBF <- str_replace(llm_in$GBF, "target b", "goal b")
llm_in$GBF <- str_replace(llm_in$GBF, "target c", "goal c")
llm_in$GBF <- str_replace(llm_in$GBF, "target d", "goal d")
llm_in$"LLM Results" <- substr(llm_in$"LLM Results", str_locate(llm_in$"LLM Results", " #T")[, 2], nchar(llm_in$"LLM Results"))
llm_in$"GBF Text" <- substr(llm_in$"GBF Text", str_locate(llm_in$"GBF Text", ": ")[, 2], nchar(llm_in$"GBF Text"))

llm_id <- llm_id %>% rename("NBT" = "NBT Name/Title", "NBT Text" = "NBT Text - English", "GBF" = "GBF Target...7", "GBF Text" = "GBF Target Text", "LLM Scores" = "Score", "LLM Results" = "generated_sample (full results)", "LLM Description" = "Description") %>% select(-"GBF Target...4")
llm_id$GBF <- substr(llm_id$GBF, 5, nchar(llm_id$GBF))
llm_id$GBF <- str_replace(llm_id$GBF, "target a", "goal a")
llm_id$GBF <- str_replace(llm_id$GBF, "target b", "goal b")
llm_id$GBF <- str_replace(llm_id$GBF, "target c", "goal c")
llm_id$GBF <- str_replace(llm_id$GBF, "target d", "goal d")
llm_id$"LLM Results" <- substr(llm_id$"LLM Results", str_locate(llm_id$"LLM Results", " #T")[, 2], nchar(llm_id$"LLM Results"))
llm_id$"GBF Text" <- substr(llm_id$"GBF Text", str_locate(llm_id$"GBF Text", ": ")[, 2], nchar(llm_id$"GBF Text"))

llm_ml <- llm_ml %>% rename("NBT" = "NBT Name/Title", "NBT Text" = "NBT Text - English", "GBF" = "GBF Target...7", "GBF Text" = "GBF Target Text", "LLM Scores" = "Score", "LLM Results" = "generated_sample (full results)", "LLM Description" = "Description") %>% select(-"GBF Target...4")
llm_ml$GBF <- substr(llm_ml$GBF, 5, nchar(llm_ml$GBF))
llm_ml$GBF <- str_replace(llm_ml$GBF, "target a", "goal a")
llm_ml$GBF <- str_replace(llm_ml$GBF, "target b", "goal b")
llm_ml$GBF <- str_replace(llm_ml$GBF, "target c", "goal c")
llm_ml$GBF <- str_replace(llm_ml$GBF, "target d", "goal d")
llm_ml$"LLM Results" <- substr(llm_ml$"LLM Results", str_locate(llm_ml$"LLM Results", " #T")[, 2], nchar(llm_ml$"LLM Results"))
llm_ml$"GBF Text" <- substr(llm_ml$"GBF Text", str_locate(llm_ml$"GBF Text", ": ")[, 2], nchar(llm_ml$"GBF Text"))

llm_pn <- llm_pn %>% rename("NBT" = "NBT Name/Title", "NBT Text" = "NBT Text - English", "GBF" = "GBF Target...7", "GBF Text" = "GBF Target Text", "LLM Scores" = "Score", "LLM Results" = "generated_sample (full results)", "LLM Description" = "Description") %>% select(-"GBF Target...4")
llm_pn$GBF <- substr(llm_pn$GBF, 5, nchar(llm_pn$GBF))
llm_pn$GBF <- str_replace(llm_pn$GBF, "target a", "goal a")
llm_pn$GBF <- str_replace(llm_pn$GBF, "target b", "goal b")
llm_pn$GBF <- str_replace(llm_pn$GBF, "target c", "goal c")
llm_pn$GBF <- str_replace(llm_pn$GBF, "target d", "goal d")
llm_pn$"LLM Results" <- substr(llm_pn$"LLM Results", str_locate(llm_pn$"LLM Results", " #T")[, 2], nchar(llm_pn$"LLM Results"))
llm_pn$"GBF Text" <- substr(llm_pn$"GBF Text", str_locate(llm_pn$"GBF Text", ": ")[, 2], nchar(llm_pn$"GBF Text"))

llm_sr <- llm_sr %>% rename("NBT" = "NBT Name/Title", "NBT Text" = "NBT Text - English", "GBF" = "GBF Target...7", "GBF Text" = "GBF Target Text", "LLM Scores" = "Score", "LLM Results" = "generated_sample (full results)", "LLM Description" = "Description") %>% select(-"GBF Target...4")
llm_sr$GBF <- substr(llm_sr$GBF, 5, nchar(llm_sr$GBF))
llm_sr$GBF <- str_replace(llm_sr$GBF, "target a", "goal a")
llm_sr$GBF <- str_replace(llm_sr$GBF, "target b", "goal b")
llm_sr$GBF <- str_replace(llm_sr$GBF, "target c", "goal c")
llm_sr$GBF <- str_replace(llm_sr$GBF, "target d", "goal d")
llm_sr$"LLM Results" <- substr(llm_sr$"LLM Results", str_locate(llm_sr$"LLM Results", " #T")[, 2], nchar(llm_sr$"LLM Results"))
llm_sr$"GBF Text" <- substr(llm_sr$"GBF Text", str_locate(llm_sr$"GBF Text", ": ")[, 2], nchar(llm_sr$"GBF Text"))

llm_cr <- llm_cr %>% rename("NBT" = "NBT Name/Title", "NBT Text" = "NBT Text - English", "GBF" = "GBF Target...7", "GBF Text" = "GBF Target Text", "LLM Scores" = "Score", "LLM Results" = "generated_sample (full results)", "LLM Description" = "Description") %>% select(-"GBF Target...4")
llm_cr$GBF <- substr(llm_cr$GBF, 5, nchar(llm_cr$GBF))
llm_cr$GBF <- str_replace(llm_cr$GBF, "target a", "goal a")
llm_cr$GBF <- str_replace(llm_cr$GBF, "target b", "goal b")
llm_cr$GBF <- str_replace(llm_cr$GBF, "target c", "goal c")
llm_cr$GBF <- str_replace(llm_cr$GBF, "target d", "goal d")
llm_cr$"LLM Results" <- substr(llm_cr$"LLM Results", str_locate(llm_cr$"LLM Results", " #T")[, 2], nchar(llm_cr$"LLM Results"))
llm_cr$"GBF Text" <- substr(llm_cr$"GBF Text", str_locate(llm_cr$"GBF Text", ": ")[, 2], nchar(llm_cr$"GBF Text"))

#ab <- select(llm_ab, "Country", "NBT", "NBT Text", "GBF", "GBF Text", "LLM Scores", "LLM Results", "LLM Description") %>% left_join(filter(bert, Country == "Antigua & B.") %>% select("NBT", "GBF", "BERT nScores"), by = c("NBT", "GBF"))

#ml <- select(llm_ml, "Country", "NBT", "NBT Text", "GBF", "GBF Text", "LLM Scores", "LLM Results", "LLM Description") %>% left_join(filter(bert, Country == "Maldives") %>% select("NBT", "GBF", "BERT nScores"), by = c("NBT", "GBF"))

#sl <- select(llm_sl, "Country", "NBT", "NBT Text", "GBF", "GBF Text", "LLM Scores", "LLM Results", "LLM Description") %>% left_join(filter(bert, Country == "Sri Lanka") %>% select("NBT", "GBF", "BERT nScores"), by = c("NBT", "GBF"))

cb <- select(llm_cb, "Country", "NBT", "NBT Text", "GBF", "GBF Text", "LLM Scores", "LLM Results", "LLM Description") %>% 
  left_join(filter(bert, Country == "Cambodia") %>% select("NBT", "GBF", "BERT nScores"), by = c("NBT", "GBF"))
table(cb$`LLM Scores`, useNA = "always")

ind <- select(llm_in, "Country", "NBT", "NBT Text", "GBF", "GBF Text", "LLM Scores", "LLM Results", "LLM Description") %>% 
  left_join(filter(bert, Country == "India") %>% select("NBT", "GBF", "BERT nScores"), by = c("NBT", "GBF"))
table(ind$`LLM Scores`, useNA = "always")

ids <- select(llm_id, "Country", "NBT", "NBT Text", "GBF", "GBF Text", "LLM Scores", "LLM Results", "LLM Description") %>% 
  left_join(filter(bert, Country == "Indonesia") %>% select("NBT", "GBF", "BERT nScores"), by = c("NBT", "GBF"))
table(ids$`LLM Scores`, useNA = "always")

ml <- select(llm_ml, "Country", "NBT", "NBT Text", "GBF", "GBF Text", "LLM Scores", "LLM Results", "LLM Description") %>% 
  left_join(filter(bert, Country == "Maldives") %>% select("NBT", "GBF", "BERT nScores"), by = c("NBT", "GBF"))
table(ml$`LLM Scores`, useNA = "always")

png <- select(llm_pn, "Country", "NBT", "NBT Text", "GBF", "GBF Text", "LLM Scores", "LLM Results", "LLM Description") %>% 
  left_join(filter(bert, Country == "Papua New Guinea") %>% select("NBT", "GBF", "BERT nScores"), by = c("NBT", "GBF"))
table(png$`LLM Scores`, useNA = "always")

sr <- select(llm_sr, "Country", "NBT", "NBT Text", "GBF", "GBF Text", "LLM Scores", "LLM Results", "LLM Description") %>% 
  left_join(filter(bert, Country == "Sri Lanka") %>% select("NBT", "GBF", "BERT nScores"), by = c("NBT", "GBF"))
table(sr$`LLM Scores`, useNA = "always")

cr <- select(llm_cr, "Country", "NBT", "NBT Text", "GBF", "GBF Text", "LLM Scores", "LLM Results", "LLM Description") %>% 
  left_join(filter(bert, Country == "Costa Rica") %>% select("NBT", "GBF", "BERT nScores"), by = c("NBT", "GBF"))
table(cr$`LLM Scores`, useNA = "always")

#ctrys <- rbind(ab, ml, sl)
ctrys <- rbind(cb, ind, ids, ml, png, sr, cr)
ctrys$"BERT Category" <- ifelse(ctrys$"BERT nScores" < 100/3, "Lower", ifelse(ctrys$"BERT nScores" > 200/3, "Higher", "Medium"))


write.xlsx(ctrys, "llm_bertt.xlsx", overwrite = TRUE)
View(cr %>% filter(is.na("LLM Scores")))