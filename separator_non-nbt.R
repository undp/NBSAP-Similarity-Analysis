# Packages ---------------------------------------------------------------------
pacman::p_load("tidyverse", "knitr", "stringr", "readr", "openxlsx", "writexl", "magrittr")

# Directory --------------------------------------------------------------------
setwd("./original")

# Files ------------------------------------------------------------------------
csv_files <- list.files(pattern = "\\.csv$")
#data_list <- list()
#for (file in csv_files) {  data_list[[file]] <- read.csv(file)}
data_list <- lapply(csv_files, read.csv)

# Adjustments ------------------------------------------------------------------
df <- data.frame("X"=integer(), "Country"=character(), "NBT.Name.Title"=character(), 
                 "NBT.Text...English"=character(), "GBF.Target"=character(), 
                 "GBF.Target.Text"=character(), "generated_sample"=character(), 
                 "Similarity_Score"=numeric(), "Description"=character())
for (i in 1:length(data_list)){
  if (unique(data_list[[i]]$Country) == "Guyana") {
    data_list[[i]] <- data_list[[i]] %>% mutate("Unnamed..3" = NA, "index.1" = NA) %>% select(colnames(df))
  } else if (unique(data_list[[i]]$Country) == "Mexico Actions") {
    data_list[[i]] <- data_list[[i]] %>% mutate("Unnamed..3" = NA, "index.1" = NA) %>% rename(index = X) %>% select(colnames(df))
  } else if ((unique(data_list[[i]]$Country) == "Kazakhstan extra" & str_detect(csv_files[i], "N99") == TRUE)) {
    data_list[[i]]$Country <- "KazakhstanN99"
  } else if (unique(data_list[[i]]$Country) == "Lebanon extra") {
    data_list[[i]] <- data_list[[i]] %>% select(-"Unnamed..4") %>% select(colnames(df))
  }
  assign(paste0("df", i), data_list[[i]])
  df <- rbind(df, get(paste0("df", i)))
  dname <- paste0("df", i)
  rm(list = dname)
}
rm(i, dname, data_list)

df <- df %>% 
  filter(index != "index") %>% 
  select(-c("Unnamed..3", "index.1")) %>% 
  mutate(Country = trimws(gsub("\\(.*?\\)", "", Country)))

# NBTs -------------------------------------------------------------------------
#df <- df %>% mutate(tst = grepl("[^A-Za-z0-9 ]", NBT.Name.Title))
df$Country <- ifelse(str_detect(df$Country, "extra|Actions"), substr(df$Country, 1, str_locate(df$Country, "\\ ")-1), df$Country)

df$NBT.Name.Title <- gsub("Actioo", "Action", df$NBT.Name.Title)

df$nbt_n <- ifelse(df$Country == "Kazakhstan3RD", 
                   substr(df$NBT.Name.Title, str_locate(df$NBT.Name.Title, "\\ ")+1, nchar(as.character(df$NBT.Name.Title))), 
                   as.character(trimws(gsub("[^0-9.]", "", df$NBT.Name.Title))))
df$nbt_t <- ifelse(df$Country == "Kazakhstan3RD", 
                   substr(df$NBT.Name.Title, 1, str_locate(df$NBT.Name.Title, "\\ ")-1), 
                   trimws(gsub("[^a-zA-Z ]", "", df$NBT.Name.Title)))


df$NBT.Text...English <- ifelse((df$Country == "Mexico" & substr(df$NBT.Text...English, 1, 5) == "2.3.4"), 
                                paste0(substr(df$NBT.Text...English, 1, 5), ".", substr(df$NBT.Text...English, 6, nchar(as.character(df$NBT.Text...English)))), 
                                df$NBT.Text...English)
df$NBT.Text...English <- ifelse(df$Country == "Mexico", trimws(substr(df$NBT.Text...English, str_locate(df$NBT.Text...English, "\\. ")[, "end"], nchar(as.character(df$NBT.Text...English)))), df$NBT.Text...English)

df_aux <- df %>% 
  group_by(Country) %>% 
  summarise(rep = n_distinct(nbt_t)) # Check here it indeed there are T different nbt terms
df <- df %>% left_join(df_aux)
rm(df_aux)

df$nbt_ttl <- ifelse((df$rep == 1 | df$Country == "Lebanon"), 
                     df$nbt_ttl <- as.character(df$nbt_n), 
                     df$nbt_ttl <- paste0(substr(df$nbt_t, 1, 1), df$nbt_n))
df <- df %>% select(-rep)

#dff <- df %>% select(Country, NBT.Name.Title, NBT.Text...English, nbt_t, nbt_n, nbt_ttl) %>% unique()
#dff <- df %>% select(Country, NBT.Text...English) %>% unique()

# Separation -------------------------------------------------------------------
dfs <- split(df , f = df$Country)
#dfs <- dfs[c(1, 2, 3, 4, 6, 5)]

# Saving -----------------------------------------------------------------------
#for (j in 1:length(dfs)) {write.csv(dfs[[j]], file = paste0("../", csv_files[j]))}
write.csv(dfs[[3]], file = paste0("../", csv_files[3]),fileEncoding = "UTF-8")

