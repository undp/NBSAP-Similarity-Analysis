# Packages ---------------------------------------------------------------------
pacman::p_load("tidyverse", "knitr", "stringr", "readr", "openxlsx", "writexl", "magrittr")

# Directory --------------------------------------------------------------------
setwd("./global")

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
  assign(paste0("df", i), data_list[[i]])
  df <- rbind(df, get(paste0("df", i)))
  dname <- paste0("df", i)
  rm(list = dname)
}
rm(i, csv_files, dname, data_list)

df <- df %>% 
  filter(index != "index") %>% 
  select(-c("Unnamed..3", "index.1")) %>% 
  mutate(Country = trimws(gsub("\\(.*?\\)", "", Country)))

# NBTs -------------------------------------------------------------------------
#df <- df %>% mutate(tst = grepl("[^A-Za-z0-9 ]", NBT.Name.Title))
df$NBT.Name.Title <- ifelse((df$Country == "Belize" | df$Country == "South Africa" | df$Country == "Guyana" | df$Country == "Vanuatu"), 
                            gsub("\\(.*?\\)", "", df$NBT.Name.Title), df$NBT.Name.Title) # removes text in and including parenthesis
df$NBT.Name.Title <- ifelse((df$Country == "Haiti" | df$Country == "Comoros" | df$Country == "Liberia"), 
                            gsub("[#:]", "", df$NBT.Name.Title), df$NBT.Name.Title) # removes special characters "#" and ":"
df$NBT.Name.Title <- ifelse(df$Country == "Tunisia",
                            gsub("DACTION", "D'ACTION", df$NBT.Name.Title), df$NBT.Name.Title) # add '
df$NBT.Name.Title <- ifelse(df$Country == "Argentina", 
                            toupper(df$NBT.Name.Title), df$NBT.Name.Title) # one of the "META" is lowercaps; corrects this
df$NBT.Name.Title <- ifelse((df$Country == "Mozambique" & substr(df$NBT.Name.Title, 8, 9) == "11"), 
                            paste0(substr(df$NBT.Name.Title, 1, 9), substr(df$NBT.Name.Title, 11, nchar(as.character(df$NBT.Name.Title)))), df$NBT.Name.Title) # removes spacing between number and letter id elements
df$NBT.Name.Title <- ifelse((df$Country == "Australia" & substr(df$NBT.Name.Title, 1, 1) == "O"), 
                            substr(df$NBT.Name.Title, 1, nchar(as.character(df$NBT.Name.Title))-3), df$NBT.Name.Title) # removes "G#"
df$NBT.Name.Title <- ifelse((df$Country == "Maldives" & substr(df$NBT.Name.Title, 1, 2) != "St"), 
                            substr(df$NBT.Name.Title, 4, nchar(as.character(df$NBT.Name.Title))), df$NBT.Name.Title) # removes "S#"
df$NBT.Name.Title <- ifelse((df$Country == "Niue" | df$Country == "Thailand"),  
                            gsub("[()]", "", df$NBT.Name.Title), df$NBT.Name.Title) # removes parenthesis
df$NBT.Name.Title <- ifelse(df$Country == "Niue",  
                            gsub("Objective ", "Objective", gsub("Fauna", "p1", gsub("Flora", "p2", df$NBT.Name.Title))), df$NBT.Name.Title) # 
df$NBT.Name.Title <- ifelse((df$Country == "Niue" & substr(df$NBT.Name.Title, 1, 1) == "O"), 
                            trimws(paste0(substr(df$NBT.Name.Title, 11, 14), " ", substr(df$NBT.Name.Title, 1, 10), substr(df$NBT.Name.Title, 15, 17))), df$NBT.Name.Title) #
df$NBT.Name.Title <- ifelse(df$Country == "Venezuela", gsub("Estratgica", "Estratégica", df$NBT.Name.Title), df$NBT.Name.Title)
df$NBT.Name.Title <- ifelse(df$Country == "Bolivia", gsub("ESTRATGICO", "ESTRATÉGICO", df$NBT.Name.Title), df$NBT.Name.Title)
df$NBT.Name.Title <- ifelse(df$Country == "Algeria", gsub("stratgique", "stratégique", df$NBT.Name.Title), df$NBT.Name.Title)
df$NBT.Name.Title <- ifelse(df$Country == "Tuvalu", gsub("Area", "area", df$NBT.Name.Title), df$NBT.Name.Title)
df$NBT.Name.Title <- ifelse(df$Country == "Niger", gsub("Stratégique", "stratégique", df$NBT.Name.Title), df$NBT.Name.Title)
df$NBT.Name.Title <- trimws(gsub("\\s+", " ", df$NBT.Name.Title)) # eliminates leading/lagging and extra spaces

df$nbt_n <- str_extract(trimws(df$NBT.Name.Title), "[^ ]+$")
df$nbt_t <- substr(df$NBT.Name.Title, 1, str_locate(df$NBT.Name.Title, df$nbt_n)-1)

df1 <- df %>% filter(Country == "Australia") %>% 
  mutate(nbt_t = ifelse(substr(NBT.Name.Title, 1, 1) == "G", NBT.Name.Title, NA)) %>% fill(nbt_t) %>% 
  mutate(nbt_t = ifelse(substr(NBT.Name.Title, 1, 1) == "O", paste0(nbt_t, " ", NBT.Name.Title), nbt_t),
         nbt_n = gsub("[^0-9.]", "", nbt_t), 
         nbt_n = gsub("(.)", "\\1.", nbt_n, perl=TRUE), 
         nbt_n = substr(nbt_n, 1, nchar(as.character(nbt_n))-1)) #
df2 <- df %>% filter(Country == "Vanuatu" | Country == "Maldives" | Country == "Sri Lanka") %>% 
  mutate(nbt_t = ifelse(substr(NBT.Name.Title, 1, 1) == "S", NBT.Name.Title, NA)) %>% fill(nbt_t) %>% 
  mutate(nbt_t = ifelse(substr(NBT.Name.Title, 1, 1) == "T", paste0(nbt_t, " ", NBT.Name.Title), nbt_t), 
         nbt_n = gsub("[^0-9.]", "", nbt_t), 
         nbt_n = gsub("(.)", "\\1.", nbt_n, perl=TRUE)) #
df3 <- df %>% filter(Country == "South Sudan") %>% 
  mutate(nbt_t = ifelse(substr(NBT.Name.Title, 1, 1) == "S", NBT.Name.Title, NA)) %>% fill(nbt_t) %>% 
  mutate(nbt_t = ifelse(substr(NBT.Name.Title, 1, 1) != "S", paste0(nbt_t, " ", NBT.Name.Title), nbt_t), 
         nbt_n = gsub("[^0-9.]", "", nbt_t), 
         nbt_n = ifelse(nchar(as.character(nbt_n)) > 1, paste0(substr(nbt_n, 1, 1), ".", substr(nbt_n, 2, nchar(as.character(nbt_n)))), nbt_n)) #
df <- df %>% filter(Country != "Australia" & Country != "Vanuatu" & Country != "Maldives" & Country != "Sri Lanka" & Country != "South Sudan")
df <- rbind(df, df1, df2, df3)
rm(df1, df2, df3)

df$nbt_t <- ifelse(df$Country == "Sudan", substr(df$nbt_t, 8, nchar(as.character(df$nbt_t))), df$nbt_t) # removes "Target"
df$nbt_t <- ifelse(df$Country == "Thailand", gsub("by 20", "", df$NBT.Name.Title), df$nbt_t) # removes part of the text
df$nbt_n <- ifelse(df$Country == "Thailand", gsub("[^0-9.]", "", df$nbt_t), df$nbt_n) # 
df$nbt_n <- ifelse(df$Country == "Thailand", paste0(substr(df$nbt_n, 1, nchar(as.character(df$nbt_n))-2), "(", substr(df$nbt_n, nchar(as.character(df$nbt_n))-1, nchar(as.character(df$nbt_n))), ")"), df$nbt_n) # 
df$nbt_t <- ifelse(df$Country == "Thailand", "Target", df$nbt_t) # 
df$nbt_n <- ifelse(df$Country == "Colombia", paste0(df$nbt_n, "(", substr(df$nbt_t, 9, 10), ")"), df$nbt_n) # isolates the year in the NBT name
df$nbt_t <- ifelse(df$Country == "Colombia", "Meta", df$nbt_t) # isolates the NBT term
df$nbt_n <- ifelse(df$Country == "Costa Rica", substr(df$nbt_n, 3, nchar(as.character(df$nbt_n))-1), df$nbt_n) # removes the "M."
df$nbt_t <- ifelse((df$Country == "Solomon Islands" & substr(df$nbt_n, 1, 1) == "M"), df$nbt_n, df$nbt_t) # ensures propper naming of the NBT
df$nbt_n <- ifelse((df$Country == "Solomon Islands" & substr(df$nbt_n, 1, 1) == "M"), "M", df$nbt_n) # ensures propper naming of the NBT
df$nbt_t <- ifelse((df$Country == "Belize" & df$NBT.Name.Title == "GOAL A"), "GOAL", df$nbt_t) # corrects a weird bug
df$nbt_t <- ifelse((df$Country == "Comoros" & df$NBT.Name.Title == "But stratégique B"), "But stratégique ", df$nbt_t) # corrects a weird bug
df$nbt_t <- ifelse((df$Country == "Montenegro" & substr(df$NBT.Name.Title, 1, 9) == "STRATEGIC"), "STRATEGIC TARGET", df$nbt_t) # corrects a weird bug
df$nbt_t <- ifelse((df$Country == "Mozambique" & substr(df$NBT.Name.Title, 1, 9) == "STRATEGIC"), "STRATEGIC OBJECTIVE", df$nbt_t) # corrects a weird bug
df$nbt_t <- ifelse((df$Country == "Australia" | df$Country == "Vanuatu" | df$Country == "Maldives" | df$Country == "Sri Lanka" | df$Country == "South Sudan"), gsub("\\d", "", df$NBT.Name.Title), df$nbt_t)
df$nbt_t <- ifelse(df$Country == "Sudan", "Target", df$nbt_t) # removes "Target"

df$nbt_t <- trimws(df$nbt_t) # eliminates leading/lagging white spaces
df$nbt_n <- trimws(df$nbt_n) # eliminates leading/lagging white spaces

df$nbt_n <- ifelse(df$Country == "Niue", gsub("[^0-9.]", "", df$NBT.Name.Title), df$nbt_n)
df$nbt_t <- ifelse(df$Country == "Niue", gsub("[^a-zA-Z ]", "", df$NBT.Name.Title), df$nbt_t)
df$nbt_n <- ifelse((df$Country == "Niue" & nchar(as.character(df$nbt_n)) > 1), paste0(substr(df$nbt_n, 1, 1), ".", substr(df$nbt_n, 2, nchar(as.character(df$nbt_n)))), df$nbt_n)
df$nbt_n <- ifelse((df$Country == "Niue" & nchar(as.character(df$nbt_n)) > 3), paste0(substr(df$nbt_n, 1, 3), ".", substr(df$nbt_n, nchar(as.character(df$nbt_n)), nchar(as.character(df$nbt_n)))), df$nbt_n)
df$nbt_t <- ifelse((df$Country == "Niue" & df$nbt_t != "THEME "), "Objective", df$nbt_t)

df_aux <- df %>% 
  group_by(Country) %>% 
  summarise(rep = n_distinct(nbt_t))
df <- df %>% left_join(df_aux)
rm(df_aux)

ctry <- c("Belize", "Bahamas", "Angola", "Armenia", "Comoros", "DRC", "Fiji", "Liberia", "Micronesia", "Moldova", "Montenegro", "Mozambique", 
          "Palau", "Qatar", "Solomon Islands", "South Africa", "Lao People's Democratic Republic", "Kyrgyzstan", "Saint Lucia", "Tunisia", 
         "Algeria", "Cuba", "Ecuador", "Australia", "Vanuatu", "Maldives", "Sri Lanka", "South Sudan", "Niue", "Suriname")
df$nbt_ttl <- ifelse((df$rep == 1 | df$Country %in% ctry), 
                     df$nbt_ttl <- df$nbt_n, 
                     df$nbt_ttl <- paste0(substr(df$nbt_t, 1, 1), df$nbt_n))
df <- df %>% select(-rep)
#dff <- df %>% select(Country, NBT.Name.Title, nbt_t, nbt_n, nbt_ttl) %>% unique()

# Removes non-UNEP/UNDP countres
df <- df %>% 
  filter(Country != "Afghanistan" & Country != "Andorra" & Country != "Australia" & Country !=  "Austria")

# Separation -------------------------------------------------------------------
dfs <- split(df , f = df$Country)
nms <- names(dfs)
nms <- str_replace(nms, " ", "_")

# Random Sampling --------------------------------------------------------------
pacman::p_load("pwr", "statmod", "rsample", "caret")
options(scipen = 999)

df$rand <- sample(100, size = nrow(df), replace = TRUE)
df$scr <- substr(df$generated_sample, 2, 6)
df$cat <- ifelse(df$scr == "0.000", "None", 
                 ifelse(df$scr == "0.250", "Low", 
                        ifelse(df$scr == "0.500", "Medium", "High")))
df$cat <- factor(df$cat, unique(df$cat))
table(df$cat)/dim(df)[1] # None (94%), Low (4%), Medium (1.5%), High (0.5%)

#pwr.t.test(n = NULL, d = 0.5, sig.level = 0.05, power = 0.8, alternative = "two.sided", type = "two.sample")
pwr <- pwr.chisq.test(w = 0.1, # Cohen's w: has a formula depending on the expected proportion of FP, FN, TP and TN... or general rules of thumb: 0.1 (low), 0.3 (medium) or 0.5 (large)
               N = NULL, # what we are looking for
               df = (2-1)*(2-1), # (num.cols - 1)*(num.rows - 1)
               power = 0.8, # generally 0.7-0.8%
               sig.level = 0.05) # generally 5-10%
# min. sample size 785

#smpl <- downSample(x = select(df, -cat), y = df$cat)
#table(smpl$Class)
#ovun.sample(cat~., data = df, method = "under", N = pwr$N/dim(df)[1]) # undersampling (counts the number of minority samples in the dataset, then randomly selects the same number from the majority sample) or oversampling (repeatedly duplicates randomly selected minority classes until there are an equal number of majority and minority samples)
#smpl <- training(initial_split(df, prop = 1.5*pwr$N/dim(df)[1], strata = "cat"))

smpl <- df %>% 
  group_by(cat) %>% 
  sample_n(size = min(n(), pwr$N/3)) %>% 
  ungroup()

table(smpl$cat)/dim(smpl)[1]
dim(smpl)[1]/dim(df)[1] #1.3%

write.csv(smpl, file = paste0("../UNDP_LLM_Assessment_sample_enterprise_19Jun24.csv"), fileEncoding = "UTF-8")

# Saving -----------------------------------------------------------------------
for (df in names(dfs)) write.csv(dfs[[df]], file = paste0("../UNDP_LLM_Assessment_", df, "_enterprise_17Apr24.csv"))
#write.csv(dfs[[2]], file = paste0("../UNDP_LLM_Assessment_", unique(dfs[[2]]$Country), "_enterprise_24May24.csv"))



