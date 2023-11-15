## code to prepare `Inscr_Bithynia` dataset goes here

#' Since the conversion of the original Excel file to CRAN-compatible
#' ASCII-data this script does not convert all datings anymore.
#' I apologize for not updating this script, but it seems too irrelevant
#' to spent more time on it than I already have, since the package
#' contains the clean Data already. Keep this as a reminder that
#' data is complicated, and that things change.

library(dplyr)
library(stringr)
library(forcats)

inscriptions <- read.csv("inst/extdata/Bithynia_Inscriptions_ascii.csv")

inscriptions <- inscriptions %>%
  mutate(ikey = na_if(ikey, "N / A"),
         ikey = na_if(ikey, ""),
         ikey = gsub("2PH", "PH", ikey),
         ikey = gsub("v", "PH", ikey),
         URL = NA)

repl <- grep("HD", inscriptions$ikey)
inscriptions$URL[repl] <- paste("https://edh-www.adw.uni-heidelberg.de/edh/inschrift/",
                                gsub("HD", "", inscriptions$ikey[repl]),
                                sep = "")
repl <- grep("PH", inscriptions$ikey)
inscriptions$URL[repl] <- paste("https://epigraphy.packhum.org/text/",
                                gsub("PH", "", inscriptions$ikey[repl]),
                                sep = "")

inscriptions$ID <- paste("I_", seq_len(nrow(inscriptions)), sep = "")
inscriptions <- inscriptions %>%
  rename(Dating = Chronological.Frame) %>%
  mutate(Language = replace(Language, Language == "Gr/Lat", "Greek/Latin"),
         Language = replace(Language, Language == "Gr / Lat", "Greek/Latin"),
         Language = factor(Language, levels = c("Greek", "Latin",
                                                "Greek/Latin")),
         Location = replace(Location, str_detect(Location, "unknown"),
                            "unknown"),
         Location = replace(Location,
                            Location == "Prusias ad Mare (Keramed)",
                            "Prusias ad Mare"),
         Location = factor(Location),
         Dating = na_if(Dating, "---"))

inscriptions$uncertain_dating <- FALSE
sel <- grep("\\?", inscriptions$Dating)
inscriptions$uncertain_dating[sel] <- TRUE
inscriptions$Dating <- gsub("\\?", "", inscriptions$Dating)

sel <- grepl("[0-9]", inscriptions$Dating)
periods <- data.frame("Dating" = unique(inscriptions$Dating[which(sel == FALSE)]))
periods$DAT_min <- NA
periods$DAT_max <- NA
#write.csv(periods, file = "periods.csv", fileEncoding = "UTF-8")
# .... Manual editing of the resulting table, saving it as "periods_edit.csv".
join_dating <- read.csv(file = system.file('extdata', 'periods_edit.csv',
                                           package = 'datplot',
                                           mustWork = TRUE),
                        row.names = 1,
                        colClasses = c("character", "character",
                                       "integer", "integer"),
                        encoding = "UTF-8")

num_dating <- data.frame("Dating" = unique(inscriptions$Dating[which(sel == TRUE)]))
num_dating$DAT_min <- NA
num_dating$DAT_max <- NA

sel <- grep("^[0-9]{1,3} AD$", num_dating$Dating)
num_dating$DAT_min[sel] <- gsub(" AD", "", num_dating$Dating[sel])
num_dating$DAT_max[sel] <- gsub(" AD", "", num_dating$Dating[sel])
sel <- grep("^[0-9]{1,3} BC$", num_dating$Dating)
num_dating$DAT_min[sel] <- paste("-", gsub(" BC", "", num_dating$Dating[sel]),
                                 sep = "")
num_dating$DAT_max[sel] <- paste("-", gsub(" BC", "", num_dating$Dating[sel]),
                                 sep = "")

join_dating <- rbind(join_dating, num_dating[!is.na(num_dating$DAT_min), ])
num_dating <- num_dating[which(is.na(num_dating$DAT_min)), ]

num_dating$Dating <- as.character(num_dating$Dating)


# Values like: 92-120 AD
sel <- grep("^[0-9]{1,3}-[0-9]{1,3} AD", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = "-| ")
  num_dating$DAT_min[r] <- split[[1]][1]
  num_dating$DAT_max[r] <- split[[1]][2]
}
# Values like: AD 92-120
sel <- grep("^AD [0-9]{1,3}-[0-9]{1,3}$", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = "-| ")
  num_dating$DAT_min[r] <- split[[1]][2]
  num_dating$DAT_max[r] <- split[[1]][3]
}
# Values like: AD 92-120
sel <- grep("^AD [0-9]{1,3}-[0-9]{1,3}$", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = "-| ")
  num_dating$DAT_min[r] <- split[[1]][2]
  num_dating$DAT_max[r] <- split[[1]][3]
}
# Values like: AD 92 - 120
sel <- grep("^AD [0-9]{1,3} - [0-9]{1,3}", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = " - | ")
  num_dating$DAT_min[r] <- split[[1]][2]
  num_dating$DAT_max[r] <- split[[1]][3]
}
# Values like: 198/199 AD
sel <- grep("^[0-9]{1,3}/[0-9]{1,3} AD", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = "/| ")
  num_dating$DAT_min[r] <- split[[1]][1]
  num_dating$DAT_max[r] <- split[[1]][2]
}
# Values like: 525-75 BC
sel <- grep("^[0-9]{1,3}-[0-9]{1,3} BC", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = "-| ")
  num_dating$DAT_min[r] <- 0 - as.numeric(split[[1]][1])
  num_dating$DAT_max[r] <- 0 - as.numeric(split[[1]][2])
}

join_dating <- rbind(join_dating, num_dating[!is.na(num_dating$DAT_min), ])
num_dating <- num_dating[which(is.na(num_dating$DAT_min)), ]

sel <- grep("^[0-9]{1}[a-z]{2} c\\. AD$", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = "[a-z]{2} c\\.")
  split <- as.numeric(split[[1]][1])
  num_dating$DAT_min[r] <- ((split - 1) * 100)
  num_dating$DAT_max[r] <- ((split - 1) * 100) + 99
}

sel <- grep("^[0-9]{1}[a-z]{2} c\\. BC$", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = "[a-z]{2} c\\.")
  split <- as.numeric(split[[1]][1])
  num_dating$DAT_min[r] <- 0 - (split * 100) + 1
  num_dating$DAT_max[r] <- 0 - ((split - 1) * 100)
}

join_dating <- rbind(join_dating, num_dating[!is.na(num_dating$DAT_min), ])
num_dating <- num_dating[which(is.na(num_dating$DAT_min)), ]

sel <- grep("^ca\\. [0-9]{1,3} AD$", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = " ")
  split <- as.numeric(split[[1]][2])
  num_dating$DAT_min[r] <- split - 10
  num_dating$DAT_max[r] <- split + 10
}
sel <- grep("^ca\\. [0-9]{1,3} BC$", num_dating$Dating)
for (r in sel) {
  split <- strsplit(x = num_dating$Dating[r], split = " ")
  split <- 0 - as.numeric(split[[1]][2])
  num_dating$DAT_min[r] <- split - 10
  num_dating$DAT_max[r] <- split + 10
}


join_dating <- rbind(join_dating, num_dating[!is.na(num_dating$DAT_min), ])
num_dating <- num_dating[which(is.na(num_dating$DAT_min)), ]
#unique(num_dating$Dating)[1:20]

join_dating$DAT_min[which(join_dating$DAT_min == 0)] <- 1
join_dating$DAT_max[which(join_dating$DAT_max == 0)] <- -1



#write.csv(num_dating, file = "num_dating.csv", fileEncoding = "UTF-8")
num_dating <- read.csv(file = system.file('extdata', 'num_dating_edit.csv',
                                          package = 'datplot',
                                          mustWork = TRUE),
                       encoding = "UTF-8",
                       row.names = 1,
                       colClasses = c("character", "character",
                                      "integer", "integer"))


join_dating <- join_dating %>%
  mutate(DAT_min = as.integer(DAT_min),
         DAT_max = as.integer(DAT_max)) %>%
  rbind(num_dating)


inscriptions <- left_join(inscriptions, join_dating, by = "Dating")


# Manual error correction
inscriptions[which(inscriptions$ID == "I_1162"), "DAT_max"] <- 63
inscriptions[which(inscriptions$ID == "I_2725"), c("DAT_min", "DAT_max")] <-
  inscriptions[which(inscriptions$ID == "I_2725"), c("DAT_max", "DAT_min")]



inscriptions <- inscriptions[, c("ID", "ikey", "Location", "Source", "Dating",
                                 "Language", "uncertain_dating",
                                 "DAT_min", "DAT_max", "URL")]





attr(inscriptions, "contact") <-
  "Barbora Weissova (Barbora.Weissova@ruhr-uni-bochum.de),
Lisa Steinmann (lisa.steinmann@rub.de)"
attr(inscriptions, "time_created") <- Sys.Date()
attr(inscriptions, "source") <-
  "Data: https://inscriptions.packhum.org/ and
  B. Weissova, Regional Economy,
  Settlement Patterns and the Road System in Bithynia
  (4th century BC - 6th century AD) (Diss. FU Berlin 2019)"
attr(inscriptions, "source_repo") <- "https://github.com/lsteinmann/datplot"
attr(inscriptions$ikey, "descr") <- "ID at https://inscriptions.packhum.org/"
attr(inscriptions$Location, "descr") <- "Findspot"
attr(inscriptions$Source, "descr") <- "Corpus/Citation of the Inscription"
attr(inscriptions$Dating, "descr") <- "Original Chronological Assesment as chr"
attr(inscriptions$Language, "descr") <- "Language of the Inscription"
attr(inscriptions$uncertain_dating, "descr") <-
  "TRUE if Dating is not certain, FALSE if dating is certain"
attr(inscriptions$DAT_min, "descr") <- "lower border of the dating timespan"
attr(inscriptions$DAT_max, "descr") <- "uppper border of the dating timespan"
attr(inscriptions$URL, "descr") <- "Link to the Inscription at
https://inscriptions.packhum.org/"

#write.table(inscriptions, file = "inscriptions.csv",
#            fileEncoding = "UTF-8", sep = ";", row.names = FALSE)


Inscr_Bithynia <- inscriptions

#inscriptions[which(inscriptions$DAT_min == 0),c(1,2,4,5,8,9)]
#inscriptions[which(inscriptions$DAT_max == 0),c(1,2,4,5,8,9)]

usethis::use_data(Inscr_Bithynia, overwrite = TRUE)

