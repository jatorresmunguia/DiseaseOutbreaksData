###################################################### 
################## ENLIGHT PROJECT ###################
###################################################### 

## A.1 Urban and environmental variables explaining 
## the frequency of pandemics 

### Working directory ###
# setwd(choose.dir())
setwd(dir = "ENLIGHT/Data/")

### Packages ###
if(!require("stringr")) install.packages("stringr")


################# Information on outbreaks ################# 
## Database on disease outbreaks from the WHO ##
## https://www.who.int/emergencies/disease-outbreak-news ##

# months 
months <- c("january", "february", "march", "april", "may", "june", "july", 
            "august", "september", "october", "november", "december")

# empty dataset
data <- data.frame("Outbreak" = NA,
                   "Date" = NA,
                   "Year" = NA,
                   "Month" = NA,
                   "Country" = NA,
                   "Description" = NA,
                   "Link" = NA)

### Reading the web page ###
for(page in 1:132){
  webpage <- readLines(paste0("https://www.who.int/emergencies/disease-outbreak-news/", page)) # Link to site

linkslines <- grep("<a class=\"sf-list-vertical__item\" href=", webpage) # This is the text of the line with the information
links <- webpage[linkslines]

for(l in 1:length(links)){
newslink <- str_match(links[l], "href=\"\\s*(.*?)\\s*\">")[, 2] # link to the DON (extended)
webpagenewslink <- readLines(newslink)

newsdatelines <- grep("<span class=\"timestamp\">", webpagenewslink) # This is the text of the line with the data
newsdate <- as.data.frame(tolower(webpagenewslink[newsdatelines]))
newsdate <- newsdate[!is.na(str_extract(newsdate[,1], na.omit(str_extract(newsdate, months))[1])), ]
newsdate <- str_match(newsdate, "<span class=\"timestamp\">\\s*(.*?)\\s*</span>")[2] # Date full
year <- c(1996:2021)[1996:2021 %in% str_split(newsdate, " ")[[1]]] # only year
month <- c(months)[months %in% str_split(newsdate, " ")[[1]]] # onlym month

newsoutbreaklines <- grep("<li class=\"active\">", webpagenewslink)
newsoutbreak <- webpagenewslink[newsoutbreaklines]
newsoutbreak <- str_match(newsoutbreak, "<li class=\"active\">\\s*(.*?)\\s*</li>")[2]

newsoutbreak <- gsub(newsoutbreak, pattern = " – ", replacement = " - ")

# From 1996:2013 there is a different format than in 2014:2021
if (year %in% c(1996:2013)) {
  outbreak <- strsplit(newsoutbreak, " in ")[[1]][1] # outbreak
  country <- strsplit(newsoutbreak, " in ")[[1]][2] # country
} else {
  outbreak <- strsplit(newsoutbreak, " - ")[[1]][1] # outbreak
  country <- strsplit(newsoutbreak, " - ")[[1]][2] # country
}

row <- nrow(data)+1
data[row, "Outbreak"] <- outbreak
data[row, "Date"] <- newsdate
data[row, "Year"] <- year
data[row, "Month"] <- month
data[row, "Country"] <- country
data[row, "Description"] <- newsoutbreak
data[row, "Link"] <- newslink

}
}

data1 <- data[!duplicated(data), ] # Deleting duplicated cases
data2 <- data1[2:nrow(data1), ] # First line is empty

# 2019 is not complete
# We take the information from the previous website
for(i in 2019){ # 
  webpage <- readLines(paste0("https://www.who.int/csr/don/archive/year/", i, "/en/")) # Link to site
  results <- grep("</span><!-- title -->", webpage) # "</span><!-- title -->" This is the text pattern to find
  monthscode <- grep(paste0(i, "</a>"), webpage)[1:length(results)] # in the page the code for the line with the date is this
  
  # Create an empty data base for the year #
  data <- data.frame("Outbreak" = rep(NA, each = length(results)),
                     "Date" = rep(i, each = length(results)),
                     "Year" = rep(i, each = length(results)),
                     "Month" = rep(NA, each = length(results)),
                     "Country" = rep(NA, each = length(results)),
                     "Description" = rep(NA, each = length(results)),
                     "Link" = rep(NA, each = length(results)))
  
  for(j in 1:length(results)){ # Total number of outbreaks in the year 
    text <- webpage[results[j]] # Specific outbreak
    text <- gsub("\t\t\t<br><span class=\"link_info\">", "", text) # Clean code
    text <- gsub("</span><!-- title -->", "", text) # Clean code
    monthtext <- webpage[monthscode[j]]
    newsdate <- tolower(monthtext)
    newsdate <- str_match(newsdate, "\\\">\\s*(.*?)\\s*</a>")[2] # Date full
    linkfull <- str_match(monthtext, "href=\\\"\\s*(.*?)\\s*\">")[2] # Link full
    data[j, "Date"] <- newsdate # Only text corresponding to full data
    data[j, "Month"] <- na.omit(as.data.frame(str_extract(tolower(monthtext), months)))[1, 1]
    data[j, "Description"] <- text # All description
    data[j, "Outbreak"] <- strsplit(text, "[–]")[[1]][1] # Only text corresponding to Outbreak
    data[j, "Country"] <- strsplit(text, "[–]")[[1]][2] # Only text corresponding to Country Name
    data[j, "Link"] <- paste0("https://www.who.int", linkfull) # Only text corresponding to full data
  }
  
  data$Outbreak <- trimws(gsub(data$Outbreak, pattern = paste0(i, " - "), replacement = ""))
  data$Country <- trimws(gsub(data$Country, pattern = "\\(new outbreak\\)", replacement = ""))
  data <- data[!grepl(pattern = "pdate", data$Description), ]
  rownames(data) <- 1:nrow(data)
  assign(paste0("data", i), data)
} # 119 DON´s related to 2019

### Extract all years except 2019 from data2
data2 <- data2[data2$Year != 2019, ] # 2637-2580 = 57
data2 <- rbind(data2, data2019) # 2699 DON's

### Add information on month, day ###
months <- as.data.frame(months)
months$month_num <- 1:12
colnames(months)[1] <- "Month" 

data2 <- merge(data2, months, by = "Month", all.x = TRUE)
data2[, "Day"] <- NA

for(i in 1:nrow(data2)){
  day <- str_match(data2[i, "Date"], paste0("^s*(.*?)\\s*", data2[i, "Month"]))[2]
  data2[i, "Day"] <- as.numeric(day)
}

# Ordering data by date #
data2 <- data2[order(data2$Year, data2$month_num, data2$Day, data2$Link, data2$Description), ]

rownames(data2) <- 1:nrow(data2)

# ID to identify each DON #
data2$ID <- paste0("DON", str_pad(rownames(data2), 4, pad = "0")) # assigning an ID to each report. Oldest one is DON0001

DONs <- data2[, c("ID", "Description", "Date", "Link")]
save(DONs, file = "DONs.RData") ## Saving all DON's (raw data)

### Information on diseases ###
data2$Disease <- data2$Outbreak

data2$icd10c <- NA
data2$icd103c <- NA
data2$icd104c <- NA

data2$icd10n <- NA
data2$icd103n <- NA
data2$icd104n <- NA

## First the information on 2019 given that it
data2[nrow(data2)+1, ] <- data2[data2$Outbreak == "Circulating vaccine-derived poliovirus type 2" & data2$Date == "29 november 2019", ]
data2[nrow(data2)+1, ] <- data2[data2$Outbreak == "Circulating vaccine-derived poliovirus type 2" & data2$Date == "29 november 2019", ]
data2[nrow(data2)+1, ] <- data2[data2$Outbreak == "Circulating vaccine-derived poliovirus type 2" & data2$Date == "29 november 2019", ]

data2[nrow(data2)+1, ] <- data2[data2$Outbreak == "Circulating vaccine-derived poliovirus type 2" & data2$Date == "29 november 2019", ]
data2[nrow(data2)+1, ] <- data2[data2$Outbreak == "Circulating vaccine-derived poliovirus type 2" & data2$Date == "29 november 2019", ]

data2[(nrow(data2)-2):nrow(data2), "Country"] <- c("Niger", "Cameroon", "Nigeria")

data2[nrow(data2)+1, ] <- data2[data2$Outbreak == "Circulating vaccine-derived poliovirus type 2" & data2$Date == "29 november 2019", ]
data2[nrow(data2)+1, ] <- data2[data2$Outbreak == "Circulating vaccine-derived poliovirus type 2" & data2$Date == "29 november 2019", ]
data2[nrow(data2)+1, ] <- data2[data2$Outbreak == "Circulating vaccine-derived poliovirus type 2" & data2$Date == "29 november 2019", ]

data2[(nrow(data2)-2):nrow(data2), "Country"] <- c("Congo Democratic Republic of the", "Angola", "Central African Republic")

data2[nrow(data2)+1, ] <- data2[data2$Outbreak == "Circulating vaccine-derived poliovirus type 2" & data2$Date == "29 november 2019", ]
data2[nrow(data2)+1, ] <- data2[data2$Outbreak == "Circulating vaccine-derived poliovirus type 2" & data2$Date == "29 november 2019", ]

data2[(nrow(data2)-1):nrow(data2), "Country"] <- c("Somalia", "Kenya")

data2[data2$Country == "Argentine Republic" & data2$Year == 2019 & !is.na(data2$Country), "Country"] <- "Argentina"
data2[data2$Country == "Bolivarian Republic of Venezuela" & data2$Year == 2019 & !is.na(data2$Country), "Country"] <- "Venezuela (Bolivarian Republic of)"

data2[data2$Country == "Democratic Republic of the Congo" & data2$Year == 2019 & !is.na(data2$Country), "Country"] <- "Congo Democratic Republic of the"

data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – European Region" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – European Region" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – European Region" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – European Region" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – European Region" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – European Region" & data2$Year == 2019, ]

data2[data2$Description == "Measles – European Region" & data2$Year == 2019, "Country"] <- c("Ukraine", "Serbia", "Israel",
                                                                                             "France", "Russian Federation", "Georgia", "Greece")


data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]

data2[(nrow(data2)-4):nrow(data2), "Country"] <- c("Madagascar", "Nigeria", "Congo Democratic Republic of the", 
                                                   "Guinea", "Chad")

data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]

data2[(nrow(data2)-5):nrow(data2), "Country"] <- c("Yemen", "Sudan", "Somalia", 
                                                   "Pakistan", "Tunisia", "Iraq") 

data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]

data2[(nrow(data2)-2):nrow(data2), "Country"] <- c("Kazakhstan", "Kyrgyzstan", "Turkey") 

data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]

data2[(nrow(data2)-3):nrow(data2), "Country"] <- c("Brazil", "Venezuela (Bolivarian Republic of)", "Colombia", "United States of America") 

data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]

data2[(nrow(data2)-2):nrow(data2), "Country"] <- c("Bangladesh", "Myanmar", "Thailand") 

data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]
data2[nrow(data2)+1, ] <- data2[data2$Description == "Measles – Global situation" & data2$Year == 2019, ]

data2[data2$Country == "Global situation" & data2$Year == 2019 & !is.na(data2$Country), "Country"] <- c("Philippines", "Viet Nam", "Cambodia",
                                                                                                        "New Zealand", "Tonga", "Fiji", "Samoa") 

data2[data2$Country == "Islamic Republic of Iran" & data2$Year == 2019 & !is.na(data2$Country), "Country"] <- "Iran (Islamic Republic of)"

data2[nrow(data2)+1, ] <- data2[grepl(data2$Description, pattern = "Island Countries and Areas"), ]
data2[nrow(data2)+1, ] <- data2[grepl(data2$Description, pattern = "Island Countries and Areas"), ]

data2[grepl(data2$Description, pattern = "Island Countries and Areas"), "Country"] <- c("Tonga", "Fiji", "Samoa")

data2[nrow(data2)+1, ] <- data2[grepl(x = data2$Description, pattern = "Western Pacific Region"), ]
data2[nrow(data2)+1, ] <- data2[grepl(x = data2$Description, pattern = "Western Pacific Region"), ]
data2[nrow(data2)+1, ] <- data2[grepl(x = data2$Description, pattern = "Western Pacific Region"), ]
data2[nrow(data2)+1, ] <- data2[grepl(x = data2$Description, pattern = "Western Pacific Region"), ]
data2[nrow(data2)+1, ] <- data2[grepl(x = data2$Description, pattern = "Western Pacific Region"), ]
data2[nrow(data2)+1, ] <- data2[grepl(x = data2$Description, pattern = "Western Pacific Region"), ]
data2[nrow(data2)+1, ] <- data2[grepl(x = data2$Description, pattern = "Western Pacific Region"), ]
data2[nrow(data2)+1, ] <- data2[grepl(x = data2$Description, pattern = "Western Pacific Region"), ]

data2[grepl(x = data2$Description, pattern = "Western Pacific Region"), "Country"] <- c("Australia", "China", "Hong Kong", "Macao",
                                                                                        "Japan", "Malaysia", "New Zealand", "Philippines", 
                                                                                        "Korea Republic of") 

##### Setting homogeneous names to Outbreaks ##### 
data2[grepl(pattern = "cholera", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "cholera", x = tolower(data2$Outbreak)), "icd10n"] <- "Intestinal infectious diseases"
data2[grepl(pattern = "cholera", x = tolower(data2$Outbreak)), "icd103n"] <- "Cholera"
data2[grepl(pattern = "cholera", x = tolower(data2$Outbreak)), "icd104n"] <- "Classical cholera"
data2[grepl(pattern = "cholera", x = tolower(data2$Outbreak)), "icd10c"] <- "A00-A09"
data2[grepl(pattern = "cholera", x = tolower(data2$Outbreak)), "icd103c"] <- "A00"
data2[grepl(pattern = "cholera", x = tolower(data2$Outbreak)), "icd104c"] <- "A000"

data2[grepl(pattern = "food-bo", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "food-bo", x = tolower(data2$Outbreak)), "icd10n"] <- "Intestinal infectious diseases"
data2[grepl(pattern = "food-bo", x = tolower(data2$Outbreak)), "icd103n"] <- "Other bacterial foodborne intoxications, not elsewhere classified"
data2[grepl(pattern = "food-bo", x = tolower(data2$Outbreak)), "icd104n"] <- "Bacterial foodborne intoxication, unspecified"
data2[grepl(pattern = "food-bo", x = tolower(data2$Outbreak)), "icd10c"] <- "A00-A09"
data2[grepl(pattern = "food-bo", x = tolower(data2$Outbreak)), "icd103c"] <- "A05"
data2[grepl(pattern = "food-bo", x = tolower(data2$Outbreak)), "icd104c"] <- "A059"

data2[grepl(pattern = "taphylococcal", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "taphylococcal", x = tolower(data2$Outbreak)), "icd10n"] <- "Intestinal infectious diseases"
data2[grepl(pattern = "taphylococcal", x = tolower(data2$Outbreak)), "icd103n"] <- "Other bacterial foodborne intoxications, not elsewhere classified"
data2[grepl(pattern = "taphylococcal", x = tolower(data2$Outbreak)), "icd104n"] <- "Foodborne staphylococcal intoxication"
data2[grepl(pattern = "taphylococcal", x = tolower(data2$Outbreak)), "icd10c"] <- "A00-A09"
data2[grepl(pattern = "taphylococcal", x = tolower(data2$Outbreak)), "icd103c"] <- "A05"
data2[grepl(pattern = "taphylococcal", x = tolower(data2$Outbreak)), "icd104c"] <- "A050"

data2[grepl(pattern = "diarrhoea", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "diarrhoea", x = tolower(data2$Outbreak)), "icd10n"] <-"Intestinal infectious diseases"
data2[grepl(pattern = "diarrhoea", x = tolower(data2$Outbreak)), "icd103n"] <-"Other gastroenteritis and colitis of infectious and unspecified origin"
data2[grepl(pattern = "diarrhoea", x = tolower(data2$Outbreak)), "icd104n"] <- "Other and unspecified gastroenteritis and colitis of infectious origin"
data2[grepl(pattern = "diarrhoea", x = tolower(data2$Outbreak)), "icd10c"] <- "A00-A09"
data2[grepl(pattern = "diarrhoea", x = tolower(data2$Outbreak)), "icd103c"] <- "A09"
data2[grepl(pattern = "diarrhoea", x = tolower(data2$Outbreak)), "icd104c"] <- "A090"

## Some news report more than one outbreak diarrhoeal and cholera ##
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Severe Acute Watery Diarrhoea with V. cholerae positive cases", x = data2$Outbreak), ]
data2[nrow(data2), "icd103n"] <- "Cholera"
data2[nrow(data2), "icd104n"] <- "Classical cholera"
data2[nrow(data2), "icd103c"] <- "A00"
data2[nrow(data2), "icd104c"] <- "A000"

data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Diarrhoeal disease/cholera", x = data2$Outbreak), ]
data2[nrow(data2), "icd103n"] <- "Cholera"
data2[nrow(data2), "icd104n"] <- "Classical cholera"
data2[nrow(data2), "icd103c"] <- "A00"
data2[nrow(data2), "icd104c"] <- "A000"

data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Cholera/acute diarrhoea", x = data2$Outbreak), ]
data2[nrow(data2), "icd103n"] <- "Cholera"
data2[nrow(data2), "icd104n"] <- "Classical cholera"
data2[nrow(data2), "icd103c"] <- "A00"
data2[nrow(data2), "icd104c"] <- "A000"

data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Cholera/diarrhoea outbreak", x = data2$Outbreak), ]
data2[nrow(data2), "icd103n"] <- "Cholera"
data2[nrow(data2), "icd104n"] <- "Classical cholera"
data2[nrow(data2), "icd103c"] <- "A00"
data2[nrow(data2), "icd104c"] <- "A000"

data2[grepl(pattern = "lassa", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "lassa", x = tolower(data2$Outbreak)), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "lassa", x = tolower(data2$Outbreak)), "icd103n"] <-"Arenaviral haemorrhagic fever"
data2[grepl(pattern = "lassa", x = tolower(data2$Outbreak)), "icd104n"] <- "Lassa fever"
data2[grepl(pattern = "lassa", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "lassa", x = tolower(data2$Outbreak)), "icd103c"] <- "A96"
data2[grepl(pattern = "lassa", x = tolower(data2$Outbreak)), "icd104c"] <- "A962"

data2[grepl(pattern = "cute haemorrhagic", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "cute haemorrhagic", x = tolower(data2$Outbreak)), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "cute haemorrhagic", x = tolower(data2$Outbreak)), "icd103n"] <- "Unspecified viral haemorrhagic fever"
data2[grepl(pattern = "cute haemorrhagic", x = tolower(data2$Outbreak)), "icd104n"] <- "Unspecified viral haemorrhagic fever"
data2[grepl(pattern = "cute haemorrhagic", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "cute haemorrhagic", x = tolower(data2$Outbreak)), "icd103c"] <- "A99"
data2[grepl(pattern = "cute haemorrhagic", x = tolower(data2$Outbreak)), "icd104c"] <- "A990"
### https://www.who.int/csr/resources/publications/surveillance/whocdscsrisr992syn.pdf?ua=1#:~:text=Acute%20haemorrhagic%20fever%20syndromes%20can,a%20potential%20to%20produce%20epidemics.###

data2[grepl(pattern = "haemorr", x = tolower(data2$Outbreak)) & is.na(data2$icd103c), "Outbreak"]
data2[grepl(pattern = "haemorr", x = tolower(data2$Outbreak)) & is.na(data2$icd103c), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "haemorr", x = tolower(data2$Outbreak)) & is.na(data2$icd103c), "icd103n"] <- "Unspecified viral haemorrhagic fever"
data2[grepl(pattern = "haemorr", x = tolower(data2$Outbreak)) & is.na(data2$icd103c), "icd104n"] <- "Unspecified viral haemorrhagic fever"
data2[grepl(pattern = "haemorr", x = tolower(data2$Outbreak)) & is.na(data2$icd103c), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "haemorr", x = tolower(data2$Outbreak)) & is.na(data2$icd103c), "icd103c"] <- "A99"
data2[grepl(pattern = "haemorr", x = tolower(data2$Outbreak)) & is.na(data2$icd104c), "icd104c"] <- "A990"

data2[grepl(pattern = "fever with renal syndrome", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "fever with renal syndrome", x = tolower(data2$Outbreak)), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "fever with renal syndrome", x = tolower(data2$Outbreak)), "icd103n"] <- "Other viral haemorrhagic fevers, not elsewhere classified"
data2[grepl(pattern = "fever with renal syndrome", x = tolower(data2$Outbreak)), "icd104n"] <- "Haemorrhagic fever with renal syndrome"
data2[grepl(pattern = "fever with renal syndrome", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "fever with renal syndrome", x = tolower(data2$Outbreak)), "icd103c"] <- "A98"
data2[grepl(pattern = "fever with renal syndrome", x = tolower(data2$Outbreak)), "icd104c"] <- "A985"

data2[grepl(pattern = "acute respira", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "acute respira", x = tolower(data2$Outbreak)), "icd10n"] <- "Codes for special purposes"
data2[grepl(pattern = "acute respira", x = tolower(data2$Outbreak)), "icd103n"] <-"Provisional assignment of new diseases of uncertain etiology or emergency use"
data2[grepl(pattern = "acute respira", x = tolower(data2$Outbreak)), "icd104n"] <- "Severe acute respiratory syndrome [SARS]"
data2[grepl(pattern = "acute respira", x = tolower(data2$Outbreak)), "icd10c"] <- "U00-U49"
data2[grepl(pattern = "acute respira", x = tolower(data2$Outbreak)), "icd103c"] <- "U04"
data2[grepl(pattern = "acute respira", x = tolower(data2$Outbreak)), "icd104c"] <- "U049"

data2[grepl(pattern = "sars", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "sars", x = tolower(data2$Outbreak)), "icd10n"] <- "Codes for special purposes"
data2[grepl(pattern = "sars", x = tolower(data2$Outbreak)), "icd103n"] <-"Provisional assignment of new diseases of uncertain etiology or emergency use"
data2[grepl(pattern = "sars", x = tolower(data2$Outbreak)), "icd104n"] <- "Severe acute respiratory syndrome [SARS]"
data2[grepl(pattern = "sars", x = tolower(data2$Outbreak)), "icd10c"] <- "U00-U49"
data2[grepl(pattern = "sars", x = tolower(data2$Outbreak)), "icd103c"] <- "U04"
data2[grepl(pattern = "sars", x = tolower(data2$Outbreak)), "icd104c"] <- "U049"

data2[grepl(pattern = "sars", x = tolower(data2$Outbreak)) & data2$Year > 2019, ]
data2[grepl(pattern = "sars", x = tolower(data2$Outbreak)) & data2$Year > 2019, "icd10n"] <- "Provisional assignment of new diseases of uncertain etiology or emergency use"
data2[grepl(pattern = "sars", x = tolower(data2$Outbreak)) & data2$Year > 2019, "icd103n"] <- "Emergency use of U07"
data2[grepl(pattern = "sars", x = tolower(data2$Outbreak)) & data2$Year > 2019, "icd104n"] <- "COVID-19, virus identified"
data2[grepl(pattern = "sars", x = tolower(data2$Outbreak)) & data2$Year > 2019, "icd10c"] <- "U00-U49"
data2[grepl(pattern = "sars", x = tolower(data2$Outbreak)) & data2$Year > 2019, "icd103c"] <- "U07"
data2[grepl(pattern = "sars", x = tolower(data2$Outbreak)) & data2$Year > 2019, "icd104c"] <- "U071"

data2[grepl(pattern = "nexplained cluster", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "nexplained cluster", x = tolower(data2$Outbreak)), "icd10n"] <- "Other infectious diseases"
data2[grepl(pattern = "nexplained cluster", x = tolower(data2$Outbreak)), "icd103n"] <-"Other and unspecified infectious diseases"
data2[grepl(pattern = "nexplained cluster", x = tolower(data2$Outbreak)), "icd104n"] <- "Other and unspecified infectious diseases"
data2[grepl(pattern = "nexplained cluster", x = tolower(data2$Outbreak)), "icd10c"] <- "B99-B99"
data2[grepl(pattern = "nexplained cluster", x = tolower(data2$Outbreak)), "icd103c"] <- "B99"
data2[grepl(pattern = "nexplained cluster", x = tolower(data2$Outbreak)), "icd104c"] <- "B990"

data2[grepl(pattern = "cute febrile", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "cute febrile", x = tolower(data2$Outbreak)), "icd10n"] <- "Other infectious diseases"
data2[grepl(pattern = "cute febrile", x = tolower(data2$Outbreak)), "icd103n"] <-"Other and unspecified infectious diseases"
data2[grepl(pattern = "cute febrile", x = tolower(data2$Outbreak)), "icd104n"] <- "Other and unspecified infectious diseases"
data2[grepl(pattern = "cute febrile", x = tolower(data2$Outbreak)), "icd10c"] <- "B99-B99"
data2[grepl(pattern = "cute febrile", x = tolower(data2$Outbreak)), "icd103c"] <- "B99"
data2[grepl(pattern = "cute febrile", x = tolower(data2$Outbreak)), "icd104c"] <- "B990"

data2[grepl(pattern = "streptococcus", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "streptococcus", x = tolower(data2$Outbreak)), "icd10n"] <- "Other bacterial diseases"
data2[grepl(pattern = "streptococcus", x = tolower(data2$Outbreak)), "icd103n"] <-"Streptococcal sepsis"
data2[grepl(pattern = "streptococcus", x = tolower(data2$Outbreak)), "icd104n"] <- "Streptococcal sepsis, unspecified"
data2[grepl(pattern = "streptococcus", x = tolower(data2$Outbreak)), "icd10c"] <- "A30-A49"
data2[grepl(pattern = "streptococcus", x = tolower(data2$Outbreak)), "icd103c"] <- "A40"
data2[grepl(pattern = "streptococcus", x = tolower(data2$Outbreak)), "icd104c"] <- "A409"

data2[grepl(pattern = "pertus", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "pertus", x = tolower(data2$Outbreak)), "icd10n"] <- "Other bacterial diseases"
data2[grepl(pattern = "pertus", x = tolower(data2$Outbreak)), "icd103n"] <-"Whooping cough"
data2[grepl(pattern = "pertus", x = tolower(data2$Outbreak)), "icd104n"] <- "Whooping cough due to Bordetella pertussis"
data2[grepl(pattern = "pertus", x = tolower(data2$Outbreak)), "icd10c"] <- "A30-A49"
data2[grepl(pattern = "pertus", x = tolower(data2$Outbreak)), "icd103c"] <- "A37"
data2[grepl(pattern = "pertus", x = tolower(data2$Outbreak)), "icd104c"] <- "A370"

data2[grepl(pattern = "haemolytic uraemic", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "haemolytic uraemic", x = tolower(data2$Outbreak)), "icd10n"] <- "Haemolytic anaemias"
data2[grepl(pattern = "haemolytic uraemic", x = tolower(data2$Outbreak)), "icd103n"] <-"Acquired haemolytic anaemia"
data2[grepl(pattern = "haemolytic uraemic", x = tolower(data2$Outbreak)), "icd104n"] <- "Haemolytic-uraemic syndrome"
data2[grepl(pattern = "haemolytic uraemic", x = tolower(data2$Outbreak)), "icd10c"] <- "D55-D59"
data2[grepl(pattern = "haemolytic uraemic", x = tolower(data2$Outbreak)), "icd103c"] <- "D59"
data2[grepl(pattern = "haemolytic uraemic", x = tolower(data2$Outbreak)), "icd104c"] <- "D593"

data2[grepl(pattern = "ak of illness", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "ak of illness", x = tolower(data2$Outbreak)), "icd10n"] <- "Other infectious diseases"
data2[grepl(pattern = "ak of illness", x = tolower(data2$Outbreak)), "icd103n"] <-"Other and unspecified infectious diseases"
data2[grepl(pattern = "ak of illness", x = tolower(data2$Outbreak)), "icd104n"] <- "Other and unspecified infectious diseases"
data2[grepl(pattern = "ak of illness", x = tolower(data2$Outbreak)), "icd10c"] <- "B99-B99"
data2[grepl(pattern = "ak of illness", x = tolower(data2$Outbreak)), "icd103c"] <- "B99"
data2[grepl(pattern = "ak of illness", x = tolower(data2$Outbreak)), "icd104c"] <- "B990"

data2[grepl(pattern = "salmonell", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "salmonell", x = tolower(data2$Outbreak)), "icd10n"] <- "Intestinal infectious diseases"
data2[grepl(pattern = "salmonell", x = tolower(data2$Outbreak)), "icd103n"] <-"Other salmonella infections"
data2[grepl(pattern = "salmonell", x = tolower(data2$Outbreak)), "icd104n"] <- "Salmonella infection, unspecified"
data2[grepl(pattern = "salmonell", x = tolower(data2$Outbreak)), "icd10c"] <- "A00-A09"
data2[grepl(pattern = "salmonell", x = tolower(data2$Outbreak)), "icd103c"] <- "A02"
data2[grepl(pattern = "salmonell", x = tolower(data2$Outbreak)), "icd104c"] <- "A029"

data2[grepl(pattern = "botuli", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "botuli", x = tolower(data2$Outbreak)), "icd10n"] <- "Intestinal infectious diseases"
data2[grepl(pattern = "botuli", x = tolower(data2$Outbreak)), "icd103n"] <-"Other bacterial foodborne intoxications, not elsewhere classified"
data2[grepl(pattern = "botuli", x = tolower(data2$Outbreak)), "icd104n"] <- "Botulism"
data2[grepl(pattern = "botuli", x = tolower(data2$Outbreak)), "icd10c"] <- "A00-A09"
data2[grepl(pattern = "botuli", x = tolower(data2$Outbreak)), "icd103c"] <- "A05"
data2[grepl(pattern = "botuli", x = tolower(data2$Outbreak)), "icd104c"] <- "A051"

data2[grepl(pattern = "valley", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "valley", x = tolower(data2$Outbreak)), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "valley", x = tolower(data2$Outbreak)), "icd103n"] <-"Other mosquito-borne viral fevers"
data2[grepl(pattern = "valley", x = tolower(data2$Outbreak)), "icd104n"] <- "Rift Valley fever"
data2[grepl(pattern = "valley", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "valley", x = tolower(data2$Outbreak)), "icd103c"] <- "A92"
data2[grepl(pattern = "valley", x = tolower(data2$Outbreak)), "icd104c"] <- "A924"

data2[grepl(pattern = "nthrax", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "nthrax", x = tolower(data2$Outbreak)), "icd10n"] <- "Certain zoonotic bacterial diseases"
data2[grepl(pattern = "nthrax", x = tolower(data2$Outbreak)), "icd103n"] <- "Anthrax"
data2[grepl(pattern = "nthrax", x = tolower(data2$Outbreak)), "icd104n"] <- "Anthrax, unspecified"
data2[grepl(pattern = "nthrax", x = tolower(data2$Outbreak)), "icd10c"] <- "A20-A28"
data2[grepl(pattern = "nthrax", x = tolower(data2$Outbreak)), "icd103c"] <- "A22"
data2[grepl(pattern = "nthrax", x = tolower(data2$Outbreak)), "icd104c"] <- "A229"

data2[grepl(pattern = "tular", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "tular", x = tolower(data2$Outbreak)), "icd10n"] <- "Certain zoonotic bacterial diseases"
data2[grepl(pattern = "tular", x = tolower(data2$Outbreak)), "icd103n"] <- "Tularaemia"
data2[grepl(pattern = "tular", x = tolower(data2$Outbreak)), "icd104n"] <- "Tularaemia, unspecified"
data2[grepl(pattern = "tular", x = tolower(data2$Outbreak)), "icd10c"] <- "A20-A28"
data2[grepl(pattern = "tular", x = tolower(data2$Outbreak)), "icd103c"] <- "A21"
data2[grepl(pattern = "tular", x = tolower(data2$Outbreak)), "icd104c"] <- "A219"

data2[grepl(pattern = "polio", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "polio", x = tolower(data2$Outbreak)), "icd10n"] <- "Viral infections of the central nervous system"
data2[grepl(pattern = "polio", x = tolower(data2$Outbreak)), "icd103n"] <- "Acute poliomyelitis"
data2[grepl(pattern = "polio", x = tolower(data2$Outbreak)), "icd104n"] <- "Acute poliomyelitis, unspecified"
data2[grepl(pattern = "polio", x = tolower(data2$Outbreak)), "icd10c"] <- "A80-A89"
data2[grepl(pattern = "polio", x = tolower(data2$Outbreak)), "icd103c"] <- "A80"
data2[grepl(pattern = "polio", x = tolower(data2$Outbreak)), "icd104c"] <- "A809"

data2[grepl(pattern = "^rabie", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "^rabie", x = tolower(data2$Outbreak)), "icd10n"] <- "Viral infections of the central nervous system"
data2[grepl(pattern = "^rabie", x = tolower(data2$Outbreak)), "icd103n"] <- "Rabies"
data2[grepl(pattern = "^rabie", x = tolower(data2$Outbreak)), "icd104n"] <- "Rabies, unspecified"
data2[grepl(pattern = "^rabie", x = tolower(data2$Outbreak)), "icd10c"] <- "A80-A89"
data2[grepl(pattern = "^rabie", x = tolower(data2$Outbreak)), "icd103c"] <- "A82"
data2[grepl(pattern = "^rabie", x = tolower(data2$Outbreak)), "icd104c"] <- "A829"

data2[grepl(pattern = "ebola", x = tolower(data2$Outbreak)), "Outbreak"] 
data2[grepl(pattern = "ebola", x = tolower(data2$Outbreak)), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "ebola", x = tolower(data2$Outbreak)), "icd103n"] <-"Other viral haemorrhagic fevers, not elsewhere classified"
data2[grepl(pattern = "ebola", x = tolower(data2$Outbreak)), "icd104n"] <- "Ebola virus disease"
data2[grepl(pattern = "ebola", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "ebola", x = tolower(data2$Outbreak)), "icd103c"] <- "A98"
data2[grepl(pattern = "ebola", x = tolower(data2$Outbreak)), "icd104c"] <- "A984"

data2[grepl(pattern = "eboal", x = tolower(data2$Outbreak)), "Outbreak"] 
data2[grepl(pattern = "eboal", x = tolower(data2$Outbreak)), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "eboal", x = tolower(data2$Outbreak)), "icd103n"] <-"Other viral haemorrhagic fevers, not elsewhere classified"
data2[grepl(pattern = "eboal", x = tolower(data2$Outbreak)), "icd104n"] <- "Ebola virus disease"
data2[grepl(pattern = "eboal", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "eboal", x = tolower(data2$Outbreak)), "icd103c"] <- "A98"
data2[grepl(pattern = "eboal", x = tolower(data2$Outbreak)), "icd104c"] <- "A984"

data2[grepl(pattern = "plague", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "plague", x = tolower(data2$Outbreak)), "icd10n"] <- "Certain zoonotic bacterial diseases"
data2[grepl(pattern = "plague", x = tolower(data2$Outbreak)), "icd103n"] <- "Plague"
data2[grepl(pattern = "plague", x = tolower(data2$Outbreak)), "icd104n"] <- "Plague, unspecified"
data2[grepl(pattern = "plague", x = tolower(data2$Outbreak)), "icd10c"] <- "A20-A28"
data2[grepl(pattern = "plague", x = tolower(data2$Outbreak)), "icd103c"] <- "A20"
data2[grepl(pattern = "plague", x = tolower(data2$Outbreak)), "icd104c"] <- "A209"

data2[grepl(pattern = "seyche", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "seyche", x = tolower(data2$Outbreak)), "icd10n"] <- "Certain zoonotic bacterial diseases"
data2[grepl(pattern = "seyche", x = tolower(data2$Outbreak)), "icd103n"] <- "Plague"
data2[grepl(pattern = "seyche", x = tolower(data2$Outbreak)), "icd104n"] <- "Plague, unspecified"
data2[grepl(pattern = "seyche", x = tolower(data2$Outbreak)), "icd10c"] <- "A20-A28"
data2[grepl(pattern = "seyche", x = tolower(data2$Outbreak)), "icd103c"] <- "A20"
data2[grepl(pattern = "seyche", x = tolower(data2$Outbreak)), "icd104c"] <- "A209"

data2[grepl(pattern = "bubonic", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "bubonic", x = tolower(data2$Outbreak)), "icd10n"] <- "Certain zoonotic bacterial diseases"
data2[grepl(pattern = "bubonic", x = tolower(data2$Outbreak)), "icd103n"] <- "Plague"
data2[grepl(pattern = "bubonic", x = tolower(data2$Outbreak)), "icd104n"] <- "Bubonic plague"
data2[grepl(pattern = "bubonic", x = tolower(data2$Outbreak)), "icd10c"] <- "A20-A28"
data2[grepl(pattern = "bubonic", x = tolower(data2$Outbreak)), "icd103c"] <- "A20"
data2[grepl(pattern = "bubonic", x = tolower(data2$Outbreak)), "icd104c"] <- "A200"

data2[grepl(pattern = "buffalopox", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "buffalopox", x = tolower(data2$Outbreak)), "icd10n"] <- "Other viral diseases"
data2[grepl(pattern = "buffalopox", x = tolower(data2$Outbreak)), "icd103n"] <- "Other viral diseases, not elsewhere classified"
data2[grepl(pattern = "buffalopox", x = tolower(data2$Outbreak)), "icd104n"] <- "Other specified viral diseases"
data2[grepl(pattern = "buffalopox", x = tolower(data2$Outbreak)), "icd10c"] <- "B25-B34"
data2[grepl(pattern = "buffalopox", x = tolower(data2$Outbreak)), "icd103c"] <- "B33"
data2[grepl(pattern = "buffalopox", x = tolower(data2$Outbreak)), "icd104c"] <- "B338"

data2[grepl(pattern = "influenza", x = tolower(data2$Outbreak)), "Outbreak"] 
data2[grepl(pattern = "influenza", x = tolower(data2$Outbreak)), "icd10n"] <-"Influenza and pneumonia"
data2[grepl(pattern = "influenza", x = tolower(data2$Outbreak)), "icd103n"] <-"Influenza due to identified zoonotic or pandemic influenza virus"
data2[grepl(pattern = "influenza", x = tolower(data2$Outbreak)), "icd104n"] <- "Influenza due to identified zoonotic or pandemic influenza virus"
data2[grepl(pattern = "influenza", x = tolower(data2$Outbreak)), "icd10c"] <- "J09-J18"
data2[grepl(pattern = "influenza", x = tolower(data2$Outbreak)), "icd103c"] <- "J09"
data2[grepl(pattern = "influenza", x = tolower(data2$Outbreak)), "icd104c"] <- "J090"

data2[grepl(pattern = "\\(h1", x = tolower(data2$Outbreak)), "Outbreak"] 
data2[grepl(pattern = "\\(h1", x = tolower(data2$Outbreak)), "icd10n"] <-"Influenza and pneumonia"
data2[grepl(pattern = "\\(h1", x = tolower(data2$Outbreak)), "icd103n"] <-"Influenza due to identified zoonotic or pandemic influenza virus"
data2[grepl(pattern = "\\(h1", x = tolower(data2$Outbreak)), "icd104n"] <- "Influenza due to identified zoonotic or pandemic influenza virus"
data2[grepl(pattern = "\\(h1", x = tolower(data2$Outbreak)), "icd10c"] <- "J09-J18"
data2[grepl(pattern = "\\(h1", x = tolower(data2$Outbreak)), "icd103c"] <- "J09"
data2[grepl(pattern = "\\(h1", x = tolower(data2$Outbreak)), "icd104c"] <- "J090"

data2[grepl(pattern = "pneumo", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "pneumo", x = tolower(data2$Outbreak)), "icd10n"] <-"Influenza and pneumonia"
data2[grepl(pattern = "pneumo", x = tolower(data2$Outbreak)), "icd103n"] <-"Viral pneumonia, not elsewhere classified"
data2[grepl(pattern = "pneumo", x = tolower(data2$Outbreak)), "icd104n"] <- "Viral pneumonia, unspecified"
data2[grepl(pattern = "pneumo", x = tolower(data2$Outbreak)), "icd10c"] <- "J09-J18"
data2[grepl(pattern = "pneumo", x = tolower(data2$Outbreak)), "icd103c"] <- "J12"
data2[grepl(pattern = "pneumo", x = tolower(data2$Outbreak)), "icd104c"] <- "J129"

data2[grepl(pattern = "Malaria", x = data2$Outbreak), "Outbreak"]
data2[grepl(pattern = "Malaria", x = data2$Outbreak), "icd10n"] <- "Protozoal diseases"
data2[grepl(pattern = "Malaria", x = data2$Outbreak), "icd103n"] <- "Unspecified malaria"
data2[grepl(pattern = "Malaria", x = data2$Outbreak), "icd104n"] <- "Unspecified malaria"
data2[grepl(pattern = "Malaria", x = data2$Outbreak), "icd10c"] <- "B50-B64"
data2[grepl(pattern = "Malaria", x = data2$Outbreak), "icd103c"] <- "B54"
data2[grepl(pattern = "Malaria", x = data2$Outbreak), "icd104c"] <- "B540"

## Some news report more than one outbreak Influenza and malaria ##
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza and malaria", x = data2$Outbreak), ]
data2[nrow(data2), "icd10n"] <- "Protozoal diseases"
data2[nrow(data2), "icd103n"] <- "Unspecified malaria"
data2[nrow(data2), "icd104n"] <- "Unspecified malaria"
data2[nrow(data2), "icd10c"] <- "B50-B64"
data2[nrow(data2), "icd103c"] <- "B54"
data2[nrow(data2), "icd104c"] <- "B540"

data2[grepl(pattern = "meningitis", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "meningitis", x = tolower(data2$Outbreak)), "icd10n"] <- "Viral infections of the central nervous system"
data2[grepl(pattern = "meningitis", x = tolower(data2$Outbreak)), "icd103n"] <- "Viral meningitis"
data2[grepl(pattern = "meningitis", x = tolower(data2$Outbreak)), "icd104n"] <- "Viral meningitis, unspecified"
data2[grepl(pattern = "meningitis", x = tolower(data2$Outbreak)), "icd10c"] <- "A80-A89"
data2[grepl(pattern = "meningitis", x = tolower(data2$Outbreak)), "icd103c"] <- "A87"
data2[grepl(pattern = "meningitis", x = tolower(data2$Outbreak)), "icd104c"] <- "A879"

data2[grepl(pattern = "gonococc", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "gonococc", x = tolower(data2$Outbreak)), "icd10n"] <- "Infections with a predominantly sexual mode of transmission"
data2[grepl(pattern = "gonococc", x = tolower(data2$Outbreak)), "icd103n"] <- "Gonococcal infection"
data2[grepl(pattern = "gonococc", x = tolower(data2$Outbreak)), "icd104n"] <- "Gonococcal infection, unspecified"
data2[grepl(pattern = "gonococc", x = tolower(data2$Outbreak)), "icd10c"] <- "A50-A64"
data2[grepl(pattern = "gonococc", x = tolower(data2$Outbreak)), "icd103c"] <- "A54"
data2[grepl(pattern = "gonococc", x = tolower(data2$Outbreak)), "icd104c"] <- "A549"

data2[grepl(pattern = "meningo", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "meningo", x = tolower(data2$Outbreak)), "icd10n"] <- "Other bacterial diseases"
data2[grepl(pattern = "meningo", x = tolower(data2$Outbreak)), "icd103n"] <- "Meningococcal infection"
data2[grepl(pattern = "meningo", x = tolower(data2$Outbreak)), "icd104n"] <- "Meningococcal meningitis"
data2[grepl(pattern = "meningo", x = tolower(data2$Outbreak)), "icd10c"] <- "A30-A49"
data2[grepl(pattern = "meningo", x = tolower(data2$Outbreak)), "icd103c"] <- "A39"
data2[grepl(pattern = "meningo", x = tolower(data2$Outbreak)), "icd104c"] <- "A390"

data2[grepl(pattern = "erebrospinal", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "erebrospinal", x = tolower(data2$Outbreak)), "icd10n"] <- "Other bacterial diseases"
data2[grepl(pattern = "erebrospinal", x = tolower(data2$Outbreak)), "icd103n"] <- "Meningococcal infection"
data2[grepl(pattern = "erebrospinal", x = tolower(data2$Outbreak)), "icd104n"] <- "Meningococcal meningitis"
data2[grepl(pattern = "erebrospinal", x = tolower(data2$Outbreak)), "icd10c"] <- "A30-A49"
data2[grepl(pattern = "erebrospinal", x = tolower(data2$Outbreak)), "icd103c"] <- "A39"
data2[grepl(pattern = "erebrospinal", x = tolower(data2$Outbreak)), "icd104c"] <- "A390"

data2[grepl(pattern = "oropouc", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "oropouc", x = tolower(data2$Outbreak)), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "oropouc", x = tolower(data2$Outbreak)), "icd103n"] <- "Other arthropod-borne viral fevers, not elsewhere classified"
data2[grepl(pattern = "oropouc", x = tolower(data2$Outbreak)), "icd104n"] <- "Oropouche virus disease"
data2[grepl(pattern = "oropouc", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "oropouc", x = tolower(data2$Outbreak)), "icd103c"] <- "A93"
data2[grepl(pattern = "oropouc", x = tolower(data2$Outbreak)), "icd104c"] <- "A930"

data2[grepl(pattern = "hand, foot", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "hand, foot", x = tolower(data2$Outbreak)), "icd10n"] <- "Viral infections characterized by skin and mucous membrane lesions"
data2[grepl(pattern = "hand, foot", x = tolower(data2$Outbreak)), "icd103n"] <- "Other viral infections characterized by skin and mucous membrane lesions, not elsewhere classified"
data2[grepl(pattern = "hand, foot", x = tolower(data2$Outbreak)), "icd104n"] <- "Enteroviral vesicular stomatitis with exanthem"
data2[grepl(pattern = "hand, foot", x = tolower(data2$Outbreak)), "icd10c"] <- "B00-B09"
data2[grepl(pattern = "hand, foot", x = tolower(data2$Outbreak)), "icd103c"] <- "B08"
data2[grepl(pattern = "hand, foot", x = tolower(data2$Outbreak)), "icd104c"] <- "B084"

data2[grepl(pattern = "novel", x = tolower(data2$Outbreak)), ]
data2[grepl(pattern = "novel", x = tolower(data2$Outbreak)), "icd10n"] <- "Provisional assignment of new diseases of uncertain etiology or emergency use"
data2[grepl(pattern = "novel", x = tolower(data2$Outbreak)), "icd103n"] <- "Severe acute respiratory syndrome [SARS]"
data2[grepl(pattern = "novel", x = tolower(data2$Outbreak)), "icd104n"] <- "Middle East respiratory syndrome coronavirus [MERS-CoV]"
data2[grepl(pattern = "novel", x = tolower(data2$Outbreak)), "icd10c"] <- "U00-U49"
data2[grepl(pattern = "novel", x = tolower(data2$Outbreak)), "icd103c"] <- "U04"
data2[grepl(pattern = "novel", x = tolower(data2$Outbreak)), "icd104c"] <- "U040"

data2[grepl(pattern = "novel ", x = tolower(data2$Outbreak)) & data2$Year > 2019, ]
data2[grepl(pattern = "novel ", x = tolower(data2$Outbreak)) & data2$Year > 2019, "icd10n"] <- "Provisional assignment of new diseases of uncertain etiology or emergency use"
data2[grepl(pattern = "novel ", x = tolower(data2$Outbreak)) & data2$Year > 2019, "icd103n"] <- "Emergency use of U07"
data2[grepl(pattern = "novel ", x = tolower(data2$Outbreak)) & data2$Year > 2019, "icd104n"] <- "COVID-19, virus identified"
data2[grepl(pattern = "novel ", x = tolower(data2$Outbreak)) & data2$Year > 2019, "icd10c"] <- "U00-U49"
data2[grepl(pattern = "novel ", x = tolower(data2$Outbreak)) & data2$Year > 2019, "icd103c"] <- "U07"
data2[grepl(pattern = "novel ", x = tolower(data2$Outbreak)) & data2$Year > 2019, "icd104c"] <- "U071"

data2[grepl(pattern = "middle ", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "middle ", x = tolower(data2$Outbreak)), "icd10n"] <- "Provisional assignment of new diseases of uncertain etiology or emergency use"
data2[grepl(pattern = "middle ", x = tolower(data2$Outbreak)), "icd103n"] <- "Severe acute respiratory syndrome [SARS]"
data2[grepl(pattern = "middle ", x = tolower(data2$Outbreak)), "icd104n"] <- "Middle East respiratory syndrome coronavirus [MERS-CoV]"
data2[grepl(pattern = "middle ", x = tolower(data2$Outbreak)), "icd10c"] <- "U00-U49"
data2[grepl(pattern = "middle ", x = tolower(data2$Outbreak)), "icd103c"] <- "U04"
data2[grepl(pattern = "middle ", x = tolower(data2$Outbreak)), "icd104c"] <- "U040"

data2[grepl(pattern = "creu", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "creu", x = tolower(data2$Outbreak)), "icd10n"] <-"Viral infections of the central nervous system"
data2[grepl(pattern = "creu", x = tolower(data2$Outbreak)), "icd103n"] <-"Atypical virus infections of central nervous system"
data2[grepl(pattern = "creu", x = tolower(data2$Outbreak)), "icd104n"] <- "Creutzfeldt-Jakob disease"
data2[grepl(pattern = "creu", x = tolower(data2$Outbreak)), "icd10c"] <- "A80-A89"
data2[grepl(pattern = "creu", x = tolower(data2$Outbreak)), "icd103c"] <- "A81"
data2[grepl(pattern = "creu", x = tolower(data2$Outbreak)), "icd104c"] <- "A810"

data2[grepl(pattern = "crimean", x = tolower(data2$Outbreak)), "Outbreak"] 
data2[grepl(pattern = "crimean", x = tolower(data2$Outbreak)), "icd10n"] <-"Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "crimean", x = tolower(data2$Outbreak)), "icd103n"] <-"Other viral haemorrhagic fevers, not elsewhere classified"
data2[grepl(pattern = "crimean", x = tolower(data2$Outbreak)), "icd104n"] <- "Crimean-Congo haemorrhagic fever"
data2[grepl(pattern = "crimean", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "crimean", x = tolower(data2$Outbreak)), "icd103c"] <- "A98"
data2[grepl(pattern = "crimean", x = tolower(data2$Outbreak)), "icd104c"] <- "A980"

data2[grepl(pattern = "dracun", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "dracun", x = tolower(data2$Outbreak)), "icd10n"] <- "Helminthiases"
data2[grepl(pattern = "dracun", x = tolower(data2$Outbreak)), "icd103n"] <- "Dracunculiasis"
data2[grepl(pattern = "dracun", x = tolower(data2$Outbreak)), "icd104n"] <- "Dracunculiasis"
data2[grepl(pattern = "dracun", x = tolower(data2$Outbreak)), "icd10c"] <- "B65-B83"
data2[grepl(pattern = "dracun", x = tolower(data2$Outbreak)), "icd103c"] <- "B72"
data2[grepl(pattern = "dracun", x = tolower(data2$Outbreak)), "icd104c"] <- "B720"

data2[grepl(pattern = "typhoid", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "typhoid", x = tolower(data2$Outbreak)), "icd10n"] <- "Intestinal infectious diseases"
data2[grepl(pattern = "typhoid", x = tolower(data2$Outbreak)), "icd103n"] <- "Typhoid and paratyphoid fevers"
data2[grepl(pattern = "typhoid", x = tolower(data2$Outbreak)), "icd104n"] <- "Typhoid fever"
data2[grepl(pattern = "typhoid", x = tolower(data2$Outbreak)), "icd10c"] <- "A00-A09"
data2[grepl(pattern = "typhoid", x = tolower(data2$Outbreak)), "icd103c"] <- "A01"
data2[grepl(pattern = "typhoid", x = tolower(data2$Outbreak)), "icd104c"] <- "A010"

data2[grepl(pattern = "dengue", x = tolower(data2$Outbreak)), "Outbreak"] 
data2[grepl(pattern = "dengue", x = tolower(data2$Outbreak)), "icd10n"] <-"Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "dengue", x = tolower(data2$Outbreak)), "icd103n"] <-"Dengue"
data2[grepl(pattern = "dengue", x = tolower(data2$Outbreak)), "icd104n"] <- "Dengue, unspecified"
data2[grepl(pattern = "dengue", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "dengue", x = tolower(data2$Outbreak)), "icd103c"] <- "A97"
data2[grepl(pattern = "dengue", x = tolower(data2$Outbreak)), "icd104c"] <- "A979"

data2[grepl(pattern = "arenav", x = tolower(data2$Outbreak)), "Outbreak"] 
data2[grepl(pattern = "arenav", x = tolower(data2$Outbreak)), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "arenav", x = tolower(data2$Outbreak)), "icd103n"] <- "Arenaviral haemorrhagic fever"
data2[grepl(pattern = "arenav", x = tolower(data2$Outbreak)), "icd104n"] <- "Arenaviral haemorrhagic fever, unspecified"
data2[grepl(pattern = "arenav", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "arenav", x = tolower(data2$Outbreak)), "icd103c"] <- "A96"
data2[grepl(pattern = "arenav", x = tolower(data2$Outbreak)), "icd104c"] <- "A969"

data2[grepl(pattern = "diphtheria", x = tolower(data2$Outbreak)), "Outbreak"] 
data2[grepl(pattern = "diphtheria", x = tolower(data2$Outbreak)), "icd10n"] <-"Other bacterial diseases"
data2[grepl(pattern = "diphtheria", x = tolower(data2$Outbreak)), "icd103n"] <-"Diphtheria"
data2[grepl(pattern = "diphtheria", x = tolower(data2$Outbreak)), "icd104n"] <- "Diphtheria, unspecified"
data2[grepl(pattern = "diphtheria", x = tolower(data2$Outbreak)), "icd10c"] <- "A30-A49"
data2[grepl(pattern = "diphtheria", x = tolower(data2$Outbreak)), "icd103c"] <- "A36"
data2[grepl(pattern = "diphtheria", x = tolower(data2$Outbreak)), "icd104c"] <- "A369"

data2[grepl(pattern = "dysente", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "dysente", x = tolower(data2$Outbreak)), "icd10n"] <- "Intestinal infectious diseases"
data2[grepl(pattern = "dysente", x = tolower(data2$Outbreak)), "icd103n"] <- "Shigellosis"
data2[grepl(pattern = "dysente", x = tolower(data2$Outbreak)), "icd104n"] <- "Shigellosis due to Shigella flexneri"
data2[grepl(pattern = "dysente", x = tolower(data2$Outbreak)), "icd10c"] <- "A00-A09"
data2[grepl(pattern = "dysente", x = tolower(data2$Outbreak)), "icd103c"] <- "A03"
data2[grepl(pattern = "dysente", x = tolower(data2$Outbreak)), "icd104c"] <- "A031"

data2[grepl(pattern = "shige", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "shige", x = tolower(data2$Outbreak)), "icd10n"] <- "Intestinal infectious diseases"
data2[grepl(pattern = "shige", x = tolower(data2$Outbreak)), "icd103n"] <- "Shigellosis"
data2[grepl(pattern = "shige", x = tolower(data2$Outbreak)), "icd104n"] <- "Shigellosis due to Shigella flexneri"
data2[grepl(pattern = "shige", x = tolower(data2$Outbreak)), "icd10c"] <- "A00-A09"
data2[grepl(pattern = "shige", x = tolower(data2$Outbreak)), "icd103c"] <- "A03"
data2[grepl(pattern = "shige", x = tolower(data2$Outbreak)), "icd104c"] <- "A031"

data2[grepl(pattern = "coli", x = tolower(data2$Outbreak)), "Outbreak"] 
data2[grepl(pattern = "coli", x = tolower(data2$Outbreak)), "icd10n"] <-"Bacterial, viral and other infectious agents"
data2[grepl(pattern = "coli", x = tolower(data2$Outbreak)), "icd103n"] <-"Other specified bacterial agents as the cause of diseases classified to other chapters"
data2[grepl(pattern = "coli", x = tolower(data2$Outbreak)), "icd104n"] <- "Escherichia coli [E. coli] as the cause of diseases classified to other chapters"
data2[grepl(pattern = "coli", x = tolower(data2$Outbreak)), "icd10c"] <- "B95-B98"
data2[grepl(pattern = "coli", x = tolower(data2$Outbreak)), "icd103c"] <- "B96"
data2[grepl(pattern = "coli", x = tolower(data2$Outbreak)), "icd104c"] <- "B962"

data2[grepl(pattern = "ehec", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "ehec", x = tolower(data2$Outbreak)), "icd10n"] <-"Bacterial, viral and other infectious agents"
data2[grepl(pattern = "ehec", x = tolower(data2$Outbreak)), "icd103n"] <-"Other specified bacterial agents as the cause of diseases classified to other chapters"
data2[grepl(pattern = "ehec", x = tolower(data2$Outbreak)), "icd104n"] <- "Escherichia coli [E. coli] as the cause of diseases classified to other chapters"
data2[grepl(pattern = "ehec", x = tolower(data2$Outbreak)), "icd10c"] <- "B95-B98"
data2[grepl(pattern = "ehec", x = tolower(data2$Outbreak)), "icd103c"] <- "B96"
data2[grepl(pattern = "ehec", x = tolower(data2$Outbreak)), "icd104c"] <- "B962"

## Some news report more than one outbreak e.coli and Haemolytic-uraemic syndrome ##
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "ehec", x = tolower(data2$Outbreak)), ]
data2[nrow(data2), "icd10n"] <- "Haemolytic anaemias"
data2[nrow(data2), "icd103n"] <- "Acquired haemolytic anaemia"
data2[nrow(data2), "icd104n"] <- "Haemolytic-uraemic syndrome"
data2[nrow(data2), "icd10c"] <- "D55-D59"
data2[nrow(data2), "icd103c"] <- "D59"
data2[nrow(data2), "icd104c"] <- "D593"

data2[grepl(pattern = "legi", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "legi", x = tolower(data2$Outbreak)), "icd10n"] <- "Other bacterial diseases"
data2[grepl(pattern = "legi", x = tolower(data2$Outbreak)), "icd103n"] <- "Other bacterial diseases, not elsewhere classified"
data2[grepl(pattern = "legi", x = tolower(data2$Outbreak)), "icd104n"] <- "Legionnaires disease"
data2[grepl(pattern = "legi", x = tolower(data2$Outbreak)), "icd10c"] <- "A30-A49"
data2[grepl(pattern = "legi", x = tolower(data2$Outbreak)), "icd103c"] <- "A48"
data2[grepl(pattern = "legi", x = tolower(data2$Outbreak)), "icd104c"] <- "A481"

data2[grepl(pattern = "encep", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "encep", x = tolower(data2$Outbreak)), "icd10n"] <- "Viral infections of the central nervous system"
data2[grepl(pattern = "encep", x = tolower(data2$Outbreak)), "icd103n"] <- "Mosquito-borne viral encephalitis"
data2[grepl(pattern = "encep", x = tolower(data2$Outbreak)), "icd104n"] <- "Mosquito-borne viral encephalitis, unspecified"
data2[grepl(pattern = "encep", x = tolower(data2$Outbreak)), "icd10c"] <- "A80-A89"
data2[grepl(pattern = "encep", x = tolower(data2$Outbreak)), "icd103c"] <- "A83"
data2[grepl(pattern = "encep", x = tolower(data2$Outbreak)), "icd104c"] <- "A839"

data2[grepl(pattern = "louis", x = tolower(data2$Outbreak)), "Outbreak"] 
data2[grepl(pattern = "louis", x = tolower(data2$Outbreak)), "icd10n"] <- "Viral infections of the central nervous system"
data2[grepl(pattern = "louis", x = tolower(data2$Outbreak)), "icd103n"] <- "Mosquito-borne viral encephalitis"
data2[grepl(pattern = "louis", x = tolower(data2$Outbreak)), "icd104n"] <- "St Louis encephalitis"
data2[grepl(pattern = "louis", x = tolower(data2$Outbreak)), "icd10c"] <- "A80-A89"
data2[grepl(pattern = "louis", x = tolower(data2$Outbreak)), "icd103c"] <- "A83"
data2[grepl(pattern = "louis", x = tolower(data2$Outbreak)), "icd104c"] <- "A833"

data2[grepl(pattern = "spongiform", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "spongiform", x = tolower(data2$Outbreak)), "icd10n"] <- "Viral infections of the central nervous system"
data2[grepl(pattern = "spongiform", x = tolower(data2$Outbreak)), "icd103n"] <- "Atypical virus infections of central nervous system"
data2[grepl(pattern = "spongiform", x = tolower(data2$Outbreak)), "icd104n"] <- "Creutzfeldt-Jakob disease"
data2[grepl(pattern = "spongiform", x = tolower(data2$Outbreak)), "icd10c"] <- "A80-A89"
data2[grepl(pattern = "spongiform", x = tolower(data2$Outbreak)), "icd103c"] <- "A81"
data2[grepl(pattern = "spongiform", x = tolower(data2$Outbreak)), "icd104c"] <- "A810"

data2[grepl(pattern = "venezuelan", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "venezuelan", x = tolower(data2$Outbreak)), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "venezuelan", x = tolower(data2$Outbreak)), "icd103n"] <- "Other mosquito-borne viral fevers"
data2[grepl(pattern = "venezuelan", x = tolower(data2$Outbreak)), "icd104n"] <- "Venezuelan equine fever"
data2[grepl(pattern = "venezuelan", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "venezuelan", x = tolower(data2$Outbreak)), "icd103c"] <- "A92"
data2[grepl(pattern = "venezuelan", x = tolower(data2$Outbreak)), "icd104c"] <- "A922"

data2[grepl(pattern = "nyong", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "nyong", x = tolower(data2$Outbreak)), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "nyong", x = tolower(data2$Outbreak)), "icd103n"] <- "Other mosquito-borne viral fevers"
data2[grepl(pattern = "nyong", x = tolower(data2$Outbreak)), "icd104n"] <- "O'nyong-nyong fever"
data2[grepl(pattern = "nyong", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "nyong", x = tolower(data2$Outbreak)), "icd103c"] <- "A92"
data2[grepl(pattern = "nyong", x = tolower(data2$Outbreak)), "icd104c"] <- "A921"

data2[grepl(pattern = "hendra", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "hendra", x = tolower(data2$Outbreak)), "icd10n"] <- "Other viral diseases"
data2[grepl(pattern = "hendra", x = tolower(data2$Outbreak)), "icd103n"] <-"Viral infection of unspecified site"
data2[grepl(pattern = "hendra", x = tolower(data2$Outbreak)), "icd104n"] <- "Other viral infections of unspecified site"
data2[grepl(pattern = "hendra", x = tolower(data2$Outbreak)), "icd10c"] <- "B25-B34"
data2[grepl(pattern = "hendra", x = tolower(data2$Outbreak)), "icd103c"] <- "B34"
data2[grepl(pattern = "hendra", x = tolower(data2$Outbreak)), "icd104c"] <- "B348"

data2[grepl(pattern = "panese", x = tolower(data2$Outbreak)), "Outbreak"] 
data2[grepl(pattern = "panese", x = tolower(data2$Outbreak)), "icd10n"] <- "Viral infections of the central nervous system"
data2[grepl(pattern = "panese", x = tolower(data2$Outbreak)), "icd103n"] <- "Mosquito-borne viral encephalitis"
data2[grepl(pattern = "panese", x = tolower(data2$Outbreak)), "icd104n"] <- "Japanese encephalitis"
data2[grepl(pattern = "panese", x = tolower(data2$Outbreak)), "icd10c"] <- "A80-A89"
data2[grepl(pattern = "panese", x = tolower(data2$Outbreak)), "icd103c"] <- "A83"
data2[grepl(pattern = "panese", x = tolower(data2$Outbreak)), "icd104c"] <- "A830"

## Some news report more than one outbreak Janpanese Encephalitis and Hendra-like Viurs ##
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Janpanese Encephalitis", x = data2$Outbreak), ]
data2[nrow(data2), "icd10n"] <- "Other viral diseases"
data2[nrow(data2), "icd103n"] <- "Viral infection of unspecified site"
data2[nrow(data2), "icd104n"] <- "Other viral infections of unspecified site"
data2[nrow(data2), "icd10c"] <- "B25-B34"
data2[nrow(data2), "icd103c"] <- "B34"
data2[nrow(data2), "icd104c"] <- "B348"

data2[grepl(pattern = "pseudomon", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "pseudomon", x = tolower(data2$Outbreak)), "icd10n"] <- "Bacterial, viral and other infectious agents"
data2[grepl(pattern = "pseudomon", x = tolower(data2$Outbreak)), "icd103n"] <- "Other specified bacterial agents as the cause of diseases classified to other chapters"
data2[grepl(pattern = "pseudomon", x = tolower(data2$Outbreak)), "icd104n"] <- "Pseudomonas (aeruginosa) as the cause of diseases classified to other chapters"
data2[grepl(pattern = "pseudomon", x = tolower(data2$Outbreak)), "icd10c"] <- "B95-B98"
data2[grepl(pattern = "pseudomon", x = tolower(data2$Outbreak)), "icd103c"] <- "B96"
data2[grepl(pattern = "pseudomon", x = tolower(data2$Outbreak)), "icd104c"] <- "B965"

data2[grepl(pattern = "enterov", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "enterov", x = tolower(data2$Outbreak)), "icd10n"] <- "Other viral diseases"
data2[grepl(pattern = "enterov", x = tolower(data2$Outbreak)), "icd103n"] <- "Viral infection of unspecified site"
data2[grepl(pattern = "enterov", x = tolower(data2$Outbreak)), "icd104n"] <- "Enterovirus infection, unspecified site"
data2[grepl(pattern = "enterov", x = tolower(data2$Outbreak)), "icd10c"] <- "B25-B34"
data2[grepl(pattern = "enterov", x = tolower(data2$Outbreak)), "icd103c"] <- "B34"
data2[grepl(pattern = "enterov", x = tolower(data2$Outbreak)), "icd104c"] <- "B341"

data2[grepl(pattern = "icrocephaly", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "icrocephaly", x = tolower(data2$Outbreak)), "icd10n"] <- "Congenital malformations of the nervous system"
data2[grepl(pattern = "icrocephaly", x = tolower(data2$Outbreak)), "icd103n"] <- "Microcephaly"
data2[grepl(pattern = "icrocephaly", x = tolower(data2$Outbreak)), "icd104n"] <- "Microcephaly"
data2[grepl(pattern = "icrocephaly", x = tolower(data2$Outbreak)), "icd10c"] <- "Q00-Q07"
data2[grepl(pattern = "icrocephaly", x = tolower(data2$Outbreak)), "icd103c"] <- "Q02"
data2[grepl(pattern = "icrocephaly", x = tolower(data2$Outbreak)), "icd104c"] <- "Q020"

data2[grepl(pattern = "elizabeth", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "elizabeth", x = tolower(data2$Outbreak)), "icd10n"] <- "Other infectious diseases"
data2[grepl(pattern = "elizabeth", x = tolower(data2$Outbreak)), "icd103n"] <- "Other and unspecified infectious diseases"
data2[grepl(pattern = "elizabeth", x = tolower(data2$Outbreak)), "icd104n"] <- "Other and unspecified infectious diseases"
data2[grepl(pattern = "elizabeth", x = tolower(data2$Outbreak)), "icd10c"] <- "B99-B99"
data2[grepl(pattern = "elizabeth", x = tolower(data2$Outbreak)), "icd103c"] <- "B99"
data2[grepl(pattern = "elizabeth", x = tolower(data2$Outbreak)), "icd104c"] <- "B990"

data2[grepl(pattern = "marburg", x = tolower(data2$Outbreak)), "Outbreak"] 
data2[grepl(pattern = "marburg", x = tolower(data2$Outbreak)), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "marburg", x = tolower(data2$Outbreak)), "icd103n"] <-"Other viral haemorrhagic fevers, not elsewhere classified"
data2[grepl(pattern = "marburg", x = tolower(data2$Outbreak)), "icd104n"] <- "Marburg virus disease"
data2[grepl(pattern = "marburg", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "marburg", x = tolower(data2$Outbreak)), "icd103c"] <- "A98"
data2[grepl(pattern = "marburg", x = tolower(data2$Outbreak)), "icd104c"] <- "A983"

data2[grepl(pattern = "mayaro", x = tolower(data2$Outbreak)), "Outbreak"] 
data2[grepl(pattern = "mayaro", x = tolower(data2$Outbreak)), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "mayaro", x = tolower(data2$Outbreak)), "icd103n"] <-"Other mosquito-borne viral fevers"
data2[grepl(pattern = "mayaro", x = tolower(data2$Outbreak)), "icd104n"] <- "Other specified mosquito-borne viral fevers"
data2[grepl(pattern = "mayaro", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "mayaro", x = tolower(data2$Outbreak)), "icd103c"] <- "A92"
data2[grepl(pattern = "mayaro", x = tolower(data2$Outbreak)), "icd104c"] <- "A928"

data2[grepl(pattern = "myocar", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "myocar", x = tolower(data2$Outbreak)), "icd10n"] <- "Other infectious diseases"
data2[grepl(pattern = "myocar", x = tolower(data2$Outbreak)), "icd103n"] <- "Other and unspecified infectious diseases"
data2[grepl(pattern = "myocar", x = tolower(data2$Outbreak)), "icd104n"] <- "Other and unspecified infectious diseases"
data2[grepl(pattern = "myocar", x = tolower(data2$Outbreak)), "icd10c"] <- "B99-B99"
data2[grepl(pattern = "myocar", x = tolower(data2$Outbreak)), "icd103c"] <- "B99"
data2[grepl(pattern = "myocar", x = tolower(data2$Outbreak)), "icd104c"] <- "B990"

data2[grepl(pattern = "swine", x = tolower(data2$Outbreak)), "Outbreak"] 
data2[grepl(pattern = "swine", x = tolower(data2$Outbreak)), "icd10n"] <-"Influenza and pneumonia"
data2[grepl(pattern = "swine", x = tolower(data2$Outbreak)), "icd103n"] <-"Influenza due to identified zoonotic or pandemic influenza virus"
data2[grepl(pattern = "swine", x = tolower(data2$Outbreak)), "icd104n"] <- "Influenza due to identified zoonotic or pandemic influenza virus"
data2[grepl(pattern = "swine", x = tolower(data2$Outbreak)), "icd10c"] <- "J09-J18"
data2[grepl(pattern = "swine", x = tolower(data2$Outbreak)), "icd103c"] <- "J09"
data2[grepl(pattern = "swine", x = tolower(data2$Outbreak)), "icd104c"] <- "J090"

data2[grepl(pattern = "food poiso", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "food poiso", x = tolower(data2$Outbreak)), "icd10n"] <- "Other infectious diseases"
data2[grepl(pattern = "food poiso", x = tolower(data2$Outbreak)), "icd103n"] <- "Other and unspecified infectious diseases"
data2[grepl(pattern = "food poiso", x = tolower(data2$Outbreak)), "icd104n"] <- "Other and unspecified infectious diseases"
data2[grepl(pattern = "food poiso", x = tolower(data2$Outbreak)), "icd10c"] <- "B99-B99"
data2[grepl(pattern = "food poiso", x = tolower(data2$Outbreak)), "icd103c"] <- "B99"
data2[grepl(pattern = "food poiso", x = tolower(data2$Outbreak)), "icd104c"] <- "B990"

## Some news report more than one outbreak Chikungunya and Dengue ##
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Chikungunya and Dengue", x = data2$Outbreak), ]
data2[nrow(data2), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[nrow(data2), "icd103n"] <- "Other mosquito-borne viral fevers"
data2[nrow(data2), "icd104n"] <- "Chikungunya virus disease"
data2[nrow(data2), "icd10c"] <- "A92-A99"
data2[nrow(data2), "icd103c"] <- "A92"
data2[nrow(data2), "icd104c"] <- "A920"

data2[grepl(pattern = "chikungunya$", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "chikungunya$", x = tolower(data2$Outbreak)), "icd10n"] <-"Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "chikungunya$", x = tolower(data2$Outbreak)), "icd103n"] <- "Other mosquito-borne viral fevers"
data2[grepl(pattern = "chikungunya$", x = tolower(data2$Outbreak)), "icd104n"] <- "Chikungunya virus disease"
data2[grepl(pattern = "chikungunya$", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "chikungunya$", x = tolower(data2$Outbreak)), "icd103c"] <- "A92"
data2[grepl(pattern = "chikungunya$", x = tolower(data2$Outbreak)), "icd104c"] <- "A920"

data2[grepl(pattern = "hanta", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "hanta", x = tolower(data2$Outbreak)), "icd10n"] <- "Other viral diseases"
data2[grepl(pattern = "hanta", x = tolower(data2$Outbreak)), "icd103n"] <-"Other viral diseases, not elsewhere classified"
data2[grepl(pattern = "hanta", x = tolower(data2$Outbreak)), "icd104n"] <- "Hantavirus (cardio-)pulmonary syndrome"
data2[grepl(pattern = "hanta", x = tolower(data2$Outbreak)), "icd10c"] <- "B25-B34"
data2[grepl(pattern = "hanta", x = tolower(data2$Outbreak)), "icd103c"] <- "B33"
data2[grepl(pattern = "hanta", x = tolower(data2$Outbreak)), "icd104c"] <- "B334"

data2[grepl(pattern = "hepatitis e", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "hepatitis e", x = tolower(data2$Outbreak)), "icd10n"] <- "Viral hepatitis"
data2[grepl(pattern = "hepatitis e", x = tolower(data2$Outbreak)), "icd103n"] <-"Other acute viral hepatitis"
data2[grepl(pattern = "hepatitis e", x = tolower(data2$Outbreak)), "icd104n"] <- "Acute hepatitis E"
data2[grepl(pattern = "hepatitis e", x = tolower(data2$Outbreak)), "icd10c"] <- "B15-B19"
data2[grepl(pattern = "hepatitis e", x = tolower(data2$Outbreak)), "icd103c"] <- "B17"
data2[grepl(pattern = "hepatitis e", x = tolower(data2$Outbreak)), "icd104c"] <- "B172"

data2[grepl(pattern = "hepatitis a", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "hepatitis a", x = tolower(data2$Outbreak)), "icd10n"] <- "Viral hepatitis"
data2[grepl(pattern = "hepatitis a", x = tolower(data2$Outbreak)), "icd103n"] <-"Acute hepatitis A"
data2[grepl(pattern = "hepatitis a", x = tolower(data2$Outbreak)), "icd104n"] <- "Acute hepatitis A"
data2[grepl(pattern = "hepatitis a", x = tolower(data2$Outbreak)), "icd10c"] <- "B15-B19"
data2[grepl(pattern = "hepatitis a", x = tolower(data2$Outbreak)), "icd103c"] <- "B15"
data2[grepl(pattern = "hepatitis a", x = tolower(data2$Outbreak)), "icd104c"] <- "B15"

data2[grepl(pattern = "nile", x = tolower(data2$Outbreak)), "Outbreak"] 
data2[grepl(pattern = "nile", x = tolower(data2$Outbreak)), "icd10n"] <-"Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "nile", x = tolower(data2$Outbreak)), "icd103n"] <-"Other mosquito-borne viral fevers"
data2[grepl(pattern = "nile", x = tolower(data2$Outbreak)), "icd104n"] <- "West Nile virus infection"
data2[grepl(pattern = "nile", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "nile", x = tolower(data2$Outbreak)), "icd103c"] <- "A92"
data2[grepl(pattern = "nile", x = tolower(data2$Outbreak)), "icd104c"] <- "A923"

data2[grepl(pattern = "seoul", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "seoul", x = tolower(data2$Outbreak)), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "seoul", x = tolower(data2$Outbreak)), "icd103n"] <- "Other viral haemorrhagic fevers, not elsewhere classified"
data2[grepl(pattern = "seoul", x = tolower(data2$Outbreak)), "icd104n"] <- "Haemorrhagic fever with renal syndrome"
data2[grepl(pattern = "seoul", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "seoul", x = tolower(data2$Outbreak)), "icd103c"] <- "A98"
data2[grepl(pattern = "seoul", x = tolower(data2$Outbreak)), "icd104c"] <- "A985"

data2[grepl(pattern = "occidioidom", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "occidioidom", x = tolower(data2$Outbreak)), "icd10n"] <- "Mycoses"
data2[grepl(pattern = "occidioidom", x = tolower(data2$Outbreak)), "icd103n"] <- "Coccidioidomycosis"
data2[grepl(pattern = "occidioidom", x = tolower(data2$Outbreak)), "icd104n"] <- "Coccidioidomycosis, unspecified"
data2[grepl(pattern = "occidioidom", x = tolower(data2$Outbreak)), "icd10c"] <- "B35-B49"
data2[grepl(pattern = "occidioidom", x = tolower(data2$Outbreak)), "icd103c"] <- "B38"
data2[grepl(pattern = "occidioidom", x = tolower(data2$Outbreak)), "icd104c"] <- "B389"

data2[grepl(pattern = "yellow", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "yellow", x = tolower(data2$Outbreak)), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "yellow", x = tolower(data2$Outbreak)), "icd103n"] <- "Yellow fever"
data2[grepl(pattern = "yellow", x = tolower(data2$Outbreak)), "icd104n"] <- "Yellow fever, unspecified"
data2[grepl(pattern = "yellow", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "yellow", x = tolower(data2$Outbreak)), "icd103c"] <- "A95"
data2[grepl(pattern = "yellow", x = tolower(data2$Outbreak)), "icd104c"] <- "A959"
data2[grepl(pattern = "sylvatic", x = tolower(data2$Outbreak)), "icd104n"] <- "Sylvatic yellow fever"
data2[grepl(pattern = "sylvatic", x = tolower(data2$Outbreak)), "icd104c"] <- "A950"

data2[grepl(pattern = "guillain", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "guillain", x = tolower(data2$Outbreak)), "icd10n"] <- "Polyneuropathies and other disorders of the peripheral nervous system"
data2[grepl(pattern = "guillain", x = tolower(data2$Outbreak)), "icd103n"] <- "Inflammatory polyneuropathy"
data2[grepl(pattern = "guillain", x = tolower(data2$Outbreak)), "icd104n"] <- "Guillain-Barré syndrome"
data2[grepl(pattern = "guillain", x = tolower(data2$Outbreak)), "icd10c"] <- "G60-G64"
data2[grepl(pattern = "guillain", x = tolower(data2$Outbreak)), "icd103c"] <- "G61"
data2[grepl(pattern = "guillain", x = tolower(data2$Outbreak)), "icd104c"] <- "G610"

data2[grepl(pattern = "listeriosis", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "listeriosis", x = tolower(data2$Outbreak)), "icd10n"] <- "Other bacterial diseases"
data2[grepl(pattern = "listeriosis", x = tolower(data2$Outbreak)), "icd103n"] <- "Listeriosis"
data2[grepl(pattern = "listeriosis", x = tolower(data2$Outbreak)), "icd104n"] <- "Listeriosis, unspecified"
data2[grepl(pattern = "listeriosis", x = tolower(data2$Outbreak)), "icd10c"] <- "A30-A49"
data2[grepl(pattern = "listeriosis", x = tolower(data2$Outbreak)), "icd103c"] <- "A32"
data2[grepl(pattern = "listeriosis", x = tolower(data2$Outbreak)), "icd104c"] <- "A329"

data2[grepl(pattern = "listeria", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "listeria", x = tolower(data2$Outbreak)), "icd10n"] <- "Other bacterial diseases"
data2[grepl(pattern = "listeria", x = tolower(data2$Outbreak)), "icd103n"] <- "Listeriosis"
data2[grepl(pattern = "listeria", x = tolower(data2$Outbreak)), "icd104n"] <- "Listeriosis, unspecified"
data2[grepl(pattern = "listeria", x = tolower(data2$Outbreak)), "icd10c"] <- "A30-A49"
data2[grepl(pattern = "listeria", x = tolower(data2$Outbreak)), "icd103c"] <- "A32"
data2[grepl(pattern = "listeria", x = tolower(data2$Outbreak)), "icd104c"] <- "A329"

data2[grepl(pattern = "zika", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "zika", x = tolower(data2$Outbreak)), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "zika", x = tolower(data2$Outbreak)), "icd103n"] <- "Other mosquito-borne viral fevers"
data2[grepl(pattern = "zika", x = tolower(data2$Outbreak)), "icd104n"] <- "Zika virus disease"
data2[grepl(pattern = "zika", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "zika", x = tolower(data2$Outbreak)), "icd103c"] <- "A92"
data2[grepl(pattern = "zika", x = tolower(data2$Outbreak)), "icd104c"] <- "A925"

data2[grepl(pattern = "h5 virus", x = tolower(data2$Outbreak)), "Outbreak"] 
data2[grepl(pattern = "h5 virus", x = tolower(data2$Outbreak)), "icd10n"] <-"Influenza and pneumonia"
data2[grepl(pattern = "h5 virus", x = tolower(data2$Outbreak)), "icd103n"] <-"Influenza due to identified zoonotic or pandemic influenza virus"
data2[grepl(pattern = "h5 virus", x = tolower(data2$Outbreak)), "icd104n"] <- "Influenza due to identified zoonotic or pandemic influenza virus"
data2[grepl(pattern = "h5 virus", x = tolower(data2$Outbreak)), "icd10c"] <- "J09-J18"
data2[grepl(pattern = "h5 virus", x = tolower(data2$Outbreak)), "icd103c"] <- "J09"
data2[grepl(pattern = "h5 virus", x = tolower(data2$Outbreak)), "icd104c"] <- "J090"

data2[grepl(pattern = "tubercu", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "tubercu", x = tolower(data2$Outbreak)), "icd10n"] <- "Tuberculosis"
data2[grepl(pattern = "tubercu", x = tolower(data2$Outbreak)), "icd103n"] <- "Respiratory tuberculosis, bacteriologically and histologically confirmed"
data2[grepl(pattern = "tubercu", x = tolower(data2$Outbreak)), "icd104n"] <- "Respiratory tuberculosis unspecified, confirmed bacteriologically and histologically"
data2[grepl(pattern = "tubercu", x = tolower(data2$Outbreak)), "icd10c"] <- "A15-A19"
data2[grepl(pattern = "tubercu", x = tolower(data2$Outbreak)), "icd103c"] <- "A15"
data2[grepl(pattern = "tubercu", x = tolower(data2$Outbreak)), "icd104c"] <- "A159"

data2[grepl(pattern = "leish", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "leish", x = tolower(data2$Outbreak)), "icd10n"] <- "Protozoal diseases"
data2[grepl(pattern = "leish", x = tolower(data2$Outbreak)), "icd103n"] <- "Leishmaniasis"
data2[grepl(pattern = "leish", x = tolower(data2$Outbreak)), "icd104n"] <- "Leishmaniasis, unspecified"
data2[grepl(pattern = "leish", x = tolower(data2$Outbreak)), "icd10c"] <- "B50-B64"
data2[grepl(pattern = "leish", x = tolower(data2$Outbreak)), "icd103c"] <- "B55"
data2[grepl(pattern = "leish", x = tolower(data2$Outbreak)), "icd104c"] <- "B559"

data2[grepl(pattern = "lepto", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "lepto", x = tolower(data2$Outbreak)), "icd10n"] <- "Certain zoonotic bacterial diseases"
data2[grepl(pattern = "lepto", x = tolower(data2$Outbreak)), "icd103n"] <- "Leptospirosis"
data2[grepl(pattern = "lepto", x = tolower(data2$Outbreak)), "icd104n"] <- "Leptospirosis, unspecified"
data2[grepl(pattern = "lepto", x = tolower(data2$Outbreak)), "icd10c"] <- "A20-A28"
data2[grepl(pattern = "lepto", x = tolower(data2$Outbreak)), "icd103c"] <- "A27"
data2[grepl(pattern = "lepto", x = tolower(data2$Outbreak)), "icd104c"] <- "A279"

data2[grepl(pattern = "nipah", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "nipah", x = tolower(data2$Outbreak)), "icd10n"] <- "Other viral diseases"
data2[grepl(pattern = "nipah", x = tolower(data2$Outbreak)), "icd103n"] <-"Viral infection of unspecified site"
data2[grepl(pattern = "nipah", x = tolower(data2$Outbreak)), "icd104n"] <- "Other viral infections of unspecified site"
data2[grepl(pattern = "nipah", x = tolower(data2$Outbreak)), "icd10c"] <- "B25-B34"
data2[grepl(pattern = "nipah", x = tolower(data2$Outbreak)), "icd103c"] <- "B34"
data2[grepl(pattern = "nipah", x = tolower(data2$Outbreak)), "icd104c"] <- "B348"

data2[grepl(pattern = "neurolog", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "neurolog", x = tolower(data2$Outbreak)), "icd10n"] <- "Other viral diseases"
data2[grepl(pattern = "neurolog", x = tolower(data2$Outbreak)), "icd103n"] <-"Viral infection of unspecified site"
data2[grepl(pattern = "neurolog", x = tolower(data2$Outbreak)), "icd104n"] <- "Other viral infections of unspecified site"
data2[grepl(pattern = "neurolog", x = tolower(data2$Outbreak)), "icd10c"] <- "B25-B34"
data2[grepl(pattern = "neurolog", x = tolower(data2$Outbreak)), "icd103c"] <- "B34"
data2[grepl(pattern = "neurolog", x = tolower(data2$Outbreak)), "icd104c"] <- "B348"

data2[grepl(pattern = "louse", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "louse", x = tolower(data2$Outbreak)), "icd10n"] <- "Rickettsioses"
data2[grepl(pattern = "louse", x = tolower(data2$Outbreak)), "icd103n"] <- "Typhus fever"
data2[grepl(pattern = "louse", x = tolower(data2$Outbreak)), "icd104n"] <- "Epidemic louse-borne typhus fever due to Rickettsia prowazekii"
data2[grepl(pattern = "louse", x = tolower(data2$Outbreak)), "icd10c"] <- "A75-A79"
data2[grepl(pattern = "louse", x = tolower(data2$Outbreak)), "icd103c"] <- "A75"
data2[grepl(pattern = "louse", x = tolower(data2$Outbreak)), "icd104c"] <- "A750"

data2[grepl(pattern = "measle", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "measle", x = tolower(data2$Outbreak)), "icd10n"] <- "Viral infections characterized by skin and mucous membrane lesions"
data2[grepl(pattern = "measle", x = tolower(data2$Outbreak)), "icd103n"] <- "Measles"
data2[grepl(pattern = "measle", x = tolower(data2$Outbreak)), "icd104n"] <- "Measles"
data2[grepl(pattern = "measle", x = tolower(data2$Outbreak)), "icd10c"] <- "B00-B09"
data2[grepl(pattern = "measle", x = tolower(data2$Outbreak)), "icd103c"] <- "B05"
data2[grepl(pattern = "measle", x = tolower(data2$Outbreak)), "icd104c"] <- "B05"

data2[grepl(pattern = "monkey", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "monkey", x = tolower(data2$Outbreak)), "icd10n"] <- "Viral infections characterized by skin and mucous membrane lesions"
data2[grepl(pattern = "monkey", x = tolower(data2$Outbreak)), "icd103n"] <- "Monkeypox"
data2[grepl(pattern = "monkey", x = tolower(data2$Outbreak)), "icd104n"] <- "Monkeypox"
data2[grepl(pattern = "monkey", x = tolower(data2$Outbreak)), "icd10c"] <- "B00-B09"
data2[grepl(pattern = "monkey", x = tolower(data2$Outbreak)), "icd103c"] <- "B04"
data2[grepl(pattern = "monkey", x = tolower(data2$Outbreak)), "icd104c"] <- "B04"

data2[grepl(pattern = "^unknown", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "^unknown", x = tolower(data2$Outbreak)), "icd10n"] <- "Other infectious diseases"
data2[grepl(pattern = "^unknown", x = tolower(data2$Outbreak)), "icd103n"] <- "Other and unspecified infectious diseases"
data2[grepl(pattern = "^unknown", x = tolower(data2$Outbreak)), "icd104n"] <- "Other and unspecified infectious diseases"
data2[grepl(pattern = "^unknown", x = tolower(data2$Outbreak)), "icd10c"] <- "B99-B99"
data2[grepl(pattern = "^unknown", x = tolower(data2$Outbreak)), "icd103c"] <- "B99"
data2[grepl(pattern = "^unknown", x = tolower(data2$Outbreak)), "icd104c"] <- "B990"

data2[grepl(pattern = "nidentified", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "nidentified", x = tolower(data2$Outbreak)), "icd10n"] <- "Other infectious diseases"
data2[grepl(pattern = "nidentified", x = tolower(data2$Outbreak)), "icd103n"] <- "Other and unspecified infectious diseases"
data2[grepl(pattern = "nidentified", x = tolower(data2$Outbreak)), "icd104n"] <- "Other and unspecified infectious diseases"
data2[grepl(pattern = "nidentified", x = tolower(data2$Outbreak)), "icd10c"] <- "B99-B99"
data2[grepl(pattern = "nidentified", x = tolower(data2$Outbreak)), "icd103c"] <- "B99"
data2[grepl(pattern = "nidentified", x = tolower(data2$Outbreak)), "icd104c"] <- "B990"

data2[grepl(pattern = "^undia", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "^undia", x = tolower(data2$Outbreak)), "icd10n"] <- "Other infectious diseases"
data2[grepl(pattern = "^undia", x = tolower(data2$Outbreak)), "icd103n"] <- "Other and unspecified infectious diseases"
data2[grepl(pattern = "^undia", x = tolower(data2$Outbreak)), "icd104n"] <- "Other and unspecified infectious diseases"
data2[grepl(pattern = "^undia", x = tolower(data2$Outbreak)), "icd10c"] <- "B99-B99"
data2[grepl(pattern = "^undia", x = tolower(data2$Outbreak)), "icd103c"] <- "B99"
data2[grepl(pattern = "^undia", x = tolower(data2$Outbreak)), "icd104c"] <- "B990"

data2[grepl(pattern = "melami", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "melami", x = tolower(data2$Outbreak)), "icd10n"] <- "Accidental poisoning by and exposure to noxious substances"
data2[grepl(pattern = "melami", x = tolower(data2$Outbreak)), "icd103n"] <- "Accidental poisoning by and exposure to other and unspecified drugs, medicaments and biological substances"
data2[grepl(pattern = "melami", x = tolower(data2$Outbreak)), "icd104n"] <- "Accidental poisoning by and exposure to other and unspecified drugs, medicaments and biological substances"
data2[grepl(pattern = "melami", x = tolower(data2$Outbreak)), "icd10c"] <- "X40-X49"
data2[grepl(pattern = "melami", x = tolower(data2$Outbreak)), "icd103c"] <- "X44"
data2[grepl(pattern = "melami", x = tolower(data2$Outbreak)), "icd104c"] <- "X44"

data2[grepl(pattern = "smallpox vac", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "smallpox vac", x = tolower(data2$Outbreak)), "icd10n"] <- "Accidental poisoning by and exposure to noxious substances"
data2[grepl(pattern = "smallpox vac", x = tolower(data2$Outbreak)), "icd103n"] <- "Accidental poisoning by and exposure to other and unspecified drugs, medicaments and biological substances"
data2[grepl(pattern = "smallpox vac", x = tolower(data2$Outbreak)), "icd104n"] <- "Accidental poisoning by and exposure to other and unspecified drugs, medicaments and biological substances"
data2[grepl(pattern = "smallpox vac", x = tolower(data2$Outbreak)), "icd10c"] <- "X40-X49"
data2[grepl(pattern = "smallpox vac", x = tolower(data2$Outbreak)), "icd103c"] <- "X44"
data2[grepl(pattern = "smallpox vac", x = tolower(data2$Outbreak)), "icd104c"] <- "X44"

data2[grepl(pattern = "lead", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "lead", x = tolower(data2$Outbreak)), "icd10n"] <- "Accidental exposure to other and unspecified factors"
data2[grepl(pattern = "lead", x = tolower(data2$Outbreak)), "icd103n"] <- "Exposure to other specified factors"
data2[grepl(pattern = "lead", x = tolower(data2$Outbreak)), "icd104n"] <- "Exposure to other specified factors"
data2[grepl(pattern = "lead", x = tolower(data2$Outbreak)), "icd10c"] <- "X58-X59"
data2[grepl(pattern = "lead", x = tolower(data2$Outbreak)), "icd103c"] <- "X58"
data2[grepl(pattern = "lead", x = tolower(data2$Outbreak)), "icd104c"] <- "X58"

## news without information on disease ##
data2[grepl(pattern = "undia", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "undia", x = tolower(data2$Outbreak)), "icd10n"] <- "Other infectious diseases"
data2[grepl(pattern = "undia", x = tolower(data2$Outbreak)), "icd103n"] <- "Other and unspecified infectious diseases"
data2[grepl(pattern = "undia", x = tolower(data2$Outbreak)), "icd104n"] <- "Other and unspecified infectious diseases"
data2[grepl(pattern = "undia", x = tolower(data2$Outbreak)), "icd10c"] <- "B99-B99"
data2[grepl(pattern = "undia", x = tolower(data2$Outbreak)), "icd103c"] <- "B99"
data2[grepl(pattern = "undia", x = tolower(data2$Outbreak)), "icd104c"] <- "B990"

data2[grepl(pattern = "HIV", x = data2$Outbreak), "Outbreak"]
data2[grepl(pattern = "HIV", x = data2$Outbreak), "icd10n"] <- "Human immunodeficiency virus [HIV] disease"
data2[grepl(pattern = "HIV", x = data2$Outbreak), "icd103n"] <- "Unspecified human immunodeficiency virus [HIV] disease"
data2[grepl(pattern = "HIV", x = data2$Outbreak), "icd104n"] <- "Unspecified human immunodeficiency virus [HIV] disease"
data2[grepl(pattern = "HIV", x = data2$Outbreak), "icd10c"] <- "B20-B24"
data2[grepl(pattern = "HIV", x = data2$Outbreak), "icd103c"] <- "B24"
data2[grepl(pattern = "HIV", x = data2$Outbreak), "icd104c"] <- "B24"

data2[grepl(pattern = "British Columbia, Canada is not SARS", x = data2$Description), ]
data2[grepl(pattern = "British Columbia, Canada is not SARS", x = data2$Description), "Country"] <- "Canada"
data2[grepl(pattern = "British Columbia, Canada is not SARS", x = data2$Description), "icd10n"] <- "Other viral diseases"
data2[grepl(pattern = "British Columbia, Canada is not SARS", x = data2$Description), "icd103n"] <-"Viral infection of unspecified site"
data2[grepl(pattern = "British Columbia, Canada is not SARS", x = data2$Description), "icd104n"] <- "Coronavirus infection, unspecified site"
data2[grepl(pattern = "British Columbia, Canada is not SARS", x = data2$Description), "icd10c"] <- "B25-B34"
data2[grepl(pattern = "British Columbia, Canada is not SARS", x = data2$Description), "icd103c"] <- "B34"
data2[grepl(pattern = "British Columbia, Canada is not SARS", x = data2$Description), "icd104c"] <- "B342"

data2[grepl(pattern = "China under investigation for SARS", x = data2$Description), ]
data2[grepl(pattern = "China under investigation for SARS", x = data2$Description), "icd10n"] <- "Codes for special purposes"
data2[grepl(pattern = "China under investigation for SARS", x = data2$Description), "icd103n"] <-"Provisional assignment of new diseases of uncertain etiology or emergency use"
data2[grepl(pattern = "China under investigation for SARS", x = data2$Description), "icd104n"] <- "Severe acute respiratory syndrome [SARS]"
data2[grepl(pattern = "China under investigation for SARS", x = data2$Description), "icd10c"] <- "U00-U49"
data2[grepl(pattern = "China under investigation for SARS", x = data2$Description), "icd103c"] <- "U04"
data2[grepl(pattern = "China under investigation for SARS", x = data2$Description), "icd104c"] <- "U049"

## reports on multiple cases of Cholera due to hurricane Mitch ##
data2[grepl(pattern = "1998 - hurricane mitch, update 8", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "1998 - hurricane mitch, update 8", x = tolower(data2$Outbreak)), "icd10n"] <- "Intestinal infectious diseases"
data2[grepl(pattern = "1998 - hurricane mitch, update 8", x = tolower(data2$Outbreak)), "icd103n"] <- "Cholera"
data2[grepl(pattern = "1998 - hurricane mitch, update 8", x = tolower(data2$Outbreak)), "icd104n"] <- "Classical cholera"
data2[grepl(pattern = "1998 - hurricane mitch, update 8", x = tolower(data2$Outbreak)), "icd10c"] <- "A00-A09"
data2[grepl(pattern = "1998 - hurricane mitch, update 8", x = tolower(data2$Outbreak)), "icd103c"] <- "A00"
data2[grepl(pattern = "1998 - hurricane mitch, update 8", x = tolower(data2$Outbreak)), "icd104c"] <- "A000"

data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - hurricane mitch, update 8", x = tolower(data2$Outbreak)), ] # Nicaragua
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - hurricane mitch, update 8", x = tolower(data2$Outbreak)), ] # El Salvador
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - hurricane mitch, update 8", x = tolower(data2$Outbreak)), ] # Honduras
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - hurricane mitch, update 8", x = tolower(data2$Outbreak)), ] # Guatemala

data2[grepl(pattern = "1998 - hurricane mitch, update 8", x = tolower(data2$Outbreak)), "Country"] <- c("Belize", "Nicaragua", "El Salvador", "Honduras", "Guatemala")

## reports on multiple cases of dengue due to hurricane Mitch ##
data2[grepl(pattern = "1998 - hurricane mitch, update 6", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "1998 - hurricane mitch, update 6", x = tolower(data2$Outbreak)), "icd10n"] <-"Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "1998 - hurricane mitch, update 6", x = tolower(data2$Outbreak)), "icd103n"] <-"Dengue"
data2[grepl(pattern = "1998 - hurricane mitch, update 6", x = tolower(data2$Outbreak)), "icd104n"] <- "Dengue, unspecified"
data2[grepl(pattern = "1998 - hurricane mitch, update 6", x = tolower(data2$Outbreak)), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "1998 - hurricane mitch, update 6", x = tolower(data2$Outbreak)), "icd103c"] <- "A97"
data2[grepl(pattern = "1998 - hurricane mitch, update 6", x = tolower(data2$Outbreak)), "icd104c"] <- "A979"

data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - hurricane mitch, update 6", x = tolower(data2$Outbreak)), ] # Nicaragua
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - hurricane mitch, update 6", x = tolower(data2$Outbreak)), ] # El Salvador
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - hurricane mitch, update 6", x = tolower(data2$Outbreak)), ] # Honduras

data2[grepl(pattern = "1998 - hurricane mitch, update 6", x = tolower(data2$Outbreak)), "Country"] <- c("Nicaragua", "El Salvador", "Honduras", "Guatemala")

## reports on multiple cases of malaria due to hurricane Mitch ##
data2[grepl(pattern = "1998 - Hurricane Mitch, Update 5", x = data2$Outbreak), "Outbreak"]
data2[grepl(pattern = "1998 - Hurricane Mitch, Update 5", x = data2$Outbreak), "icd10n"] <- "Protozoal diseases"
data2[grepl(pattern = "1998 - Hurricane Mitch, Update 5", x = data2$Outbreak), "icd103n"] <- "Unspecified malaria"
data2[grepl(pattern = "1998 - Hurricane Mitch, Update 5", x = data2$Outbreak), "icd104n"] <- "Unspecified malaria"
data2[grepl(pattern = "1998 - Hurricane Mitch, Update 5", x = data2$Outbreak), "icd10c"] <- "B50-B64"
data2[grepl(pattern = "1998 - Hurricane Mitch, Update 5", x = data2$Outbreak), "icd103c"] <- "B54"
data2[grepl(pattern = "1998 - Hurricane Mitch, Update 5", x = data2$Outbreak), "icd104c"] <- "B540"

data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - hurricane mitch, update 5", x = tolower(data2$Outbreak)), ] # Nicaragua
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - hurricane mitch, update 5", x = tolower(data2$Outbreak)), ] # Honduras

data2[grepl(pattern = "1998 - hurricane mitch, update 5", x = tolower(data2$Outbreak)), "Country"] <- c("Nicaragua", "Honduras", "Guatemala")

## reports on multiple cases of Leptospirosis due to hurricane Mitch ##
data2[grepl(pattern = "1998 - hurricane mitch, update 4", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "1998 - hurricane mitch, update 4", x = tolower(data2$Outbreak)), "icd10n"] <- "Certain zoonotic bacterial diseases"
data2[grepl(pattern = "1998 - hurricane mitch, update 4", x = tolower(data2$Outbreak)), "icd103n"] <- "Leptospirosis"
data2[grepl(pattern = "1998 - hurricane mitch, update 4", x = tolower(data2$Outbreak)), "icd104n"] <- "Leptospirosis, unspecified"
data2[grepl(pattern = "1998 - hurricane mitch, update 4", x = tolower(data2$Outbreak)), "icd10c"] <- "A20-A28"
data2[grepl(pattern = "1998 - hurricane mitch, update 4", x = tolower(data2$Outbreak)), "icd103c"] <- "A27"
data2[grepl(pattern = "1998 - hurricane mitch, update 4", x = tolower(data2$Outbreak)), "icd104c"] <- "A279"

data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - hurricane mitch, update 4", x = tolower(data2$Outbreak)), ] # Nicaragua
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - hurricane mitch, update 4", x = tolower(data2$Outbreak)), ] # Honduras

data2[grepl(pattern = "1998 - hurricane mitch, update 4", x = tolower(data2$Outbreak)), "Country"] <- c("Nicaragua", "Honduras", "Guatemala")

## Rwanda repatriation movement ##
# bloody diarrhoea, no severe cholera, measles
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), ] # bloody diarrhoea
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), ] # no severe cholera

data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "Outbreak"]
data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd10n"][[1]] <- "Intestinal infectious diseases"
data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd103n"][[1]] <- "Cholera"
data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd104n"][[1]] <- "Classical cholera"
data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd10c"][[1]] <- "A00-A09"
data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd103c"][[1]] <- "A00"
data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd104c"][[1]] <- "A000"

data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd10n"][[2]] <- "Intestinal infectious diseases"
data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd103n"][[2]] <- "Other gastroenteritis and colitis of infectious and unspecified origin"
data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd104n"][[2]] <- "Other and unspecified gastroenteritis and colitis of infectious origin"
data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd10c"][[2]] <- "A00-A09"
data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd103c"][[2]] <- "A09"
data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd104c"][[2]] <- "A090"

data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd10n"][[3]] <- "Viral infections characterized by skin and mucous membrane lesions"
data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd103n"][[3]] <- "Measles"
data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd104n"][[3]] <- "Measles"
data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd10c"][[3]] <- "B00-B09"
data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd103c"][[3]] <- "B05"
data2[grepl(pattern = "repatriation", x = tolower(data2$Outbreak)), "icd104c"][[3]] <- "B05"

### Information for countries
for(row in rownames(data2[is.na(data2$Country) == TRUE, ])){
  data2[row, "Country"] <- strsplit(data2[row, "Outbreak"], " in ")[[1]][1] # outbreak
}

isocountries <- read.csv("iso countries.csv")

#### By country, in the order they appear in the database ####
### India ###
data2[grepl(pattern = "India", x = data2$Country), "Country"]

# refers to multiple countries south west Indian Ocean #
# Chikungunya in La Réunion (France), Mayotte, Maurice, Seychelles and India #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "the south west Indian Ocean", x = data2$Country), ][2, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "the south west Indian Ocean", x = data2$Country), ][2, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "the south west Indian Ocean", x = data2$Country), ][2, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "the south west Indian Ocean", x = data2$Country), ][2, ]

data2[grepl(pattern = "indian", x = tolower(data2$Country)) & grepl(pattern = "chikung", x = tolower(data2$icd104n)), "Country"] <- c("India", "Réunion", "Mayotte", "Mauritius", "Seychelles")

# Several European countries have reported imported cases in people returning # 
# from these islands: France (160 imported cases), Germany, Italy, Norway and Switzerland #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Mayotte", x = data2$Country), ][3, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Mayotte", x = data2$Country), ][3, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Mayotte", x = data2$Country), ][3, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Mayotte", x = data2$Country), ][3, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Mayotte", x = data2$Country), ][3, ]

data2[grepl(pattern = "mayotte", x = tolower(data2$Country)) & grepl(pattern = "17 march 2006", x = tolower(data2$Date)), "Country"][2:6]  <- c("France", "Germany", "Italy", "Norway", "Switzerland")

# Dengue in Madagascar and Maldives #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "indian", x = tolower(data2$Country)), ]
data2[grepl(pattern = "indian", x = tolower(data2$Country)) & grepl(pattern = "dengue", x = tolower(data2$icd104n)), "Country"]  <- c("Madagascar", "Maldives")

## the rest of news referring to India ##
data2[grepl(pattern = "India", x = data2$Country), "Country"] <- "India"

### Venezuela ###
data2[grepl(pattern = "Venezuela", x = data2$Country), "Country"]

# Guillain-Barré syndrome – Colombia and Venezuela # 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "colombia and venezuela", x = tolower(data2$Country)), ]
data2[grepl(pattern = "colombia and venezuela", x = tolower(data2$Country)), "Country"] <- c("Colombia", "Venezuela (Bolivarian Republic of)")

## the rest of news referring to Venezuela ##
data2[grepl(pattern = "Venezuela", x = data2$Country), "Country"] <- "Venezuela (Bolivarian Republic of)"


### Democratic Republic of Congo ###
data2[grepl(pattern = "Congo", x = data2$Country) & grepl(pattern = "Democratic", x = data2$Country), "Country"]

# Guillain-Barré syndrome – Congo and Democratic Republic of Congo # 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = " and the republi", x = tolower(data2$Country)), ]
data2[grepl(pattern = " and the republi", x = tolower(data2$Country)), "Country"] <- c("Congo", "Congo Democratic Republic of the")

# Polio in Angola and the Democratic Republic of the Congo #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Polio in Angola  and the Democratic Republic of the Congo", x = data2$Description), ]
data2[grepl(pattern = "Polio in Angola  and the Democratic Republic of the Congo", x = data2$Description), "Country"] <- c("Angola", "Congo Democratic Republic of the")

# Poliomyelitis in Angola and the Democratic Republic of the Congo #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Poliomyelitis in Angola and the Democratic Republic of the Congo", x = data2$Description), ]
data2[grepl(pattern = "Poliomyelitis in Angola and the Democratic Republic of the Congo", x = data2$Description), "Country"] <- c("Angola", "Congo Democratic Republic of the")

## the rest of news referring to Democratic Republic of Congo ##
data2[grepl(pattern = "Congo", x = data2$Country) & grepl(pattern = "Democratic", x = data2$Country), "Country"] <- "Congo Democratic Republic of the"

### Nigeria ###
data2[grepl(pattern = "Nigeria", x = data2$Country), "Country"]

# Monkeypox - United Kingdom of Great Britain and Northern Ireland #
data2[grepl(pattern = "United Kingdom of Great Britain and Northern Ireland ex Nigeria", x = data2$Country), "Country"] <- "United Kingdom of Great Britain and Northern Ireland"

# Poliomyelitis in Nigeria and West Africa #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Nigeria and West Africa", x = data2$Country), ] # Benin
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Nigeria and West Africa", x = data2$Country), ] # Burkina Faso
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Nigeria and West Africa", x = data2$Country), ] # Ghana
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Nigeria and West Africa", x = data2$Country), ] # Niger
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Nigeria and West Africa", x = data2$Country), ] # Mali
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Nigeria and West Africa", x = data2$Country), ] # Togo

data2[grepl(pattern = "Nigeria and West Africa", x = data2$Country), "Country"] <- c("Nigeria", "Benin", "Burkina Faso", "Ghana", "Niger", "Mali", "Togo")

# Poliomyelitis in Nigeria and West/Central Africa #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Nigeria and West/Central Africa", x = data2$Country), ] # Benin
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Nigeria and West/Central Africa", x = data2$Country), ] # Niger
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Nigeria and West/Central Africa", x = data2$Country), ] # Côte d'Ivoire
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Nigeria and West/Central Africa", x = data2$Country), ] # Ghana
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Nigeria and West/Central Africa", x = data2$Country), ] # Guinea
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Nigeria and West/Central Africa", x = data2$Country), ] # Togo
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Nigeria and West/Central Africa", x = data2$Country), ] # Chad

data2[grepl(pattern = "Nigeria and West/Central Africa", x = data2$Country), "Country"] <- c("Nigeria", "Benin", "Niger", "Côte d'Ivoire", "Ghana", "Guinea", "Togo", "Chad")

## the rest of news referring to Nigeria ##
data2[grepl(pattern = "Nigeria", x = data2$Country), "Country"] <- "Nigeria"

### Madagascar ###
data2[grepl(pattern = "Madagascar", x = data2$Country), "Country"]

# Seychelles – Suspected Plague (Ex- Madagascar) # 
data2[grepl(pattern = "Ex", x = data2$Country) & grepl(pattern = "Madagas", x = data2$Country), "Country"] <- "Seychelles"

## the rest of news referring to Madagascar ##
data2[grepl(pattern = "Madagascar", x = data2$Country), "Country"] <- "Madagascar"

### Guinea ###
data2[grepl(pattern = "Guinea", x = data2$Country), "Country"]

# Guinea-Bissau # 
data2[grepl(pattern = "Bissau", x = data2$Country), "Country"] <- "Guinea-Bissau"

# Papua New Guinea #
data2[grepl(pattern = "Papua", x = data2$Country), "Country"] <- "Papua New Guinea"

# Update on polio in central Africa - polio confirmed in Equatorial Guinea, linked to outbreak in Cameroon #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Equatorial Guinea, linked", x = data2$Country), ] 

data2[grepl(pattern = "Equatorial Guinea, linked", x = data2$Country), "Country"] <- c("Equatorial Guinea", "Cameroon") 

## the rest of news referring to Guinea ##
data2[grepl(pattern = "Guinea", x = data2$Country) & !grepl(pattern = "Equato", x = data2$Country) & ! grepl(pattern = "Bissau", x = data2$Country) & !grepl(pattern = "Papua", x = data2$Country), "Country"] <- "Guinea"

### Saudi Arabia ###
data2[grepl(pattern = "Saudi", x = data2$Country), "Country"]

# Middle East respiratory syndrome coronavirus (MERS-CoV) – Saudi Arabia, United Arab Emirates, and Qatar #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Saudi Arabia, United Arab Emirates, and Qatar", x = data2$Country), ] # United Arab Emirates
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Saudi Arabia, United Arab Emirates, and Qatar", x = data2$Country), ] # Qatar

data2[grepl(pattern = "Saudi Arabia, United Arab Emirates, and Qatar", x = data2$Country), "Country"] <- c("Saudi Arabia", "United Arab Emirates", "Qatar")

# Middle East respiratory syndrome coronavirus (MERS-CoV) – Saudi Arabia and Qatar #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Saudi Arabia and Qatar", x = data2$Country), ] # Qatar

data2[grepl(pattern = "Saudi Arabia and Qatar", x = data2$Country), "Country"] <- c("Saudi Arabia", "Qatar")

# Meningococcal disease in Francen(Update), United Kingdom (Update), Oman, Saudi ARabia, Netherlands #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Oman, Saudi ARabia,", x = data2$Country), ] # France
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Oman, Saudi ARabia,", x = data2$Country), ] # UK
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Oman, Saudi ARabia,", x = data2$Country), ] # Oman
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Oman, Saudi ARabia,", x = data2$Country), ] # Netherlands

data2[grepl(pattern = "Oman, Saudi ARabia,", x = data2$Country), "Country"] <- c("Saudi Arabia", "France", "United Kingdom of Great Britain and Northern Ireland", "Oman", "Netherlands")

### United States of America ###
data2[grepl(pattern = "United States of America", x = data2$Country), "Country"]

# Seoul virus – United States of America and Canada #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "United States of America and Canada", x = data2$Country), ] # Canada

data2[grepl(pattern = "United States of America and Canada", x = data2$Country), "Country"] <- c("United States of America", "Canada")

## the rest of news referring to USA ##
data2[grepl(pattern = "United States of America", x = data2$Country), "Country"] <- "United States of America"

### China ###
data2[grepl(pattern = "China", x = data2$Country), "Country"]

# Novel Coronavirus – Japan (ex-China) # 
data2[grepl(pattern = "ex-China", x = data2$Country), "Country"] <- "Japan"

# Hong Kong # 
data2[grepl(pattern = "China", x = data2$Country) & grepl(pattern = "Kong", x = data2$Country), "Country"] <- "Hong Kong"

# China, Taiwan and Hong Kong #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "China, Taiwan and Hong Kong", x = data2$Description), ] # China
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "China, Taiwan and Hong Kong", x = data2$Description), ] # Taiwan

data2[grepl(pattern = "China, Taiwan and Hong Kong", x = data2$Description), "Country"] <- c("Hong Kong", "China", "Taiwan")

# situation in China, Thailand - update 47 #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "China, Thailand - update 47", x = data2$Description), ] # Thailand

data2[grepl(pattern = "China, Thailand - update 47", x = data2$Description), "Country"] <- c("China", "Thailand")


# Indonesia, China - update 44 #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Indonesia, China - update 44", x = data2$Description), ] # 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "China, Indonesia - update 41", x = data2$Description), ] # 

data2[grepl(pattern = "Indonesia, China - update 44", x = data2$Country), "Country"] <- c("China", "Indonesia")
data2[grepl(pattern = "China, Indonesia - update 41", x = data2$Country), "Country"] <- c("China", "Indonesia")

# Thailand; China announces suspected spread of infection # 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Thailand; China announces suspected spread of infection", x = data2$Description), ] # 

data2[grepl(pattern = "Thailand; China announces suspected spread of infection", x = data2$Country), "Country"] <- c("China", "Thailand")

# Taiwan, data on in-flight transmission, #
data2[grepl(pattern = "Taiwan, data on in-flight transmission,", x = data2$Description), ]
data2[grepl(pattern = "Taiwan, data on in-flight transmission,", x = data2$Description), "icd10n"] <- "Codes for special purposes"
data2[grepl(pattern = "Taiwan, data on in-flight transmission,", x = data2$Description), "icd103n"] <-"Provisional assignment of new diseases of uncertain etiology or emergency use"
data2[grepl(pattern = "Taiwan, data on in-flight transmission,", x = data2$Description), "icd104n"] <- "Severe acute respiratory syndrome [SARS]"
data2[grepl(pattern = "Taiwan, data on in-flight transmission,", x = data2$Description), "icd10c"] <- "U00-U49"
data2[grepl(pattern = "Taiwan, data on in-flight transmission,", x = data2$Description), "icd103c"] <- "U04"
data2[grepl(pattern = "Taiwan, data on in-flight transmission,", x = data2$Description), "icd104c"] <- "U049"

# The new deaths occurred in China (4), Hong Kong SA (3), Taiwan (8), and Singapore (1)
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Taiwan, data on in-flight transmission,", x = data2$Description), ] # 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Taiwan, data on in-flight transmission,", x = data2$Description), ] # 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Taiwan, data on in-flight transmission,", x = data2$Description), ] # 

data2[grepl(pattern = "Taiwan, data on in-flight transmission,", x = data2$Description), "Country"] <- c("China", "Hong Kong", "Taiwan Province of China", "Singapore") 

# China and Japan #
data2[grepl(pattern = "China and Japan", x = data2$Description), ]
data2[grepl(pattern = "China and Japan", x = data2$Description), "icd10n"] <- "Codes for special purposes"
data2[grepl(pattern = "China and Japan", x = data2$Description), "icd103n"] <-"Provisional assignment of new diseases of uncertain etiology or emergency use"
data2[grepl(pattern = "China and Japan", x = data2$Description), "icd104n"] <- "Severe acute respiratory syndrome [SARS]"
data2[grepl(pattern = "China and Japan", x = data2$Description), "icd10c"] <- "U00-U49"
data2[grepl(pattern = "China and Japan", x = data2$Description), "icd103c"] <- "U04"
data2[grepl(pattern = "China and Japan", x = data2$Description), "icd104c"] <- "U049"

data2[nrow(data2)+1, ] <- data2[grepl(pattern = "China and Japan", x = data2$Description), ] # 

data2[grepl(pattern = "China and Japan", x = data2$Description), "Country"] <- c("China", "Japan")

# China and Taiwan 
data2[grepl(pattern = "China", x = data2$Country) & grepl(pattern = "Taiwan", x = data2$Country), "Country"] <- "Taiwan Province of China"

# Only China for the rest of news
data2[grepl(pattern = "China", x = data2$Country) & !grepl(pattern = "Taiwan", x = data2$Country), "Country"] <- "China"

# Saudi Arabia # 
data2[grepl(pattern = "Saudi Arabia", x = data2$Country), "Country"] <- "Saudi Arabia"

# Circulating vaccine-derived poliovirus type 2 – Global update #
# African Region
# Since 2017, several genetically-distinct cVDPV2 outbreaks continue to be reported across the Region. 
# In total, 21 countries are affected by ongoing cVDPV2, and outbreak response activities continue to be implemented in
# Angola, Benin, Burkina Faso, Cameroon, Central African Republic, Chad, Côte d’Ivoire, Democratic Republic of the Congo, 
# Ethiopia, Ghana, Guinea, Kenya, Liberia, Mali, Niger, Nigeria, Republic of Congo, Senegal, Sierra Leone, South Sudan and Togo. 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ] 

data2[grepl(pattern = "2 - Global update", x = data2$Description), "Country"] <- c("Angola", "Benin", "Burkina Faso", "Cameroon", 
                                                                                   "Central African Republic", "Chad", "Côte d'Ivoire", 
                                                                                   "Congo Democratic Republic of the", "Ethiopia", "Ghana", 
                                                                                   "Guinea", "Kenya", "Liberia", "Mali", "Niger", "Nigeria", 
                                                                                   "Congo", "Senegal", "Sierra Leone", "South Sudan", "Togo")

# Eastern Mediterranean Region # 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ][2, ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ][2, ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ][2, ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ][2, ] 

data2[grepl(pattern = "2 - Global update", x = data2$Description), "Country"][22:25] <- c("Afghanistan", "Pakistan", "Sudan", "Somalia")

# European Region #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ][1, ] 

data2[grepl(pattern = "2 - Global update", x = data2$Description), "Country"][26] <- "Tajikistan"

# Western Pacific Region # 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ][1, ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "2 - Global update", x = data2$Description), ][1, ] 

data2[grepl(pattern = "2 - Global update", x = data2$Description), "Country"][27:28] <- c("Philippines", "Malaysia")

data2[grepl(pattern = "Lao Peo", x = data2$Country), "Country"] <- "Lao People's Democratic Republic"

data2[grepl(pattern = "Guiana", x = data2$Country), "Country"] <- "French Guiana"

data2[grepl(pattern = "Mayott", x = data2$Country), "Country"] <- "Mayotte"

data2[grepl(pattern = "South Sudan", x = data2$Country), "Country"] <- "South Sudan"

# French Territories of the Americas #
# French Guiana, Guadeloupe, Martinique, Saint-Martin, and Saint-Barthélemy #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "French Territories of the Americas", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "French Territories of the Americas", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "French Territories of the Americas", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "French Territories of the Americas", x = data2$Country), ] 

data2[grepl(pattern = "French Territories of the Americas", x = data2$Country), "Country"] <- c("French Guiana", "Guadeloupe", "Martinique",
                                                                                                "Saint Martin (French part)", "Saint Barthélemy")

data2[grepl(pattern = "Saudi Arabia", x = data2$Country), "Country"] <- "Saudi Arabia"

data2[grepl(pattern = "Emirates", x = data2$Country), "Country"] <- "United Arab Emirates"

# Marburg virus disease - Uganda and Kenya
data2[grepl(pattern = "Uganda and Kenya", x = data2$Country), ]

data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Uganda and Kenya", x = data2$Country), ][2, ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Uganda and Kenya", x = data2$Country), ][1, ] 

data2[grepl(pattern = "Uganda and Kenya", x = data2$Country), "Country"] <- c("Uganda", "Uganda", "Kenya", "Kenya")

data2[grepl(pattern = "Uganda", x = data2$Country), "Country"] <- "Uganda"

# Case of Marburg Haemorrhagic Fever imported into the Netherlands from Uganda #
data2[grepl(pattern = "into the Netherlands from Uganda", x = data2$Description), "Country"] <- "Netherlands"

data2[grepl(pattern = "Netherlands", x = data2$Country), "Country"] <- "Netherlands"

# 2000 - Meningococcal disease in Singapore, Indonesia, Iran and Morocco #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Singapore, Indonesia, Iran and Morocco", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Singapore, Indonesia, Iran and Morocco", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Singapore, Indonesia, Iran and Morocco", x = data2$Country), ]

data2[grepl(pattern = "Singapore, Indonesia, Iran and Morocco", x = data2$Country), "Country"] <- c("Singapore", "Indonesia", 
                                                                                                    "Iran (Islamic Republic of)", "Morocco")

data2[grepl(pattern = "Iran", x = data2$Country), "Country"] <- "Iran (Islamic Republic of)"

data2[grepl(pattern = "union", x = data2$Country), "Country"] <- "Réunion"

# Measles - Western Pacific Region #
# Australia
# China
# Hong Kong SAR, China
# Macao SAR, China
# Japan
# Malaysia    
# New Zealand
# The Philippines

data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Western Pacific Region", x = data2$Description), ][1, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Western Pacific Region", x = data2$Description), ][1, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Western Pacific Region", x = data2$Description), ][1, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Western Pacific Region", x = data2$Description), ][1, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Western Pacific Region", x = data2$Description), ][1, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Western Pacific Region", x = data2$Description), ][1, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Western Pacific Region", x = data2$Description), ][1, ]

data2[grepl(pattern = "Western Pacific Region", x = data2$Country), "Country"] <- c("Australia", "China", "Hong Kong", "Macao",
                                                                                    "Japan", "Malaysia", "New Zealand", "Philippines")

# Zika virus infection – Argentina and France #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Argentina and France", x = data2$Country), ]

data2[grepl(pattern = "Argentina and France", x = data2$Country), "Country"] <- c("Argentina", "France")

data2[grepl(pattern = "Argentin", x = data2$Country), "Country"] <- "Argentina"

# Zika virus infection – Argentina and France #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = " European Region", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = " European Region", x = data2$Country), ]

data2[grepl(pattern = " European Region", x = data2$Country), "Country"] <- c("Albania", "Romania", "Ukraine")

# Circulating vaccine-derived polioviruses – Horn of Africa #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Horn of Africa", x = data2$Country) & grepl(pattern = "Circulating vaccine", x = data2$Outbreak), ]

data2[grepl(pattern = "Horn of Africa", x = data2$Country) & grepl(pattern = "Circulating vaccine", x = data2$Outbreak), "Country"] <- c("Somalia", "Kenya")

# Rift Valley Fever in Kenya, Somalia and the United Republic of Tanzania #
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Kenya, Somalia", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Kenya, Somalia", x = data2$Country), ]

data2[grepl(pattern = "Kenya, Somalia", x = data2$Country), "Country"] <- c("Somalia", "Kenya", "Tanzania United Republic of")

# 1998 - Rift Valley Fever in Kenya and Somalia - Update 2
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Kenya and Somalia - Update 2", x = data2$Country), ]

data2[grepl(pattern = "Kenya and Somalia - Update 2", x = data2$Country), "Country"] <- c("Somalia", "Kenya")

data2[grepl(pattern = "Banglad", x = data2$Country), "Country"] <- "Bangladesh"

# 1996 - Ebola infection in Côte d'Ivoire/Liberia
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Ivoire/Liberia", x = data2$Country), ]

data2[grepl(pattern = "Ivoire/Liberia", x = data2$Country), "Country"] <- c("Côte d'Ivoire", "Liberia")

data2[grepl(pattern = "voire", x = data2$Country), "Country"] <- "Côte d'Ivoire"

# Lassa Fever - Benin, Togo and Burkina Faso
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Benin, Togo and Burkina Faso", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Benin, Togo and Burkina Faso", x = data2$Country), ]

data2[grepl(pattern = "Benin, Togo and Burkina Faso", x = data2$Country), "Country"] <- c("Benin", "Togo", "Burkina Faso")

data2[grepl(pattern = "- Bovine Spongiform Encephalopathy", x = data2$Country), "Country"] <- c("United Kingdom of Great Britain and Northern Ireland")
data2[grepl(pattern = "1996 - Creutzfeldt-Jakob Disease", x = data2$Country), "Country"] <- c("United Kingdom of Great Britain and Northern Ireland")

# 1996 - Dengue/dengue haemorrhagic fever in the Philippines and Malaysia
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Philippines and Malaysi", x = data2$Country), ] 

data2[grepl(pattern = "Philippines and Malaysi", x = data2$Country), "Country"] <- c("Philippines", "Malaysia")

data2[grepl(pattern = "Philippines", x = data2$Country), "Country"] <- "Philippines"

# 1996 - Global Cholera Update 2
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1996 - Global Cholera Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1996 - Global Cholera Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1996 - Global Cholera Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1996 - Global Cholera Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1996 - Global Cholera Update 2", x = data2$Country), ]

data2[grepl(pattern = "1996 - Global Cholera Update 2", x = data2$Country), "Country"] <- c("Cabo Verde", "Niger", "Nigeria", 
                                                                                            "Senegal", "Somalia", "Mexico")

# 1996 - Global Cholera Update
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1996 - Global Cholera Update", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1996 - Global Cholera Update", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1996 - Global Cholera Update", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1996 - Global Cholera Update", x = data2$Country), ]

data2[grepl(pattern = "1996 - Global Cholera Update", x = data2$Country), "Country"] <- c("Cabo Verde", "Côte d'Ivoire", 
                                                                                          "Iran (Islamic Republic of)", 
                                                                                          "Iraq", "Senegal")

# the Great Lakes area (Burundi, United Republic of Tanzania) - Update 3
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Update 3", x = data2$Country) & grepl(pattern = "Great Lakes", x = data2$Country), ]

data2[grepl(pattern = "Update 3", x = data2$Country) & grepl(pattern = "Great Lakes", x = data2$Country), "Country"] <- c("Tanzania United Republic of", "Burundi")

# the Great Lakes area (Burundi, United Republic of Tanzania) - Update 2
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Update 2", x = data2$Country) & grepl(pattern = "Great Lakes", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Update 2", x = data2$Country) & grepl(pattern = "Great Lakes", x = data2$Country), ]

data2[grepl(pattern = "Update 2", x = data2$Country) & grepl(pattern = "Great Lakes", x = data2$Country), "Country"] <- c("Rwanda", "Tanzania United Republic of", "Burundi")

# the Great Lakes area (Burundi, United Republic of Tanzania) - Update 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Update", x = data2$Country) & grepl(pattern = "Great Lakes", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Update", x = data2$Country) & grepl(pattern = "Great Lakes", x = data2$Country), ]

data2[grepl(pattern = "Update", x = data2$Country) & grepl(pattern = "Great Lakes", x = data2$Country), "Country"] <- c("Rwanda", "Tanzania United Republic of", "Burundi")

# the Great Lakes area (Burundi, United Republic of Tanzania) 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Great Lakes a", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Great Lakes a", x = data2$Country), ]

data2[grepl(pattern = "Great Lakes a", x = data2$Country), "Country"] <- c("Rwanda", "Tanzania United Republic of", "Burundi")

# the Great Lakes area (Burundi, United Republic of Tanzania) 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Rwanda and", x = data2$Country), ]

data2[grepl(pattern = "Rwanda and", x = data2$Country), "Country"] <- c("Rwanda", "Congo Democratic Republic of the")

data2[grepl(pattern = "Rwanda", x = data2$Country), "Country"] <- "Rwanda"

# Zaire is known from 1997 as Democratic Republic of the Congo
data2[grepl(pattern = "Zaire", x = data2$Country), "Country"] <- "Congo Democratic Republic of the"

data2[grepl(pattern = "Zimbabwe", x = data2$Country), "Country"] <- "Zimbabwe"

data2[grepl(pattern = "Yemen", x = data2$Country), "Country"] <- "Yemen"

# Viet Nam and Cambodia - update 12
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Viet Nam and Cambodia", x = data2$Country) & grepl(pattern = "12", x = data2$Country), ] 

data2[grepl(pattern = "Viet Nam and Cambodia", x = data2$Country) & grepl(pattern = "12", x = data2$Country), "Country"] <- c("Viet Nam", "Cambodia")

# Viet Nam and Cambodia - update 8
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Viet Nam and Cambodia", x = data2$Country) & grepl(pattern = "8", x = data2$Country), ] 

data2[grepl(pattern = "Viet Nam and Cambodia", x = data2$Country) & grepl(pattern = "8", x = data2$Country), "Country"] <- c("Viet Nam", "Cambodia")

# Avian influenza A(H5N1) - update 22: First data on patients from Viet Nam, Clinical data from Hong Kong 1997, Susceptibility of H5N1 viruses to antiviral drugs
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "First data on patients from Viet Nam", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "First data on patients from Viet Nam", x = data2$Country), ] 

data2[grepl(pattern = "First data on patients from Viet Nam", x = data2$Country), "Country"] <- c("Viet Nam", "Hong Kong", "Thailand")

# Thailand and Viet Nam
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Thailand and Viet Nam", x = data2$Country), ] 

data2[grepl(pattern = "Thailand and Viet Nam", x = data2$Country), "Country"] <- c("Viet Nam", "Thailand")

data2[grepl(pattern = "Viet Nam", x = data2$Country), "Country"] <- "Viet Nam"

data2[grepl(pattern = "Russia", x = data2$Country), "Country"] <- "Russian Federation"

# Cholera - Vibrio cholerae O139 strain
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Cholera - Vibrio cholerae O139 strain", x = data2$Country), ] 

data2[grepl(pattern = "Cholera - Vibrio cholerae O139 strain", x = data2$Country), "Country"] <- c("India", "Hong Kong") 

data2[grepl(pattern = "Armenia", x = data2$Country), "Country"] <- "Armenia"

data2[grepl(pattern = "Comoros", x = data2$Country), "Country"] <- "Comoros"

# Cholera - Vibrio cholerae O139 strain
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Dengue", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Dengue", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Dengue", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Dengue", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Dengue", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Dengue", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Dengue", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Dengue", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Dengue", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Dengue", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Dengue", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Dengue", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Dengue", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Dengue", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Dengue", x = data2$Country), ] 

data2[grepl(pattern = "1998 - Dengue", x = data2$Country), "Country"] <- c("Malaysia", "Taiwan", "Cambodia", "Viet Nam", "Thailand", 
                                                                           "Philippines", "Indonesia", "Myanmar", "Guam", "Cook Islands",
                                                                           "Fiji", "New Caledonia", "Kiribati", "Brazil", "Venezuela", 
                                                                           "Colombia") 

# 1998 - Influenza
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Influenza", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Influenza", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Influenza", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1998 - Influenza", x = data2$Country), ] 

data2[grepl(pattern = "1998 - Influenza", x = data2$Country), "Country"] <- c("United States of America", "Canada", "Israel",
                                                                              "Iran (Islamic Republic of)", "Japan") 

# 1999 - Cholera outbreaks
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1999 - Cholera outbreaks", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1999 - Cholera outbreaks", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1999 - Cholera outbreaks", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1999 - Cholera outbreaks", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "1999 - Cholera outbreaks", x = data2$Country), ] 

data2[grepl(pattern = "1999 - Cholera outbreaks", x = data2$Country), "Country"] <- c("Burundi", "Madagascar", 
                                                                                      "Honduras", "Nicaragua", 
                                                                                      "Afghanistan", "Brunei Darussalam") 

data2[grepl(pattern = "Belgium", x = data2$Country), "Country"] <- "Belgium"

# France and the United Kingdom
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "France and the", x = data2$Country), ] 

data2[grepl(pattern = "France and the", x = data2$Country), "Country"] <- c("France", "United Kingdom of Great Britain and Northern Ireland") 

data2[grepl(pattern = "France", x = data2$Country), "Country"] <- "France"

data2[grepl(pattern = "2000 - Ebola haemorrhagic fever - Update", x = data2$Country), "Country"] <- "Uganda"

data2[grepl(pattern = "UK", x = data2$Country), "Country"] <- "United Kingdom of Great Britain and Northern Ireland"

# 2001 - Meningococcal disease, serogroup W135 - Update 3
data2[grepl(pattern = "serogroup W135 - Update 3", x = data2$Country), "Country"] <- "France"

# 2001 - Meningococcal disease, serogroup W135 - Update 2
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135 - Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135 - Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135 - Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135 - Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135 - Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135 - Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135 - Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135 - Update 2", x = data2$Country), ]

data2[grepl(pattern = "serogroup W135 - Update 2", x = data2$Country), "Country"] <- c("Burkina Faso", "Niger", "Central African Republic",
                                                                                       "Denkmark", "France", "Norway", "United Kingdom of Great Britain and Northern Ireland", 
                                                                                       "Saudi Arabia", "Singapore")

# 2001 - Meningococcal disease, serogroup W135 - Update
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135 - Update", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135 - Update", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135 - Update", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135 - Update", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135 - Update", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135 - Update", x = data2$Country), ]

data2[grepl(pattern = "serogroup W135 - Update", x = data2$Country), "Country"] <- c("Burkina Faso", "Central African Republic",
                                                                                     "Denkmark", "France", "Norway", "United Kingdom of Great Britain and Northern Ireland", 
                                                                                     "Singapore")

# 2001 - Meningococcal disease, serogroup W135
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "serogroup W135", x = data2$Country), ]

data2[grepl(pattern = "serogroup W135", x = data2$Country), "Country"] <- c("Burkina Faso", "Central African Republic",
                                                                            "France", "Norway", "United Kingdom of Great Britain and Northern Ireland", 
                                                                            "Saudi Arabia", "Singapore")

data2[grepl(pattern = "Yellow fever - Update 6", x = data2$Country), "Country"] <- "Côte d'Ivoire"

# Gabon/The Republic of the Congo
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Gabon", x = data2$Country) & grepl(pattern = "Congo", x = data2$Country), ][1, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Gabon", x = data2$Country) & grepl(pattern = "Congo", x = data2$Country), ][2, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Gabon", x = data2$Country) & grepl(pattern = "Congo", x = data2$Country), ][3, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Gabon", x = data2$Country) & grepl(pattern = "Congo", x = data2$Country), ][4, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Gabon", x = data2$Country) & grepl(pattern = "Congo", x = data2$Country), ][5, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Gabon", x = data2$Country) & grepl(pattern = "Congo", x = data2$Country), ][6, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Gabon", x = data2$Country) & grepl(pattern = "Congo", x = data2$Country), ][7, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Gabon", x = data2$Country) & grepl(pattern = "Congo", x = data2$Country), ][8, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Gabon", x = data2$Country) & grepl(pattern = "Congo", x = data2$Country), ][9, ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Gabon", x = data2$Country) & grepl(pattern = "Congo", x = data2$Country), ][10, ]

data2[grepl(pattern = "Gabon", x = data2$Country) & grepl(pattern = "Congo", x = data2$Country), "Country"] <- c(rep("Congo", 10), rep("Gabon", 10))

data2[grepl(pattern = "Republic of the Congo", x = data2$Country), "Country"] <- "Congo"

data2[grepl(pattern = "Congo - update", x = data2$Country), "Country"] <- "Congo"

data2[grepl(pattern = "the Republic of Congo", x = data2$Country), "Country"] <- "Congo"

data2[grepl(pattern = "the Congo", x = data2$Country), "Country"] <- "Congo"

data2[grepl(pattern = "a stone marten", x = data2$Country), "Country"] <- "Germany"

data2[grepl(pattern = "Afghanistan", x = data2$Country), "Country"] <- "Afghanistan"

# Africa (summary) - Update 2
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update 2", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update 2", x = data2$Country), ]

data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update 2", x = data2$Country), "Country"] <- c("Benin", "Burkina Faso", "Cameroon", "Central African Republic", "Chad",
                                                                                                                                "Ghana", "Mali", "Niger", "Nigeria", "Tanzania", "Togo", "Congo Democratic Republic of the")

# Africa (summary) - Update
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update", x = data2$Country), ]

data2[grepl(pattern = "Africa \\(summary", x = data2$Country) & grepl(pattern = "Update", x = data2$Country), "Country"] <- c("Benin", "Burkina Faso", "Cameroon", "Central African Republic", "Chad",
                                                                                                                              "Ghana", "Mali", "Niger", "Nigeria", "Tanzania", "Togo")

# Africa (summary)
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Africa \\(summary", x = data2$Country), ]

data2[grepl(pattern = "Africa \\(summary", x = data2$Country), "Country"] <- c("Benin", "Burkina Faso", "Cameroon", "Central African Republic",
                                                                               "Chad", "Mali", "Niger", "Nigeria")

data2[grepl(pattern = "Algeria", x = data2$Country), "Country"] <- "Algeria"

data2[grepl(pattern = "Angola", x = data2$Country), "Country"] <- "Angola"

data2[grepl(pattern = "summary of cases to date - Update 5", x = data2$Country), "Country"] <- "China"

data2[grepl(pattern = "Australia", x = data2$Country), "Country"] <- "Australia"

# Avian Influenza - Assessment of the current situation
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Assessment of the current situation", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Assessment of the current situation", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Assessment of the current situation", x = data2$Country), ]

data2[grepl(pattern = "Assessment of the current situation", x = data2$Country), "Country"] <- c("China", "Indonesia", "Thailand", "Viet Nam")

# Avian influenza - cumulative number of cases - update 18
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "cumulative number of cases - update 18", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "cumulative number of cases - update 18", x = data2$Country), ]

data2[grepl(pattern = "cumulative number of cases - update 18", x = data2$Country), "Country"] <- c("Cambodia", "Thailand", "Viet Nam")

data2[grepl(pattern = "Azerbaijan", x = data2$Country), "Country"] <- "Azerbaijan"

# Circulating vaccine-derived poliovirus type 2 – Africa Region: Disease outbreak news: Update, 31 July 2019
# Niger, Cameroon, and Nigeria
# Democratic Republic of the Congo
# Angola
# Central African Republic
# Somalia, Kenya, and Ethiopia
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "31 july 2019", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "31 july 2019", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "31 july 2019", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "31 july 2019", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "31 july 2019", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "31 july 2019", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "31 july 2019", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "31 july 2019", x = data2$Date), ]

data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "31 july 2019", x = data2$Date), "Country"] <- c("Niger", "Nigeria", "Cameroon", 
                                                                                                                       "Congo Democratic Republic of the", 
                                                                                                                       "Angola", "Central African Republic", 
                                                                                                                       "Somalia", "Kenya", "Ethiopia")

# Recent outbreaks of cholera in Africa
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "18 february 2004", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "18 february 2004", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "18 february 2004", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "18 february 2004", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "18 february 2004", x = data2$Date), ]

data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "18 february 2004", x = data2$Date), "Country"] <- c("Burundi", "Cameroon", "Mali", 
                                                                                                                           "Mozambique", "South Africa", "Zambia")

# 1999 - Cholera in Africa
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "26 february 1999", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "26 february 1999", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "26 february 1999", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "26 february 1999", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "26 february 1999", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "26 february 1999", x = data2$Date), ]

data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "26 february 1999", x = data2$Date), "Country"] <- c("Kenya", "Mozambique", "Somalia", "Uganda", 
                                                                                                                           "Tanzania United Republic of", "Zambia", "Zimbabwe")

# 1998 - Cholera in Africa
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "17 october 1996", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "17 october 1996", x = data2$Date), ]

data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "17 october 1996", x = data2$Date), "Country"] <- c("Guinea-Bissau", "Senagal", "Togo") 

# 1996 - Cholera in Africa
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "15 february 1996", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "15 february 1996", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "15 february 1996", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "15 february 1996", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "15 february 1996", x = data2$Date), ]

data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "15 february 1996", x = data2$Date), "Country"] <- c("Burkina Faso", "Cabo Verde", "Mali", 
                                                                                                                           "Senegal", "Argentina", "Ecuador") 

data2[grepl(pattern = "Balearic", x = data2$Country), "Country"] <- NA

data2[grepl(pattern = "Bangladesh", x = data2$Country), "Country"] <- "Bangladesh"

data2[grepl(pattern = "Benin", x = data2$Country), "Country"] <- "Benin"

# Geographical spread of H5N1 avian influenza in birds - update 28
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "birds - update 28", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "birds - update 28", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "birds - update 28", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "birds - update 28", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "birds - update 28", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "birds - update 28", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "birds - update 28", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "birds - update 28", x = data2$Country), ] 

data2[grepl(pattern = "birds - update 28", x = data2$Country), "Country"] <- c("Kazakhstan", "Russian Federation", "Mongolia", "China", 
                                                                               "Viet Nam", "Cambodia", "Thailand", 
                                                                               "Lao People's Democratic Republic", "Indonesia") 

# Geographical spread of H5N1 avian influenza in birds - update 34
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "birds - update 34", x = data2$Country), ]

data2[grepl(pattern = "birds - update 34", x = data2$Country), "Country"] <- c("Turkey", "Romania")

data2[grepl(pattern = "Bolivia", x = data2$Country), "Country"] <- "Bolivia (Plurinational State of)"

# Zika virus infection – Brazil and Colombia
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Brazil and", x = data2$Country), ] 

data2[grepl(pattern = "Brazil and", x = data2$Country), "Country"] <- c("Brazil", "Colombia") 

data2[grepl(pattern = "Brazil", x = data2$Country), "Country"] <- "Brazil"

# West Africa: Togo, Burkina Faso and Ghana
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Togo, Burkina Faso and Ghana", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Togo, Burkina Faso and Ghana", x = data2$Country), ] 

data2[grepl(pattern = "Togo, Burkina Faso and Ghana", x = data2$Country), "Country"] <- c("Togo", "Burkina Faso", "Ghana")

data2[grepl(pattern = "Burkina Faso", x = data2$Country), "Country"] <- "Burkina Faso"

data2[grepl(pattern = "Burundi", x = data2$Country), "Country"] <- "Burundi"

data2[grepl(pattern = "39;Ivoir", x = data2$Country), "Country"] <- "Côte d'Ivoire"

data2[grepl(pattern = "Cambodia", x = data2$Country), "Country"] <- "Cambodia"

# Botulism in the United States and Canada
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "States and Canada", x = data2$Country), ] 

data2[grepl(pattern = "States and Canada", x = data2$Country), "Country"] <- c("Canada", "United States of America") 

data2[grepl(pattern = "Canada", x = data2$Country), "Country"] <- "Canada"

data2[grepl(pattern = "Verde", x = data2$Country), "Country"] <- "Cabo Verde"

# EHEC outbreak: Increase in cases in Germany
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "EHEC outbreak: Increase in cases in Germany", x = data2$Description) & grepl(pattern = "B95-B98", x = data2$icd10c), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "EHEC outbreak: Increase in cases in Germany", x = data2$Description) & grepl(pattern = "B95-B98", x = data2$icd10c), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "EHEC outbreak: Increase in cases in Germany", x = data2$Description) & grepl(pattern = "B95-B98", x = data2$icd10c), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "EHEC outbreak: Increase in cases in Germany", x = data2$Description) & grepl(pattern = "B95-B98", x = data2$icd10c), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "EHEC outbreak: Increase in cases in Germany", x = data2$Description) & grepl(pattern = "B95-B98", x = data2$icd10c), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "EHEC outbreak: Increase in cases in Germany", x = data2$Description) & grepl(pattern = "B95-B98", x = data2$icd10c), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "EHEC outbreak: Increase in cases in Germany", x = data2$Description) & grepl(pattern = "B95-B98", x = data2$icd10c), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "EHEC outbreak: Increase in cases in Germany", x = data2$Description) & grepl(pattern = "B95-B98", x = data2$icd10c), ]

data2[grepl(pattern = "EHEC outbreak: Increase in cases in Germany", x = data2$Description) & grepl(pattern = "B95-B98", x = data2$icd10c), "Country"] <- c("Germany", "Austria", "Denmark", "France", 
                                                                                                                                                            "Netherlands", "Norway", "Sweden", "Switzerland", 
                                                                                                                                                            "United Kingdom of Great Britain and Northern Ireland")

# EHEC outbreak: Increase in cases in Germany
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "EHEC outbreak: Increase in cases in Germany", x = data2$Description) & grepl(pattern = "D55-D59", x = data2$icd10c), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "EHEC outbreak: Increase in cases in Germany", x = data2$Description) & grepl(pattern = "D55-D59", x = data2$icd10c), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "EHEC outbreak: Increase in cases in Germany", x = data2$Description) & grepl(pattern = "D55-D59", x = data2$icd10c), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "EHEC outbreak: Increase in cases in Germany", x = data2$Description) & grepl(pattern = "D55-D59", x = data2$icd10c), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "EHEC outbreak: Increase in cases in Germany", x = data2$Description) & grepl(pattern = "D55-D59", x = data2$icd10c), ]

data2[grepl(pattern = "EHEC outbreak: Increase in cases in Germany", x = data2$Description) & grepl(pattern = "D55-D59", x = data2$icd10c), "Country"] <- c("Germany", "Denmark", "Netherlands", "Spain", "Sweden",
                                                                                                                                                            "United Kingdom of Great Britain and Northern Ireland")

# Update 74 - Global decline in cases and deaths continues
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "cases and deaths continues", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "cases and deaths continues", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "cases and deaths continues", x = data2$Country), ] 

data2[grepl(pattern = "cases and deaths continues", x = data2$Country), "Country"] <- c("Canada", "Taiwan Province of China", "China", "Hong Kong") 

# Cholera in Central Africa: Cameroon, Chad, Niger and Nigeria
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Central Africa", x = data2$Country) & grepl(pattern = "8 october 2010", x = data2$Date), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Central Africa", x = data2$Country) & grepl(pattern = "8 october 2010", x = data2$Date), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Central Africa", x = data2$Country) & grepl(pattern = "8 october 2010", x = data2$Date), ] 

data2[grepl(pattern = "Central Africa", x = data2$Country) & grepl(pattern = "8 october 2010", x = data2$Date), "Country"] <- c("Cameroon", "Chad", "Niger", "Nigeria")  

data2[grepl(pattern = "Central Africa", x = data2$Country), "Country"] <- "Central African Republic"

# Chad, Mozambique and Zambia
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Chad, Mozambique and Zambia", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Chad, Mozambique and Zambia", x = data2$Country), ]

data2[grepl(pattern = "Chad, Mozambique and Zambia", x = data2$Country), "Country"] <- c("Chad", "Mozambique", "Zambia")

data2[grepl(pattern = "Chad", x = data2$Country), "Country"] <- "Chad"

data2[grepl(pattern = "Chile", x = data2$Country), "Country"] <- "Chile"

data2[grepl(pattern = "^China", x = data2$Country), "Country"] <- "China"

data2[grepl(pattern = "onfirmed international spread of wild poliovirus from Pakistan", x = data2$Country), "Country"] <- "Pakistan"

data2[grepl(pattern = "Cyprus", x = data2$Country), "Country"] <- "Cyprus"

data2[grepl(pattern = "mark", x = data2$Country), "Country"] <- "Denmark"

data2[grepl(pattern = "Detection of poliovirus", x = data2$Country), "Country"] <- "Brazil"

data2[grepl(pattern = "Djibouti", x = data2$Country), "Country"] <- "Djibouti"

# domestic cats (Thailand), Situation (human)
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "domestic cats ", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "domestic cats ", x = data2$Country), ]

data2[grepl(pattern = "domestic cats ", x = data2$Country), "Country"] <- c("Thailand", "Japan", "China")

data2[grepl(pattern = "domestic cats", x = data2$Country), "Country"] <- "Germany"

# Dominica and Cuba
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Dominica and Cuba", x = data2$Country), ]

data2[grepl(pattern = "Dominica and Cuba", x = data2$Country), "Country"] <- c("Dominica", "Cuba")

# Ebola virus disease – Democratic Republic of the Congo
data2[grepl(pattern = "Ebola virus disease – Democratic Republic of the Congo", x = data2$Description), "Country"] <- "Congo Democratic Republic of the"

data2[grepl(pattern = "Liber", x = data2$Country), "Country"] <- "Liberia"
data2[grepl(pattern = "Ebola virus disease in Liberia", x = data2$Description), "Country"] <- "Liberia"

data2[grepl(pattern = "Ebola virus disease in Guinea", x = data2$Description), "Country"] <- "Guinea"

data2[grepl(pattern = "Update on polio in Equatorial Guinea", x = data2$Description), "Country"] <- "Equatorial Guinea"

data2[grepl(pattern = "Egypt", x = data2$Country), "Country"] <- "Egypt"

data2[grepl(pattern = "Salvador", x = data2$Country), "Country"] <- "El Salvador"

data2[grepl(pattern = "nterovirus D68", x = data2$Country), "Country"] <- "United States of America"

data2[grepl(pattern = "Ethiopia", x = data2$Country), "Country"] <- "Ethiopia"

# West Nile Virus Infection (WNV) in Europe
# Albania (2 cases), Greece (22 cases) Israel (6 cases), Romania (1 case) and the Russian Federation
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Europe", x = data2$Country) & grepl(pattern = "16 august 2011", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Europe", x = data2$Country) & grepl(pattern = "16 august 2011", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Europe", x = data2$Country) & grepl(pattern = "16 august 2011", x = data2$Date), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Europe", x = data2$Country) & grepl(pattern = "16 august 2011", x = data2$Date), ]

data2[grepl(pattern = "Europe", x = data2$Country) & grepl(pattern = "16 august 2011", x = data2$Date), "Country"] <- c("Albania", "Greece", "Israel", 
                                                                                                                        "Romania", "Russian Federation")

# European Region and the Americas
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "European Region and the Americas", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "European Region and the Americas", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "European Region and the Americas", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "European Region and the Americas", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "European Region and the Americas", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "European Region and the Americas", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "European Region and the Americas", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "European Region and the Americas", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "European Region and the Americas", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "European Region and the Americas", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "European Region and the Americas", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "European Region and the Americas", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "European Region and the Americas", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "European Region and the Americas", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "European Region and the Americas", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "European Region and the Americas", x = data2$Country), ]

data2[grepl(pattern = "European Region and the Americas", x = data2$Country), "Country"] <- c("Chile", "United States of America", "Austria", "Belgium", "Denmark",
                                                                                              "Finland", "France", "Germany", "Ireland", "Italy", 
                                                                                              "Netherlands", "Norway", "Portugal", "Slovenia", "Spain",
                                                                                              "Sweden", "United Kingdom of Great Britain and Northern Ireland")

# Avian influenza A(H5N1) - update 21:Global surveillance guidelines, Investigation of possible human-to-human transmission: data on second sister in family cluster in Viet Nam
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "family cluster", x = data2$Country), ]

data2[grepl(pattern = "family cluster", x = data2$Country), "Country"] <- c("Viet Nam", "China")

data2[grepl(pattern = "Ouganda", x = data2$Country), "Country"] <- "Uganda"

data2[grepl(pattern = "Gabon", x = data2$Country), "Country"] <- "Gabon"

data2[grepl(pattern = "Germany", x = data2$Country), "Country"] <- "Germany"

data2[grepl(pattern = "Ghana", x = data2$Country), "Country"] <- "Ghana"

data2[grepl(pattern = "Greece", x = data2$Country), "Country"] <- "Greece"

# Guyana, Barbados and Ecuador
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Guyana, Barbados and Ecuador", x = data2$Country), ] 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Guyana, Barbados and Ecuador", x = data2$Country), ] 

data2[grepl(pattern = "Guyana, Barbados and Ecuador", x = data2$Country), "Country"] <- c("Guyana", "Barbados", "Ecuador")

data2[grepl(pattern = "Haiti", x = data2$Country), "Country"] <- "Haiti"

data2[grepl(pattern = "Human infection with avian influenza A", x = data2$Country), "Country"] <- "China"

data2[grepl(pattern = "^humans", x = data2$Country), "Country"] <- c("Japan", "Viet Nam", "Thailand", "Thailand", "Viet Nam", 
                                                                     "Viet Nam", "Viet Nam", "Viet Nam", "Viet Nam")

# Thailand, Indonesia - update 36
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Thailand, Indonesia", x = data2$Country), ]

data2[grepl(pattern = "Thailand, Indonesia", x = data2$Country), "Country"] <- c("Thailand", "Indonesia") 

# Malaysia and Indonesia
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Malaysia and Indonesia", x = data2$Country), ]

data2[grepl(pattern = "Malaysia and Indonesia", x = data2$Country), "Country"] <- c("Malaysia", "Indonesia") 

data2[grepl(pattern = "Indonesia", x = data2$Country), "Country"] <- "Indonesia"

data2[grepl(pattern = "Iraq", x = data2$Country), "Country"] <- "Iraq"

data2[grepl(pattern = "Pakistan", x = data2$Country), "Country"] <- "Pakistan"

data2[grepl(pattern = "Israel", x = data2$Country), "Country"] <- "Israel"

data2[grepl(pattern = "Japan", x = data2$Country), "Country"] <- "Japan"

data2[grepl(pattern = "Kosovo", x = data2$Country), "Country"] <- "Kosovo"

# Laos and Thailand
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Laos and Thailand", x = data2$Country), ]

data2[grepl(pattern = "Laos and Thailand", x = data2$Country), "Country"] <- c("Thailand", "Lao People's Democratic Republic")

data2[grepl(pattern = "last outbreak area", x = data2$Country), "Country"] <- "Taiwan Province of China"

# 1998 - Cholera in Latin America and El Niño
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "tin America and El Ni", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "tin America and El Ni", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "tin America and El Ni", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "tin America and El Ni", x = data2$Country), ]

data2[grepl(pattern = "tin America and El Ni", x = data2$Country), "Country"] <- c("Bolivia", "Honduras", "Ecuador", "Peru", "Nicaragua")

data2[grepl(pattern = "Liberia", x = data2$Country), "Country"] <- "Liberia"

data2[grepl(pattern = "Portugal", x = data2$Country), "Country"] <- "Portugal"

# Malaysia and Singapore
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Malaysia and Singapore", x = data2$Country), ]

data2[grepl(pattern = "Malaysia and Singapore", x = data2$Country), "Country"] <- c("Malaysia", "Singapore")

data2[grepl(pattern = "Malaysia", x = data2$Country), "Country"] <- "Malaysia"

data2[grepl(pattern = "Mali", x = data2$Country), "Country"] <- "Mali"

# Influenza - update 113
# 13 August 2010 - Influenza H1N1 (2009) virus transmission remains locally intense in parts of India and New Zealand.
# Except in South Africa and New Zealand, overall influenza activity and rates of respiratory diseases remained low 
# in other countries of the temperate southern hemisphere (Australia, Chile, and Argentina). 
# In South Africa, active circulation of seasonal influenza H3N2 and type B viruses was observed during June and July 2010.
# Influenza H1N1 (2009) continued to circulate at low to moderate levels over the past month in the tropics of the 
# Americas (Costa Rica, Colombia, Peru, Bolivia, Brazil), West Africa (Ghana), and 
# South and Southeast Asia (India, Bangladesh, Thailand, Cambodia, Singapore, Malaysia).
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 113", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 113", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 113", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 113", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 113", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 113", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 113", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 113", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 113", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 113", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 113", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 113", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 113", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 113", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 113", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 113", x = data2$Country), ]

data2[grepl(pattern = "Influenza - update 113", x = data2$Country), "Country"] <- c("South Africa", "Ghana", 
                                                                                    "India", "Bangladesh", "Thailand", "Cambodia", "Singapore", "Malaysia",
                                                                                    "New Zealand", "Australia", 
                                                                                    "Chile", "Argentina", "Costa Rica", "Colombia", "Peru", "Bolivia", "Brazil")

# 20 August 2010 - The situation in New Zealand and India remains largely unchanged since the last update. 
# Influenza H1N1 (2009) virus transmission remains locally intense in parts of India and New Zealand.
# South Africa and New Zealand, overall influenza activity and rates of respiratory diseases remained low 
# in other countries of the temperate southern hemisphere (Australia, Chile, and Argentina). 
# In South Africa, active circulation of seasonal influenza H3N2 and type B viruses was observed during June through mid-August 2010.
# In Argentina, there are unconfirmed media reports of localized influenza outbreaks in at least one part of the country.
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 114", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 114", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 114", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 114", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 114", x = data2$Country), ]

data2[grepl(pattern = "Influenza - update 114", x = data2$Country), "Country"] <- c("India",
                                                                                    "New Zealand", "Australia", 
                                                                                    "Chile", "Argentina", 
                                                                                    "South Africa")

# 27 August 2010 - Worldwide, H1N1 2009 virus transmission remains most intense in parts of India 
# and in parts of the temperate southern hemisphere, particularly New Zealand and more recently in Australia.
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 115", x = data2$Country), ]
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Influenza - update 115", x = data2$Country), ]

data2[grepl(pattern = "Influenza - update 115", x = data2$Country), "Country"] <- c("India", "New Zealand", "Australia")

# Last update from Influenza A(H1N1) is from 29 June 2009 (update 55), we use that because have all the previous reports #
countriesAH1N1 <- c("Zimbabwe", "Djibouti", "Lesotho", "Angola", "Malawi", "Tajikistan", "Iceland", "Sudan", "Trinidad and Tobago",
                    "Congo", "Somalia", "Nigeria", "Burundi", "Saint Lucia", "Sri Lanka", "Pakistan", "Slovenia", "Romania", "Slovakia",
                    "Korea (Democratic People's Republic of)", "Austria", "Lithuania", "Latvia", "United Arab Emirates",
                    "Georgia", "Albania", "Nepal", "Armenia", 
                    "Ghana", "Zambia", "Tuvalu", "Cameroon", "Madagascar", "Mozambique", "Timor-Leste", "Pakistan", "Kiribati", "Maldives", "French Guiana", "Falkland Islands (Malvinas)", "Wallis and Futuna",
                    "Afghanistan", "Andorra", "Belize", "Bhutan", "Botswana", "Réunion", "Haiti", "Marshall Islands", "Micronesia (Federated States of)", "Namibia", 
                    "Azerbaijan", "Gabon", "Grenada", "Kazakhstan", "Moldova Republic of", "Nauru", "Eswatini", "Suriname",
                    "Bonaire Sint Eustatius and Saba", "Saint Kitts and Nevis", "Saint Vincent and the Grenadines", "Seychelles", "Solomon Islands", "Sudan", "Tonga", 
                    "Turks and Caicos Islands", "Tanzania United Republic of", "American Samoa", "Guam", "Algeria", "Antigua and Barbuda", "Argentina", "Aruba", "Australia", "Austria", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belgium", 
                    "Bermuda", "Bolivia", "Bosnia and Herzegovina", "Brazil", "Virgin Islands (British)", "Brunei Darussalam", "Bulgaria", "Cambodia", "Canada", "Cabo Verde", 
                    "Cayman Islands", "Chile", "China", "Colombia", "Cook Islands", "Costa Rica", "Côte d'Ivoire", "Croatia", "Cuba", "Cyprus", "Czechia", "Denmark", "Dominica",
                    "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Estonia", "Ethiopia", "Fiji", "Finland", "France", "French Polynesia", 
                    "Martinique", "New Caledonia", "Germany", "Greece", "Guadeloupe", "Guatemala", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran (Islamic Republic of)", "Iraq",
                    "Ireland", "Israel", "Italy","Jamaica", "Japan", "Jordan", "Kenya", "Korea Republic of", "Kuwait", "Lao People's Democratic Republic", "Latvia", "Lebanon", 
                    "Libya", "Lithuania", "Luxembourg", "Malaysia", "Mexico", "Monaco", "Montenegro", "Morocco", "Myanmar", "Nepal", "Netherlands", "New Caledonia", "Curaçao", "Sint Maarten (Dutch part)",
                    "New Zealand", "Nicaragua", "North Macedonia", "Norway", "Oman", "Palau", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Puerto Rico", "Qatar",
                    "Romania", "Russian Federation", "Samoa", "Saint Martin (French part)", "Saint Lucia", "Saudi Arabia", "Serbia", "Singapore", "Slovakia", "Slovenia", "South Africa", "Spain", 
                    "Sri Lanka", "Suriname", "Sweden", "Switzerland", "Syrian Arab Republic", "Thailand", "Trinidad and Tobago", "Tunisia", "Turkey", "Uganda", "Ukraine", "United Arab Emirates", 
                    "United Kingdom of Great Britain and Northern Ireland", "United States of America", "Uruguay", "Vanuatu", "Venezuela", "Viet Nam", "Virgin Islands (U.S.)", "Gaza", "Yemen")
countriesAH1N1 <- unique(countriesAH1N1)
countriesAH1N1 <- countriesAH1N1[order(countriesAH1N1)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesAH1N1)-1), ] <- data2[grepl(pattern = "Influenza A\\(H1N1\\) - update 55", x = data2$Country), ]

data2[grepl(pattern = "Influenza A\\(H1N1\\) - update 55", x = data2$Country), "Country"] <- countriesAH1N1

# all the influenza updates are accounted already
data2[grepl(pattern = "Influenza A\\(H1N1\\)", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "Pandemic \\(H1N1\\)", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "Hong Kong", x = data2$Country), "Country"] <- "Hong Kong"

# Human influenza A/H3N2 activity increases in many countries in central and eastern Europe - update 6
countriesah3n2 <- c("Czechia", "Finland", "Greece", "Israel", "Russian Federation", "Switzerland", "Ukraine", 
                    "Belgium", "France", "Germany", "Latvia", "Norway", "Portugal", "Spain", "United States of America",
                    "United Kingdom of Great Britain and Northern Ireland", "Canada", "Mexico", "Algeria", "Austria",
                    "Chile", "Croatia", "Hong Kong", "Hungary", "Iceland", "Japan", "Madagascar", 
                    "Serbia", "Montenegro", "Slovenia", "Thailand")

countriesah3n2 <- unique(countriesah3n2)
countriesah3n2 <- countriesah3n2[order(countriesah3n2)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesah3n2)-1), ] <- data2[grepl(pattern = "many countries and moving eastwards - update 6", x = data2$Country), ]

data2[grepl(pattern = "many countries and moving eastwards - update 6", x = data2$Description), "Country"] <- countriesah3n2

# Human influenza A(H3N2) activity remains widespread in many countries - update 7
countriesah3n2 <- c("Belgium", "Canada", "Croatia", "Czechia", "Finland", "France", "Danemark", "Germany",
                    "Israel", "Latvia", "Norway", "Portugal", "Korea Republic of", "Romania", "Switzerland",
                    "United Kingdom of Great Britain and Northern Ireland", "United States of America")

countriesah3n2 <- unique(countriesah3n2)
countriesah3n2 <- countriesah3n2[order(countriesah3n2)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesah3n2)-1), ] <- data2[grepl(pattern = "many countries - update 7", x = data2$Country), ]

data2[grepl(pattern = "many countries - update 7", x = data2$Country), "Country"] <- countriesah3n2

# Human influenza A/H3N2 activity increases in many countries in central and eastern Europe - update 8
countriesah3n2 <- c("Austria", "Belgium", "Canada", "Croatia", "France", "Danemark", "Germany", "Israel", "Italy",
                    "Japan", "Latvia", "Norway", "Romania", "Russian Federation", "Slovenia", "Sweden", "Switzerland",
                    "Ukraine", "United Kingdom of Great Britain and Northern Ireland", "United States of America",
                    "Greece", "Guyana", "Hong Kong", "Hungary", "Morocco", "Portugal")

countriesah3n2 <- unique(countriesah3n2)
countriesah3n2 <- countriesah3n2[order(countriesah3n2)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesah3n2)-1), ] <- data2[grepl(pattern = "many countries", x = data2$Country), ]

data2[grepl(pattern = "many countries", x = data2$Country), "Country"] <- countriesah3n2

# Swine flu illness in the United States and Mexico - update 2
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "States and Mexico - update 2", x = data2$Country), ]

data2[grepl(pattern = "States and Mexico - update 2", x = data2$Country), "Country"] <- c("Mexico", "United States of America")

# Swine flu illness in the United States and Mexico 
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "States and Mexico", x = data2$Country), ]

data2[grepl(pattern = "States and Mexico", x = data2$Country), "Country"] <- c("Mexico", "United States of America")

data2[grepl(pattern = "Mexico", x = data2$Country), "Country"] <- "Mexico"

data2[grepl(pattern = "Mongolia", x = data2$Country), "Country"] <- "Mongolia"

data2[grepl(pattern = "Mozambique", x = data2$Country), "Country"] <- "Mozambique"

data2[grepl(pattern = "Nepal", x = data2$Country), "Country"] <- "Nepal"

data2[grepl(pattern = "Niger \\(update\\)", x = data2$Country), "Country"] <- "Niger"

data2[grepl(pattern = "Paraguay", x = data2$Country), "Country"] <- "Paraguay"

data2[grepl(pattern = "Tanzania", x = data2$Country), "Country"] <- "Tanzania United Republic of"

data2[grepl(pattern = "Republic of Korea", x = data2$Country), "Country"] <- "Korea Republic of"

data2[grepl(pattern = "Romania", x = data2$Country), "Country"] <- "Romania"

data2[grepl(pattern = "^Sen", x = data2$Country), "Country"] <- "Senegal"

data2[grepl(pattern = "Sierra L", x = data2$Country), "Country"] <- "Sierra Leone"

data2[grepl(pattern = "Singapore", x = data2$Country), "Country"] <- "Singapore"

data2[grepl(pattern = "Somalia", x = data2$Country), "Country"] <- "Somalia"

# South Africa and Zambia - Update
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "South Africa and Zambia - Update", x = data2$Country), ]

data2[grepl(pattern = "South Africa and Zambia - Update", x = data2$Country), "Country"] <- c("South Africa", "Zambia")

# South Africa and Zambia
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "South Africa and Zambia", x = data2$Country), ]

data2[grepl(pattern = "South Africa and Zambia", x = data2$Country), "Country"] <- c("South Africa", "Zambia")

data2[grepl(pattern = "outh Af", x = data2$Country), "Country"] <- "South Africa"

data2[grepl(pattern = "outh Sudan", x = data2$Country), "Country"] <- "South Sudan"

data2[grepl(pattern = "ern Sudan", x = data2$Country), "Country"] <- "Sudan"

data2[grepl(pattern = "Spain", x = data2$Country), "Country"] <- "Spain"

data2[grepl(pattern = "^Sudan", x = data2$Country), "Country"] <- "Sudan"

data2[grepl(pattern = "Taiwan", x = data2$Country), "Country"] <- "Taiwan Province of China"

data2[grepl(pattern = "Tajikistan", x = data2$Country), "Country"] <- "Tajikistan"

data2[grepl(pattern = "Thailand", x = data2$Country), "Country"] <- "Thailand"

data2[grepl(pattern = "Cook Islands", x = data2$Country), "Country"] <- "Cook Islands"

data2[grepl(pattern = "Microne", x = data2$Country), "Country"] <- "Micronesia (Federated States of)"

data2[grepl(pattern = "Saint Martin", x = data2$Country), "Country"] <- "Saint Martin (French part)"

data2[grepl(pattern = "pines", x = data2$Country), "Country"] <- "Philippines"

data2[grepl(pattern = "Syria", x = data2$Country), "Country"] <- "Syrian Arab Republic"

data2[grepl(pattern = "jikistan", x = data2$Country), "Country"] <- "Tajikistan"

data2[grepl(pattern = "United King", x = data2$Country), "Country"] <- "United Kingdom of Great Britain and Northern Ireland"

data2[grepl(pattern = "United States", x = data2$Country), "Country"] <- "United States of America"

data2[grepl(pattern = "Leste", x = data2$Country), "Country"] <- "Timor-Leste"

data2[grepl(pattern = "Togo", x = data2$Country), "Country"] <- "Togo"

data2[grepl(pattern = "Toronto", x = data2$Country), "Country"] <- "Canada"

data2[grepl(pattern = "Turkey", x = data2$Country), "Country"] <- "Turkey"

data2[grepl(pattern = "Venezuela", x = data2$Country), "Country"] <- "Venezuela (Bolivarian Republic of)"

data2[grepl(pattern = "Bolivia", x = data2$Country), "Country"] <- "Bolivia (Plurinational State of)"

# Ebola virus disease: background and summary
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Ebola virus disease: background and summary", x = data2$Country), ]

data2[grepl(pattern = "Ebola virus disease: background and summary", x = data2$Country), "Country"] <- c("Guinea", "Liberia")

data2[grepl(pattern = "Kenya", x = data2$Country), "Country"] <- "Kenya"

# Measles outbreaks: Regions of the Americas, Europe and Africa
# France, Germany, Kyrgyzstan, Romania, Macedonia, United Kingdom
# Democratic Republic of the Congo, Nigeria, Zambia, Ethiopia
# Canada, United States, Ecuador, Brazil, Columbia, Mexico, Chile
countriesmeasles <- c("France", "Germany", "Kyrgyzstan", "Romania", "North Macedonia", 
                      "United Kingdom of Great Britain and Northern Ireland",
                      "Congo Democratic Republic of the", "Nigeria", "Zambia", "Ethiopia",
                      "United States of America", "Canada", "Ecuador", "Brazil", "Colombia", "Mexico", "Chile")

countriesmeasles <- unique(countriesmeasles)
countriesmeasles <- countriesmeasles[order(countriesmeasles)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesmeasles)-1), ] <- data2[grepl(pattern = "Measles outbreaks: Regions of the Americas, Europe and Africa", x = data2$Country), ]

data2[grepl(pattern = "Measles outbreaks: Regions of the Americas, Europe and Africa", x = data2$Description), "Country"] <- countriesmeasles

# Middle East respiratory syndrome coronavirus (MERS-CoV)
data2[grepl(pattern = "Middle East respiratory syndrome", x = data2$Country) & grepl(pattern = "14 april 2021", x = data2$Date), "Country"] <- "Saudi Arabia"

data2[grepl(pattern = "Middle East respiratory syndrome", x = data2$Country) , "Country"][29:37] <- c("Saudi Arabia", "United Arab Emirates",
                                                                                                     "Qatar", "Kuwait", "Spain", "Italy", 
                                                                                                     "Oman", "Italy", "France")
data2[grepl(pattern = "Middle East respiratory syndrome", x = data2$Country), "Country"] <- NA

data2[grepl(pattern = "Monkeypox in Central African Republic", x = data2$Description), "Country"] <- "Central African Republic"

data2[grepl(pattern = "mark", x = data2$Country), "Country"] <- "Denmark"

data2[grepl(pattern = "monkeys", x = data2$Country), "Country"] <- "United States of America"

# Human influenza epidemic spreads to more countries in northern hemisphere - update 3
countriesinfluenza <- c("Austria", "Canada", "Denmark", "Finland", "Morocco", "Norway",
                        "Portugal", "Korea Republic of", "Russian Federation", "Spain",
                        "Switzerland", "United Kingdom of Great Britain and Northern Ireland",
                        "United States of America", "Czechia", "Germany", "Hong Kong", "Italy",
                        "Thailand", "Ukraine")

countriesinfluenza <- unique(countriesinfluenza)
countriesinfluenza <- countriesinfluenza[order(countriesinfluenza)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesinfluenza)-1), ] <- data2[grepl(pattern = "northern hemisphere - update 3", x = data2$Country), ]

data2[grepl(pattern = "northern hemisphere - update 3", x = data2$Country), "Country"] <- countriesinfluenza

# Human influenza activity further increases and spreads to more countries in northern hemisphere - update 4
countriesinfluenza <- c("Canada", "Denmark", "Finland", "France", "Hong Kong", "Italy",
                        "Japan", "Latvia", "Norway", "Portugal", "Russian Federation",
                        "Spain", "Switzerland", "Ukraine", 
                        "United Kingdom of Great Britain and Northern Ireland",
                        "United States of America", "Austria", "Chile", "Germany", "Hungary",
                        "Iceland", "Ireland")

countriesinfluenza <- unique(countriesinfluenza)
countriesinfluenza <- countriesinfluenza[order(countriesinfluenza)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesinfluenza)-1), ] <- data2[grepl(pattern = "hemisphere - update 4", x = data2$Country), ]

data2[grepl(pattern = "hemisphere - update 4", x = data2$Country), "Country"] <- countriesinfluenza

# Widespread human influenza activity persists in northern hemisphere - update 5
countriesinfluenza <- c("Canada", "Czechia", "Denmark", "France", "Italy", "Norway", "Portugal", 
                        "Russian Federation", "Spain", "Switzerland", "Ukraine", 
                        "United Kingdom of Great Britain and Northern Ireland",
                        "United States of America", "Austria", "Chile", "Hong Kong", "Hungary",
                        "Iceland", "India", "Japan", "Latvia", "Thailand", "Tunisia")

countriesinfluenza <- unique(countriesinfluenza)
countriesinfluenza <- countriesinfluenza[order(countriesinfluenza)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesinfluenza)-1), ] <- data2[grepl(pattern = "hemisphere - update 5", x = data2$Country), ]

data2[grepl(pattern = "hemisphere - update 5", x = data2$Country), "Country"] <- countriesinfluenza

# Human influenza A/H3N2 epidemic continues in northern hemisphere update 2
countriesinfluenza <- c("Canada", "France", "Israel", "Italy", "Latvia", "Norway", 
                        "Spain", "United Kingdom of Great Britain and Northern Ireland",
                        "United States of America", "Chile", "Denmark", "Guyana",
                        "Switzerland")

countriesinfluenza <- unique(countriesinfluenza)
countriesinfluenza <- countriesinfluenza[order(countriesinfluenza)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesinfluenza)-1), ] <- data2[grepl(pattern = "hemisphere update 2", x = data2$Country), ]

data2[grepl(pattern = "hemisphere update 2", x = data2$Country), "Country"] <- countriesinfluenza


data2[grepl(pattern = "Operational readiness and preparedness", x = data2$Country), "Country"] <- "Congo Democratic Republic of the"

data2[grepl(pattern = "pigs$", x = data2$Country), "Country"] <- "China"

data2[grepl(pattern = "pigs and humans", x = data2$Country), "Country"] <- "Philippines"

data2[grepl(pattern = "Poliovirus", x = data2$Country) & data2$Year == 2015, "Country"] <- "Madagascar"

# Poliovirus in South Sudan and Madagascar
data2[nrow(data2)+1, ] <- data2[grepl(pattern = "Poliovirus", x = data2$Country) & data2$Year == 2014, ]
data2[grepl(pattern = "Poliovirus", x = data2$Country) & data2$Year == 2014, "Country"] <- c("Madagascar", "South Sudan")

data2[grepl(pattern = "poultry, Development of an H5N1 vaccine for humans", x = data2$Country), "Country"] <- "China"

data2[grepl(pattern = "outbreak in poultry in the Democratic Peopl", x = data2$Description), "Country"] <- "Korea (Democratic People's Republic of)"

data2[grepl(pattern = "poultry$", x = data2$Country), "Country"] <- "Hong Kong"

data2[grepl(pattern = "Rapidly growing outbreak of meningococcal disease", x = data2$Country), "Country"] <- "Niger"

# Zika virus infection - Region of the Americas
# Costa Rica, Curaçao, Jamaica and Nicaragua
countriesnzika <- c("Costa Rica", "Curaçao", "Jamaica", "Nicaragua")

countriesnzika <- unique(countriesnzika)
countriesnzika <- countriesnzika[order(countriesnzika)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesnzika)-1), ] <- data2[grepl(pattern = "Region of the Americas", x = data2$Description), ]

data2[grepl(pattern = "Region of the Americas", x = data2$Description), "Country"] <- countriesnzika

data2[grepl(pattern = "Republic of the Sudan", x = data2$Country), "Country"] <- "Sudan"

data2[grepl(pattern = "Panama", x = data2$Country), "Country"] <- "Panama"

data2[grepl(pattern = "Rift Valley fever in Niger", x = data2$Description), "Country"] <- "Niger"
data2[grepl(pattern = "Rift Valley fever in China", x = data2$Description), "Country"] <- "China"
data2[grepl(pattern = "Rift Valley Fever in Niger", x = data2$Description), "Country"] <- "Niger"

data2[grepl(pattern = "COVID-19, virus identified", x = data2$icd104n), "Country"] <- NA

data2[grepl(pattern = "Outbreak of illness in schools in Angola", x = data2$Description), "Country"] <- "Angola"

# Novel coronavirus infection 2013
countriesnCoV <- c("Jordan", "Qatar", "Saudi Arabia", "United Arab Emirates", "France", "Germany", "Tunisia",
                   "United Kingdom of Great Britain and Northern Ireland")

countriesnCoV <- unique(countriesnCoV)
countriesnCoV <- countriesnCoV[order(countriesnCoV)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesnCoV)-1), ] <- data2[grepl(pattern = "Novel coronavirus infection", x = data2$Description) & data2$Date == "11 february 2013", ]

data2[grepl(pattern = "Novel coronavirus infection", x = data2$Country) & data2$Year == 2013, "Country"] <- NA

data2[grepl(pattern = "Novel coronavirus infection", x = data2$Description) & data2$Date == "11 february 2013", "Country"] <- countriesnCoV

# Novel coronavirus infection 2012
countriesnCoV <- c("Qatar", "Saudi Arabia", "United Kingdom of Great Britain and Northern Ireland")

countriesnCoV <- unique(countriesnCoV)
countriesnCoV <- countriesnCoV[order(countriesnCoV)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesnCoV)-1), ] <- data2[grepl(pattern = "Novel coronavirus infection", x = data2$Description) & data2$Date == "25 september 2012", ]

data2[grepl(pattern = "Novel coronavirus infection", x = data2$Country) & data2$Year == 2012, "Country"] <- NA

data2[grepl(pattern = "Novel coronavirus infection", x = data2$Description) & data2$Date == "25 september 2012", "Country"] <- countriesnCoV

# Severe Acute Respiratory Syndrome (SARS) - multi-country outbreak
countriessars <- c("China", "Hong Kong", "Singapore", "Viet Nam", "Canada", "United States of America",
                   "Malaysia", "Brazil", "Germany", "Thailand", "Philippines", "Indonesia", "Taiwan Province of China",
                   "Switzerland", "Australia", "Belgium", "France", "Ireland", "Romania", 
                   "United Kingdom of Great Britain and Northern Ireland", "Mongolia", "Japan", "Sweden", "India",
                   "Macao", "Poland", "Bulgaria", "Slovenia", "Spain", "Colombia")

countriessars <- unique(countriessars)
countriessars <- countriessars[order(countriessars)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriessars)-1), ] <- data2[grepl(pattern = "Severe Acute Respiratory Syndrome", x = data2$Country) & data2$Date == "20 may 2003", ]

data2[grepl(pattern = "Severe Acute Respiratory Syndrome", x = data2$Country) & data2$Date != "20 may 2003", "Country"] <- NA

data2[grepl(pattern = "Severe Acute Respiratory Syndrome", x = data2$Country) & data2$Date == "20 may 2003", "Country"] <- countriessars

# 1999 - Sylvatic yellow fever in South America
countriessylvatic <- c("Brazil", "Bolivia (Plurinational State of)", "Colombia", "Peru")

countriessylvatic <- unique(countriessylvatic)
countriessylvatic <- countriessylvatic[order(countriessylvatic)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriessylvatic)-1), ] <- data2[grepl(pattern = "South America", x = data2$Country), ]

data2[grepl(pattern = "South America", x = data2$Country), "Country"] <- countriessylvatic

data2[grepl(pattern = "Statement by WHO Director", x = data2$Country), "Country"] <- NA

# Swine influenza
countriesswine <- c("Canada", "United Kingdom of Great Britain and Northern Ireland",
                    "United States of America", "Mexico", "New Zealand", "Israel",
                    "Spain")

countriesswine <- unique(countriesswine)
countriesswine <- countriesswine[order(countriesswine)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesswine)-1), ] <- data2[grepl(pattern = "Swine influenza - update 3", x = data2$Country), ]

data2[grepl(pattern = "Swine influenza - update 3", x = data2$Country), "Country"] <- countriesswine

data2[grepl(pattern = "Swine influenza - update 4", x = data2$Country), "Country"] <- NA

data2[grepl(pattern = "Test Rift valley", x = data2$Country), "Country"] <- "Iraq"

# Meningococcal disease: 2013 epidemic season in the African Meningitis Belt
countriesmening <- c("Guinea", "South Sudan", "Benin", "Burkina Faso", "Nigeria")

countriesmening <- unique(countriesmening)
countriesmening <- countriesmening[order(countriesmening)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesmening)-1), ] <- data2[grepl(pattern = "2013 epidemic season in the African Meningitis Belt", x = data2$Description), ]

data2[grepl(pattern = "2013 epidemic season in the African Meningitis Belt", x = data2$Description), "Country"] <- countriesmening


# Meningococcal disease: situation in the African Meningitis Belt 2012
countriesmening <- c("Benin", "Burkina Faso", "Chad", "Ghana", "Sudan", "Côte d'Ivoire")

countriesmening <- unique(countriesmening)
countriesmening <- countriesmening[order(countriesmening)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesmening)-1), ] <- data2[grepl(pattern = "Meningococcal disease", x = data2$Description) & data2$Date == "23 march 2012", ]

data2[grepl(pattern = "Meningococcal disease", x = data2$Description) & data2$Date == "23 march 2012", "Country"] <- countriesmening

data2[grepl(pattern = "Meningococcal disease", x = data2$Description) & data2$Date == "24 may 2012", "Country"] <- NA

# Meningococcal disease: situation in the African Meningitis Belt 2001
countriesmening <- c("Benin", "Ethiopia", "Chad", "Burkina Faso", "Cameroon", "Central African Republic", "Gambia", "Niger")

countriesmening <- unique(countriesmening)
countriesmening <- countriesmening[order(countriesmening)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesmening)-1), ] <- data2[grepl(pattern = "Meningitis Belt", x = data2$Description) & data2$Date == "20 february 2001", ]

data2[grepl(pattern = "Meningitis Belt", x = data2$Description) & data2$Date == "20 february 2001", "Country"] <- countriesmening

data2[grepl(pattern = "Meningitis Belt", x = data2$Description) & data2$Year == 2001 & data2$Date != "20 february 2001", "Country"] <- NA

# Meningococcal disease: situation in the African Meningitis Belt 2002
countriesmening <- c("Burkina Faso", "Niger")

countriesmening <- unique(countriesmening)
countriesmening <- countriesmening[order(countriesmening)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesmening)-1), ] <- data2[grepl(pattern = "Meningitis Belt", x = data2$Country) & data2$Year == 2002, ]

data2[grepl(pattern = "Meningitis Belt", x = data2$Country) & data2$Year == 2002, "Country"] <- countriesmening

# Meningococcal disease: situation in the African Meningitis Belt 2006
countriesmening <- c("Burkina Faso", "Côte d'Ivoire", "Niger", "Kenya", "Mali", "Sudan", "Uganda")

countriesmening <- unique(countriesmening)
countriesmening <- countriesmening[order(countriesmening)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesmening)-1), ] <- data2[grepl(pattern = "Meningitis Belt", x = data2$Country) & data2$Year == 2006, ]

data2[grepl(pattern = "Meningitis Belt", x = data2$Country) & data2$Year == 2006, "Country"] <- countriesmening

# Meningococcal disease: situation in the African Meningitis Belt 2008
countriesmening <- c("Burkina Faso", "Central African Republic", "Congo Democratic Republic of the", 
                     "Côte d'Ivoire", "Benin", "Ethiopia", "Ghana", "Mali", "Niger", "Nigeria", "Togo")

countriesmening <- unique(countriesmening)
countriesmening <- countriesmening[order(countriesmening)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesmening)-1), ] <- data2[grepl(pattern = "Meningitis Belt", x = data2$Country) & data2$Year == 2008, ]

data2[grepl(pattern = "Meningitis Belt", x = data2$Country) & data2$Year == 2008, "Country"] <- countriesmening

# Meningococcal disease: situation in the African Meningitis Belt 2009
countriesmening <- c("Niger", "Nigeria")

countriesmening <- unique(countriesmening)
countriesmening <- countriesmening[order(countriesmening)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesmening)-1), ] <- data2[grepl(pattern = "Meningitis Belt", x = data2$Country) & data2$Year == 2009, ]

data2[grepl(pattern = "Meningitis Belt", x = data2$Country) & data2$Year == 2009, "Country"] <- countriesmening

data2[grepl(pattern = "Hantavirus Pulmonary Syndrome in the Americas", x = data2$Description), "Country"] <- "Argentina"

# Dengue in the Americas
data2[grepl(pattern = "Dengue in the Americas", x = data2$Description), "Country"] <- NA


# Measles – The Americas
countriesmeasles <- c("Canada", "United States of America", "Mexico", "Brazil")

countriesmeasles <- unique(countriesmeasles)
countriesmeasles <- countriesmeasles[order(countriesmeasles)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesmeasles)-1), ] <- data2[grepl(pattern = "Measles - The Americas", x = data2$Description), ]

data2[grepl(pattern = "Measles - The Americas", x = data2$Description), "Country"] <- countriesmeasles

data2[grepl(pattern = "the Horn of Africa - update", x = data2$Country), "Country"] <- "Rwanda"

# Early start of human influenza activity in the northern hemisphere
# Canada, Finland, France, Israel, Norway, Portugal, Spain, the United Kingdom (UK) and the United States of America (USA)
countriesinfluenza <- c("Canada", "United States of America", "Finland", "France", "Israel", "Norway", "Portugal",
                        "Spain", "United Kingdom of Great Britain and Northern Ireland")

countriesinfluenza <- unique(countriesinfluenza)
countriesinfluenza <- countriesinfluenza[order(countriesinfluenza)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesinfluenza)-1), ] <- data2[grepl(pattern = "the northern hemisphere", x = data2$Country), ]

data2[grepl(pattern = "the northern hemisphere", x = data2$Country), "Country"] <- countriesinfluenza

# 1996 - Yellow fever in travellers
countriesyellowf <- c("Switzerland", "United States of America")

countriesyellowf <- unique(countriesyellowf)
countriesyellowf <- countriesyellowf[order(countriesyellowf)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesyellowf)-1), ] <- data2[grepl(pattern = "Yellow fever in traveller", x = data2$Description), ]

data2[grepl(pattern = "Yellow fever in traveller", x = data2$Description), "Country"] <- countriesyellowf

data2[grepl(pattern = "two previously reported cases - update 4", x = data2$Country), "Country"] <- "China"

data2[grepl(pattern = "Poliovirus in Cameroon - update", x = data2$Description), "Country"] <- "Cameroon"

# Ebola virus disease, West Africa
countriesebola <- c("Sierra Leone", "Guinea", "Liberia")

countriesebola <- unique(countriesebola)
countriesebola <- countriesebola[order(countriesebola)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesebola)-1), ] <- data2[grepl(pattern = "Ebola virus disease, West Africa", x = data2$Description) & data2$Date == "1 april 2014", ]

data2[grepl(pattern = "Ebola virus disease, West Africa", x = data2$Description) & data2$Date != "1 april 2014", "Country"] <- NA

data2[grepl(pattern = "Ebola virus disease, West Africa", x = data2$Description) & data2$Date == "1 april 2014", "Country"] <- countriesebola

# Human infection with avian influenza A(H5N1) virus
countriesah5n1 <- c("China", "Canada", "Hong Kong", "Malaysia")

countriesah5n1 <- unique(countriesah5n1)
countriesah5n1 <- countriesah5n1[order(countriesah5n1)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesah5n1)-1), ] <- data2[grepl(pattern = "Human infection with avian influenza A\\(H7N9\\) virus", x = data2$Description) & data2$Date == "6 january 2014", ]

data2[grepl(pattern = "Human infection with avian influenza A\\(H7N9\\) virus", x = data2$Description) & data2$Country == "update" & data2$Date != "6 january 2014" & data2$Year == 2014, "Country"] <- NA

data2[grepl(pattern = "Human infection with avian influenza A\\(H7N9\\) virus", x = data2$Description) & data2$Date == "6 january 2014", "Country"] <- countriesah5n1

# Middle East respiratory syndrome coronavirus (MERS-CoV) 
countriesmers <- c("United Arab Emirates", "Jordan", "Saudi Arabia", "Malaysia", "Greece", "Egypt", "United States of America",
                   "Yemen", "Netherlands", "Iran (Islamic Republic of)", "Algeria")

countriesmers <- unique(countriesmers)
countriesmers <- countriesmers[order(countriesmers)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesmers)-1), ] <- data2[grepl(pattern = "MERS-CoV", x = data2$Description) & data2$Date == "3 january 2014", ]

data2[grepl(pattern = "MERS-CoV", x = data2$Description) & data2$Country == "update" & data2$Date != "3 january 2014" & data2$Year == 2014, "Country"] <- NA

data2[grepl(pattern = "MERS-CoV", x = data2$Description) & data2$Date == "3 january 2014", "Country"] <- countriesmers

data2[grepl(pattern = "Update on polio", x = data2$Country) , "Country"] <- "Cameroon"

data2[grepl(pattern = "Polio outbreak in the Middle East - update", x = data2$Description) , "Country"] <- "Syrian Arab Republic"

data2[grepl(pattern = "Human infection with avian influenza A\\(H5N1\\) virus - update", x = data2$Description) , "Country"] <- "Canada"

data2[grepl(pattern = "Update 95 - SARS: Chronology of a serial killer", x = data2$Description) , "Country"] <- NA

data2[grepl(pattern = "Update 92 - Chronology of travel recommendations, areas with local transmission", x = data2$Description) , "Country"] <- NA

data2[grepl(pattern = "Update 91 - SARS research: the effect of patents and patent applications", x = data2$Description) , "Country"] <- NA

data2[grepl(pattern = "Update 89 - What happens if SARS returns", x = data2$Description) , "Country"] <- NA

data2[grepl(pattern = "Update 84 - Can SARS be eradicated or eliminated", x = data2$Description) , "Country"] <- NA

data2[grepl(pattern = "Update 83 - One hundred days into the outbreak", x = data2$Description) , "Country"] <- NA

# Update 73 - No new deaths, but vigilance needed for imported cases
countriessars <- c("Canada", "Hong Kong", "Taiwan Province of China", "Germany", "United States of America")

countriessars <- unique(countriessars)
countriessars <- countriessars[order(countriessars)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriessars)-1), ] <- data2[grepl(pattern = "Update 73 - No new deaths, but vigilance needed for imported cases", x = data2$Description), ]

data2[grepl(pattern = "Update 73 - No new deaths, but vigilance needed for imported cases", x = data2$Description), "Country"] <- countriessars

data2[grepl(pattern = "Update 73 - No new deaths, but vigilance needed for imported cases", x = data2$Description), c("icd10c", "icd103c", "icd104c", "icd10n", "icd103n", "icd104n")] <- c(rep("U00-U49", length(countriessars)), rep("U04", length(countriessars)), rep("U049", length(countriessars)), rep("Codes for special purposes", length(countriessars)), rep("Provisional assignment of new diseases of uncertain etiology or emergency use", length(countriessars)), rep("Severe acute respiratory syndrome [SARS]", length(countriessars)))

# 2001 - Cholera in West Africa - Update
countriecholera <- c("Burkina Faso", "Guinea", "Côte d'Ivoire")

countriecholera <- unique(countriecholera)
countriecholera <- countriecholera[order(countriecholera)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriecholera)-1), ] <- data2[grepl(pattern = "2001 - Cholera in West Africa - Update", x = data2$Description), ]

data2[grepl(pattern = "2001 - Cholera in West Africa - Update", x = data2$Description), "Country"] <- countriecholera

# 2005 - Cholera in West Africa - Update
countriecholera <- c("Benin", "Burkina Faso", "Guinea", "Guinea-Bissau", "Mali", "Mauritania", "Niger", "Senegal")

countriecholera <- unique(countriecholera)
countriecholera <- countriecholera[order(countriecholera)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriecholera)-1), ] <- data2[grepl(pattern = "West Africa - update", x = data2$Country), ]

data2[grepl(pattern = "West Africa - update", x = data2$Country), "Country"] <- countriecholera


# 1997 - Meningitis in West Africa
countriemeningitis <- c("Benin", "Burkina Faso", "Ghana", "Mali", "Gambia", "Rwanda", "Togo", "Niger", "Senegal")

countriemeningitis <- unique(countriemeningitis)
countriemeningitis <- countriemeningitis[order(countriemeningitis)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriemeningitis)-1), ] <- data2[grepl(pattern = "Meningitis in West Africa", x = data2$Description) & data2$Date == "31 january 1997", ]

data2[grepl(pattern = "1997 - Meningitis in West Africa", x = data2$Description) & data2$Date != "31 january 1997", "Country"] <- NA

data2[grepl(pattern = "1997 - Meningitis in West Africa", x = data2$Description) & data2$Date == "31 january 1997", "Country"] <- countriemeningitis

data2[grepl(pattern = "1998 - Cholera in West Africa", x = data2$Description), "Country"] <- NA

# 2001 - Cholera in West Africa
countriescholera <- c("Burkina Faso", "Niger", "Côte d'Ivoire")

countriescholera <- unique(countriescholera)
countriescholera <- countriescholera[order(countriescholera)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriescholera)-1), ] <- data2[grepl(pattern = "2001 - Cholera in West Africa", x = data2$Description) & data2$Country == "West Africa", ]

data2[grepl(pattern = "2001 - Cholera in West Africa", x = data2$Description) & data2$Country == "West Africa", "Country"] <- countriescholera

# 2005 - Cholera in West Africa
countriescholera <- c("Burkina Faso", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Mauritania", "Niger", "Senegal")

countriescholera <- unique(countriescholera)
countriescholera <- countriescholera[order(countriescholera)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriescholera)-1), ] <- data2[grepl(pattern = "^Cholera in West Africa$", x = data2$Description) & data2$Country == "West Africa", ]

data2[grepl(pattern = "^Cholera in West Africa$", x = data2$Description) & data2$Country == "West Africa", "Country"] <- countriescholera

# Ebola virus disease outbreak – west Africa
countriesebola <- c("Guinea", "Liberia", "Sierra Leone", "Nigeria", "Senegal")

countriesebola <- unique(countriesebola)
countriesebola <- countriesebola[order(countriesebola)]

data2[(nrow(data2)+1):(nrow(data2)+length(countriesebola)-1), ] <- data2[grepl(pattern = "Ebola virus disease", x = data2$Description) & grepl(pattern = "est Africa", x = data2$Country) & data2$Date == "4 august 2014", ]

data2[grepl(pattern = "Ebola virus disease", x = data2$Description) & grepl(pattern = "est Africa", x = data2$Country) & data2$Date != "4 august 2014", "Country"] <- NA

data2[grepl(pattern = "Ebola virus disease", x = data2$Description) & grepl(pattern = "est Africa", x = data2$Country) & data2$Date == "4 august 2014", "Country"] <- countriesebola

data2[grepl(pattern = "Wild polio and vaccine derived polio", x = data2$Description), "Country"] <- "Nigeria"

data2[grepl(pattern = "Yellow fever", x = data2$Country), "Country"] <- "Congo Democratic Republic of the"

data2[grepl(pattern = "the Great Lakes Region", x = data2$Country), "Country"] <- "Rwanda"

data2[grepl(pattern = "^Update", x = data2$Description) & is.na(data2$icd10n), ]
data2[grepl(pattern = "^Update", x = data2$Description) & is.na(data2$icd10n), "icd10n"] <- "Codes for special purposes"
data2[grepl(pattern = "^Update", x = data2$Description) & is.na(data2$icd103n), "icd103n"] <-"Provisional assignment of new diseases of uncertain etiology or emergency use"
data2[grepl(pattern = "^Update", x = data2$Description) & is.na(data2$icd104n), "icd104n"] <- "Severe acute respiratory syndrome [SARS]"
data2[grepl(pattern = "^Update", x = data2$Description) & is.na(data2$icd10c), "icd10c"] <- "U00-U49"
data2[grepl(pattern = "^Update", x = data2$Description) & is.na(data2$icd103c), "icd103c"] <- "U04"
data2[grepl(pattern = "^Update", x = data2$Description) & is.na(data2$icd104c), "icd104c"] <- "U049"

data2[grepl(pattern = "Health conditions for travellers", x = data2$Description) & is.na(data2$icd10c), ]
data2[grepl(pattern = "Health conditions for travellers", x = data2$Description) & is.na(data2$icd10c), "icd10n"] <- "Arthropod-borne viral fevers and viral haemorrhagic fevers"
data2[grepl(pattern = "Health conditions for travellers", x = data2$Description) & is.na(data2$icd10c), "icd103n"] <- "Yellow fever"
data2[grepl(pattern = "Health conditions for travellers", x = data2$Description) & is.na(data2$icd10c), "icd104n"] <- "Yellow fever, unspecified"
data2[grepl(pattern = "Health conditions for travellers", x = data2$Description) & is.na(data2$icd10c), "icd10c"] <- "A92-A99"
data2[grepl(pattern = "Health conditions for travellers", x = data2$Description) & is.na(data2$icd10c), "icd103c"] <- "A95"
data2[grepl(pattern = "Health conditions for travellers", x = data2$Description) & is.na(data2$icd10c), "icd104c"] <- "A959"

data2[grepl(pattern = "Health situation in Rwandan", x = data2$Description) & is.na(data2$icd10c), ]
data2[grepl(pattern = "Health situation in Rwandan", x = data2$Description) & is.na(data2$icd10n), "icd10n"] <- "Intestinal infectious diseases"
data2[grepl(pattern = "Health situation in Rwandan", x = data2$Description) & is.na(data2$icd103n), "icd103n"] <- "Cholera"
data2[grepl(pattern = "Health situation in Rwandan", x = data2$Description) & is.na(data2$icd104n), "icd104n"] <- "Classical cholera"
data2[grepl(pattern = "Health situation in Rwandan", x = data2$Description) & is.na(data2$icd10c), "icd10c"] <- "A00-A09"
data2[grepl(pattern = "Health situation in Rwandan", x = data2$Description) & is.na(data2$icd103c), "icd103c"] <- "A00"
data2[grepl(pattern = "Health situation in Rwandan", x = data2$Description) & is.na(data2$icd104c), "icd104c"] <- "A000"

# The composition for the influenza vaccine for the 1997- 1998 season was announced at a meeting of international 
# experts that was held at the World Health Organization (WHO) headquarters in Geneva on 19 February.
data2[grepl(pattern = "Influenza vaccine for 1997-1998 season", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "African Region", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "Health conditions for travellers", x = data2$Description), 
      c("Country")] <- NA

data2[grepl(pattern = "International Travel and Health", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "1997 - Monkeypox", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "Rabies post-exposure treatment", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "Transmissible spongiform encephalopathies", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "997 - Surveillance Standards", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "Vancomycin resistant Staphylococcus aureus", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "1998 - Hurricane Mitch", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "1998 -cholera - European Commision lifts ban on East African fresh fish imports", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "2000 - Acute haemorrhagic fever syndrome - Update 2", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "a world altered by SARS", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "Asia: altered role of domestic ducks", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "Asia: need for a long-term response, comparison with previous outbreaks", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "Asia\\.", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "Asia", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "vian influenza - Current evaluation of risks to humans from H5N1 following recent reports", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "Necessary precautions to prevent human infection of H5N1, need for virus sharing", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = " epidemiology of human H5N1 cases reported to WHO", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "spread of the virus to new countries", x = data2$Country), 
      c("Country")] <- NA

data2[grepl(pattern = "^Africa", x = data2$Country) & grepl(pattern = "9 january 1998", x = data2$Date), 
      c("Country")] <- NA

data2[grepl(pattern = "Medical Impact of Use of Antimicrobial drugs in Food Animals", x = data2$Description), 
      c("Country")] <- NA

# Update 86 - Hong Kong removed from list of areas with local transmission
# Update 85 - WHO case definition for diagnostic purposes, Hong Kong close to being removed from list of areas with local transmission
data2[grepl(pattern = "removed from list of areas with local transmission", x = data2$Description), 
      c("Country")] <- NA

# 10 September 2010 - On August 10 the WHO Director-General Dr Margaret Chan announced that the 
# H1N1 influenza event has moved into the post-pandemic period
data2[grepl(pattern = "nfluenza updates", x = data2$Description), 
      c("Country")] <- NA

# Update 93 - Toronto removed from list of areas with recent local transmission
data2[grepl(pattern = "Update 93 - Toronto removed from list of areas with recent local transmission", x = data2$Description), 
      c("Country")] <- NA

# "Toronto, interpretation of “areas with recent local transmission”"            
data2[grepl(pattern = "areas with recent local transmission", x = data2$Description), 
      c("Country")] <- NA

#  Silicone implants          
data2[grepl(pattern = "Silicone implants", x = data2$Description), 
      c("Country")] <- NA

#  Sexposure treatment          
data2[grepl(pattern = "exposure treatment", x = data2$Description), 
      c("Country")] <- NA

#  Cholera in Africa         
data2[grepl(pattern = "1998 - Cholera in Africa", x = data2$Description), 
      c("Country")] <- NA

data2 <- data2[order(data2$ID), ]
rownames(data2) <- 1:nrow(data2)

# DON without information on breakouts 
isocountries[grepl(pattern = "Namibia", isocountries$Country), "iso2"] <- "NA"

data3 <- merge(data2, isocountries, by = "Country", all.x = TRUE)

data3 <- data3[!is.na(data3$Country), ]

data3 <- data3[!is.na(data3$icd10c), ] # 3185 observations

data3$key <- paste0(data3$iso3, data3$Year, data3$icd104c) # to identify all the DONs for each outbreak

data3$key <- as.factor(data3$key)

data3$DONs <- NA
for(don in levels(data3$key)){
  dons <- paste(data3[data3$key == don, "ID"], sep = ", ")
  dons <- paste(unlist(dons), collapse = ", ")
  data3[data3$key == don, "DONs"] <- dons
}

data3 <- data3[order(data3$ID), ]
data3a <- data3
save(data3a, file = "data3a.RData")

data4 <- unique(data3[, c("Country", "iso2", "iso3", "icd10c", "icd103c", "icd104c", "icd10n", "icd103n", "icd104n", "Year", "DONs")])

data4 <- data4[!grepl(pattern = "X", data4$icd10c), ] # Deleting all other than diseases events
data4 <- data4[!grepl(pattern = "D", data4$icd10c), ] # Deleting all other than diseases events
data4 <- data4[!grepl(pattern = "G", data4$icd10c), ] # Deleting all other than diseases events
data4 <- data4[!grepl(pattern = "Q", data4$icd10c), ] # Deleting all other than diseases events
# 1509 obs

## ICD11 ##
icd11 <- read.csv("icd1011.csv")

data4 <- merge(data4[, !colnames(data4) %in% c("icd10n", "icd103n", "icd104n")], icd11, by = c("icd10c", "icd103c", "icd104c"))

# save(data2, file = "DONall.RData")

save(data3, file = "DONxc.RData")

DONsOutbreaks <- data4[, c("Country", "iso2", "iso3", "Year", "icd10n", "icd103n", "icd104n", "icd10c", "icd103c", "icd104c", "icd11c1", "icd11c2", "icd11c3", "icd11l1", "icd11l2", "icd11l3", "Disease", "DONs", "Definition")]
save(DONsOutbreaks, file = "DONsOutbreaks.RData")

levels(factor(DONsOutbreaks$Country)) # Number of countries
levels(factor(DONsOutbreaks$icd104n)) # Number of diseases

#### Corona Dashboard ####
coronaOut <- read.csv("WHO-COVID-19-global-data.csv")
coronaOut$Year <- substr(coronaOut$Date_reported, 7, 10)

coronaOut <- aggregate(data = coronaOut, New_cases ~ Country_code + Country + Year, sum)
coronaOut <- coronaOut[coronaOut$New_cases > 0, ]

coronaOut$icd10n <- "Provisional assignment of new diseases of uncertain etiology or emergency use"
coronaOut$icd103n <- "Emergency use of U07"
coronaOut$icd104n <- "COVID-19, virus identified"
coronaOut$icd10c <- "U00-U49"
coronaOut$icd103c <- "U07"
coronaOut$icd104c <- "U071"
coronaOut$icd11c1 <- "25"
coronaOut$icd11c2 <- "RA01"
coronaOut$icd11c3 <- "25RA01"
coronaOut$icd11l1 <- "Codes for special purposes"
coronaOut$icd11l2 <- "International provisional assignment of new diseases of uncertain aetiology and emergency use"
coronaOut$icd11l3 <- "COVID-19"
coronaOut$Disease <- "COVID-19"
coronaOut$DONs <- "Coronavirus dashboard"
coronaOut$Definition <- "Infectious disease caused by the SARS-CoV-2 virus."
colnames(coronaOut)[1] <- "iso2"
coronaOut <- merge(coronaOut[, c(1, 3, 5:19)], isocountries, by = "iso2", all.x = TRUE)

## Bonaire Sint Eustatius and Saba together in the iso 
coronaOut[!is.na(coronaOut$iso2) & coronaOut$iso2 == "XA", "Country"] <- "Bonaire Sint Eustatius and Saba"
coronaOut[!is.na(coronaOut$iso2) & coronaOut$iso2 == "XB", "Country"] <- "Bonaire Sint Eustatius and Saba"
coronaOut[!is.na(coronaOut$iso2) & coronaOut$iso2 == "XC", "Country"] <- "Bonaire Sint Eustatius and Saba"
coronaOut[!is.na(coronaOut$iso2) & coronaOut$iso2 == "XA", "iso2"] <- "BQ"
coronaOut[!is.na(coronaOut$iso2) & coronaOut$iso2 == "XB", "iso2"] <- "BQ"
coronaOut[!is.na(coronaOut$iso2) & coronaOut$iso2 == "XC", "iso2"] <- "BQ"
coronaOut[!is.na(coronaOut$iso2) & coronaOut$iso2 == "XA", "iso2"] <- "BES"
coronaOut[!is.na(coronaOut$iso2) & coronaOut$iso2 == "XB", "iso2"] <- "BES"
coronaOut[!is.na(coronaOut$iso2) & coronaOut$iso2 == "XC", "iso2"] <- "BES"

COVIDOutbreaks <- coronaOut[, colnames(DONsOutbreaks)]
COVIDOutbreaks <- COVIDOutbreaks[!is.na(COVIDOutbreaks$Country),]
save(COVIDOutbreaks, file = "COVIDOutbreaks.RData")

levels(factor(COVIDOutbreaks$Country)) # Number of countries
levels(factor(COVIDOutbreaks$icd104n)) # Only one disease

#### Final dataset of outbreaks ####
Outbreaks <- rbind(DONsOutbreaks, COVIDOutbreaks[, colnames(DONsOutbreaks)]) # 1502 + 441
rownames(Outbreaks) <- 1:nrow(Outbreaks)
Outbreaks[!is.na(Outbreaks$iso2) & Outbreaks$iso2 == "XA", "Country"] <- "Bonaire Sint Eustatius and Saba"
Outbreaks[!is.na(Outbreaks$iso2) & Outbreaks$iso2 == "XA", "iso2"] <- "BQ"
Outbreaks[!is.na(Outbreaks$iso2) & Outbreaks$iso2 == "XA", "iso3"] <- "BES"

Outbreaks[!is.na(Outbreaks$Country) & Outbreaks$Country == "Kosovo", "iso2"] <- "XK"
Outbreaks[!is.na(Outbreaks$iso2) & Outbreaks$iso2 == "XK", "Country"] <- "Kosovo"
Outbreaks[!is.na(Outbreaks$iso2) & Outbreaks$iso2 == "XK", "iso3"] <- "XXK"

Outbreaks[!is.na(Outbreaks$Country) & Outbreaks$Country == "Gaza", "iso2"] <- "PS"
Outbreaks[!is.na(Outbreaks$Country) & Outbreaks$Country == "Gaza", "Country"] <- "Palestine State of"
Outbreaks[!is.na(Outbreaks$Country) & Outbreaks$Country == "Palestine State of", "iso3"] <- "PSE"

Outbreaks <- Outbreaks[!is.na(Outbreaks$Country), ]

levels(factor(Outbreaks$Country)) # Number of countries
levels(factor(Outbreaks$icd104n)) # Number of disease

save(Outbreaks, file = "Outbreaks.RData")

