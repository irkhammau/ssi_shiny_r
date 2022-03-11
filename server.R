library(shiny)
library(ECharts2Shiny)
library(jsonlite)
library(dplyr)
library(curl)

data_json <- fromJSON("https://us-central1-surveillance-ssi.cloudfunctions.net/pasiensAsaClass1") #Get all data
data_json <- as.data.frame(data_json)

#SSI
get_ssi <- function(data_json) {
  data_ssi <- data_json[,"data"]
  tanggal_total <- list()
  tanggal_total <- data_ssi$tanggal_operasi
  tanggal_total <- as.data.frame(tanggal_total)
  tanggal_total$contain <- 1
  str(tanggal_total)
  tanggal_total <- tanggal_total %>%
    mutate(tanggal=as.Date(tanggal_total, format = "%Y-%m-%d"))
  tanggal_total <- tanggal_total %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
    group_by(year, month)%>%
    summarise(total_operation = sum(contain))
  data_ssi <- data_ssi[,"surveillance"]
  tanggal_ssi <- list()
  gejala_ssi <- list()
  
  for (data in data_ssi){
    tanggal <- c(data[,"tanggal_operasi"])
    gejala <- c(data[,"gejala"])
    tanggal_ssi <- c(tanggal_ssi,tanggal)
    gejala_ssi <- c(gejala_ssi,gejala)
  }
  tanggal_ssi <- as.data.frame((tanggal_ssi))
  gejala_ssi <- as.data.frame((gejala_ssi))
  tanggal_ssi <- as.data.frame(t(tanggal_ssi))
  gejala_ssi <- as.data.frame(t(gejala_ssi))
  colnames(tanggal_ssi) <- c("tanggal")
  colnames(gejala_ssi) <- c("gejala")
  tanggal_ssi$id <- c(1:nrow(tanggal_ssi))
  gejala_ssi$id <- c(1:nrow(gejala_ssi))
  
  final_ssi <- inner_join(tanggal_ssi,gejala_ssi,by="id",copy=FALSE)
  final_ssi$contain <- 1
  final_ssi <- na.omit(final_ssi)
  super <- data.frame()
  organ <- data.frame()
  deep <- data.frame()
  for (data in final_ssi){
    super <- final_ssi[final_ssi$gejala == "Superficial SSI",]
    super <- super %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    super <- super %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(superficial_ssi = sum(contain))
    organ <- final_ssi[final_ssi$gejala == "Organ/space SSI",]
    organ <- organ %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    organ <- organ %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(organ_ssi = sum(contain))
    deep <- final_ssi[final_ssi$gejala == "Deep SSI",]
    deep <- deep %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    deep <- deep %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(deep_ssi = sum(contain))
  }
  final_ssi <- full_join(tanggal_total,super)
  final_ssi <- full_join(final_ssi, deep)
  final_ssi <- full_join(final_ssi, organ)
  final_ssi$tanggal <- paste(final_ssi$year,final_ssi$month,sep="/")
  final_ssi <- subset(final_ssi, select=-c(year,month))
  final_ssi <- final_ssi %>%
    select(tanggal, everything())
  final_ssi <- final_ssi[order(final_ssi$tanggal),]
  final_ssi[is.na(final_ssi)] <- 0
  final_ssi$total_ssi <- rowSums(final_ssi[,3:5])
  final_ssi <- final_ssi %>% 
    mutate(overall_ssi_risk = paste0(as.character(as.integer(total_ssi / total_operation * 100)),"%"))
  total_overall_ssi <- data.frame(colSums(final_ssi[,2:6]))
  total_overall_ssi <- as.data.frame(t(total_overall_ssi))
  total_overall_ssi$tanggal <- c("Total")
  total_overall_ssi$overall_ssi_risk <- c("-")
  total_overall_ssi <- total_overall_ssi %>%
    select(tanggal, everything())
  final_ssi <- rbind(final_ssi,total_overall_ssi)
  rownames(final_ssi) <- NULL
  ssi <- as.data.frame(final_ssi)
  names(ssi) <- c("Year/Month","Total Operation","Superficial SSI","Deep SSI","Organ/space SSI","Total SSI","Overall SSI Risk")
  return(ssi)
}

#ASA
get_asa <- function(data_json) {
  data_asa <- data_json[,"data"]
  final_asa <- data.frame(
    tanggal = data_asa$tanggal_operasi,
    asa_class = data_asa$asa_class,
    contain = 1
  )
  final_asa <- na.omit(final_asa)
  sehat <- data.frame()
  ringan <- data.frame()
  parah <- data.frame()
  lumpuh <- data.frame()
  mati <- data.frame()
  for (data in final_asa){
    sehat <- final_asa[final_asa$asa_class == "Orang sehat",]
    sehat <- sehat %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    sehat <- sehat %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(sehat = sum(contain))
    ringan <- final_asa[final_asa$asa_class == "Penyakit ringan ",]
    ringan <- ringan %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    ringan <- ringan %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(ringan = sum(contain))
    parah <- final_asa[final_asa$asa_class == "Penyakit yang parah tetapi tidak melumpuhkan",]
    parah <- parah %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    parah <- parah %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(parah = sum(contain))
    lumpuh <- final_asa[final_asa$asa_class == "Penyakit melumpuhkan yang dapat mengancap kehidupan ",]
    lumpuh <- lumpuh %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    lumpuh <- lumpuh %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(lumpuh = sum(contain))
    mati <- final_asa[final_asa$asa_class == "Pasien yang hampir mati, tidak bisa bertahan dengan atau tanpa operasi ",]
    mati <- mati %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    mati <- mati %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(mati = sum(contain))
  }
  final_asa <- full_join(sehat,ringan)
  final_asa <- full_join(final_asa, parah)
  final_asa <- full_join(final_asa, lumpuh)
  final_asa <- full_join(final_asa, mati)
  final_asa$tanggal <- paste(final_asa$year,final_asa$month,sep="/")
  final_asa <- subset(final_asa, select=-c(year,month))
  final_asa <- final_asa %>%
    select(tanggal, everything())
  final_asa <- final_asa[order(final_asa$tanggal),]
  final_asa[is.na(final_asa)] <- 0
  final_asa$total_asa <- rowSums(final_asa[,2:6])
  total_overall_asa <- data.frame(colSums(final_asa[,2:7]))
  total_overall_asa <- as.data.frame(t(total_overall_asa))
  total_overall_asa$tanggal <- c("Total")
  total_overall_asa <- total_overall_asa %>%
    select(tanggal, everything())
  final_asa <- rbind(final_asa,total_overall_asa)
  rownames(final_asa) <- NULL
  asa <- as.data.frame(final_asa)
  names(asa) <- c("Tahun/Bulan","Orang sehat","Penyakit Ringan","Penyakit Parah","Penyakit Melumpuhkan","Pasien Hampir Mati","Total")
  return(asa)
}

#Report func
get_report <- function(json,row_start,row_end) {
  report <- as.data.frame(json[1:(nrow(json)-1),row_start:row_end])
  str(report)
  row.names(report) <- json[1:(nrow(json)-1),1]
  names(report) <- names(json[1:(nrow(json)-1),row_start:row_end])
  return(report)
}
get_report_ssi <- function(json,row) {
  report <- as.data.frame(json[1:(nrow(json)-1),row])
  row.names(report) <- json[1:(nrow(json)-1),1]
  names(report) <- "Overall SSI risk (%)"
  report[,1] <- gsub('%','',report[,1])
  str(report)
  return(report)
}
#WOUND
get_wound <- function(data_json) {
  data_wound <- data_json[,"data"]
  final_wound <- data.frame(
    tanggal = data_wound$tanggal_operasi,
    wound = data_wound$wound_class,
    contain = 1
  )
  final_wound <- na.omit(final_wound)
  bersih <- data.frame()
  bersih_terkontaminasi <- data.frame()
  terkontaminasi <- data.frame()
  infeksi <- data.frame()
  for (data in final_wound){
    bersih <- final_wound[final_wound$wound == "Bersih",]
    bersih <- bersih %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    bersih <- bersih %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(bersih = sum(contain))
    bersih_terkontaminasi <- final_wound[final_wound$wound == "Bersih terkontaminasi",]
    bersih_terkontaminasi <- bersih_terkontaminasi %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    bersih_terkontaminasi <- bersih_terkontaminasi %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(bersih_terkontaminasi = sum(contain))
    terkontaminasi <- final_wound[final_wound$wound == "Terkontaminasi",]
    terkontaminasi <- terkontaminasi %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    terkontaminasi <- terkontaminasi %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(terkontaminasi = sum(contain))
    infeksi <- final_wound[final_wound$wound == "Kotor/ terinfeksi",]
    infeksi <- infeksi %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    infeksi <- infeksi %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(infeksi = sum(contain))
  }
  final_wound <- full_join(bersih,bersih_terkontaminasi)
  final_wound <- full_join(final_wound, infeksi)
  final_wound <- full_join(final_wound, terkontaminasi)
  final_wound$tanggal <- paste(final_wound$year,final_wound$month,sep="/")
  final_wound <- subset(final_wound, select=-c(year,month))
  final_wound <- final_wound %>%
    select(tanggal, everything())
  final_wound <- final_wound[order(final_wound$tanggal),]
  final_wound[is.na(final_wound)] <- 0
  final_wound$total_wound <- rowSums(final_wound[,2:5])
  total_overall_wound <- data.frame(colSums(final_wound[,2:6]))
  total_overall_wound <- as.data.frame(t(total_overall_wound))
  total_overall_wound$tanggal <- c("Total")
  total_overall_wound <- total_overall_wound %>%
    select(tanggal, everything())
  final_wound <- rbind(final_wound,total_overall_wound)
  rownames(final_wound) <- NULL
  wound <- as.data.frame(final_wound)
  names(wound) <- c("Tahun/Bulan","Bersih","Bersih Terkontaminsi","Terkontaminsai","Kotor/Terinfeksi","Total")
  return(wound)
}

#URGENCY
get_urgency <- function(data_json) {
  data_urgency <- data_json[,"data"]
  final_urgency <- data.frame(
    tanggal = data_urgency$tanggal_operasi,
    urgency = data_urgency$urgency,
    contain = 1
  )
  final_urgency <- na.omit(final_urgency)
  darurat <- data.frame()
  mendesak <- data.frame()
  semi <- data.frame()
  elektif <- data.frame()
  for (data in final_urgency){
    darurat <- final_urgency[final_urgency$urgency == "Darurat - harus segera dilakukan",]
    darurat <- darurat %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    darurat <- darurat %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(darurat = sum(contain))
    mendesak <- final_urgency[final_urgency$urgency == "Mendesak- harus dilakukan dalam waktu 24-48 jam      ",]
    mendesak <- mendesak %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    mendesak <- mendesak %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(mendesak = sum(contain))
    semi <- final_urgency[final_urgency$urgency == "Semi mendesak- dilakukan beberapa hari-minggu",]
    semi <- semi %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    semi <- semi %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(semi = sum(contain))
    elektif <- final_urgency[final_urgency$urgency == "Elektif- tidak ada batasan waktu",]
    elektif <- elektif %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    elektif <- elektif %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(elektif = sum(contain))
  }
  final_urgency <- full_join(darurat,mendesak)
  final_urgency <- full_join(final_urgency, semi)
  final_urgency <- full_join(final_urgency, elektif)
  final_urgency$tanggal <- paste(final_urgency$year,final_urgency$month,sep="/")
  final_urgency <- subset(final_urgency, select=-c(year,month))
  final_urgency <- final_urgency %>%
    select(tanggal, everything())
  final_urgency <- final_urgency[order(final_urgency$tanggal),]
  final_urgency[is.na(final_urgency)] <- 0
  final_urgency$total_urgency <- rowSums(final_urgency[,2:5])
  total_overall_urgency <- data.frame(colSums(final_urgency[,2:6]))
  total_overall_urgency <- as.data.frame(t(total_overall_urgency))
  total_overall_urgency$tanggal <- c("Total")
  total_overall_urgency <- total_overall_urgency %>%
    select(tanggal, everything())
  final_urgency <- rbind(final_urgency,total_overall_urgency)
  rownames(final_urgency) <- NULL
  urgency <- as.data.frame(final_urgency)
  names(urgency) <- c("Tahun/Bulan","Darurat","Mendesak","Semi Mendesak","Elektif","Total")
  return(urgency)
}
get_antibiotic<-function(data_json){
  data_antibiotic <-  data_json$data$surveillance
  tanggal_antibiotik <- list()
  antibiotik <- list()
  for (data in data_antibiotic){
    tanggal <- c(data[,"tanggal_operasi"])
    antibiotik_ <- c(data[,"antibiotik"])
    tanggal_antibiotik <- c(tanggal_antibiotik,tanggal)
    antibiotik <- c(antibiotik,antibiotik_)
  }
  tanggal_antibiotik <- as.data.frame((tanggal_antibiotik))
  antibiotik <- as.data.frame((antibiotik))
  tanggal_antibiotik <- as.data.frame(t(tanggal_antibiotik))
  antibiotik <- as.data.frame(t(antibiotik))
  colnames(tanggal_antibiotik) <- c("tanggal")
  colnames(antibiotik) <- c("antibiotik")
  tanggal_antibiotik$id <- c(1:nrow(tanggal_antibiotik))
  antibiotik$id <- c(1:nrow(antibiotik))
  
  final_antibiotik <- inner_join(tanggal_antibiotik,antibiotik,by="id",copy=FALSE)
  final_antibiotik$contain <- 1
  final_antibiotik <- subset(final_antibiotik, select=-c(id))
  
  for (data in final_antibiotik){
    antibiotik1 <- final_antibiotik[final_antibiotik$antibiotik == "Amikasin Sulfat",]
    antibiotik1 <- antibiotik1 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik1 <- antibiotik1 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(amikasin_sulfat = sum(contain))
    
    antibiotik2 <- final_antibiotik[final_antibiotik$antibiotik == "Gentamycin",]
    antibiotik2 <- antibiotik2 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik2 <- antibiotik2 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(gentamycint = sum(contain))
    
    antibiotik3 <- final_antibiotik[final_antibiotik$antibiotik == "Streptomisin sulfat",]
    antibiotik3 <- antibiotik3 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik3 <- antibiotik3 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(streptomisin_sulfat = sum(contain))
    
    antibiotik4 <- final_antibiotik[final_antibiotik$antibiotik == "Cefadroxil",]
    antibiotik4 <- antibiotik4 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik4 <- antibiotik4 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(cefadroxil = sum(contain))
    
    antibiotik5 <- final_antibiotik[final_antibiotik$antibiotik == "Cefepime",]
    antibiotik5 <- antibiotik5 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik5 <- antibiotik5 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(cefepime = sum(contain))
    
    antibiotik6 <- final_antibiotik[final_antibiotik$antibiotik == "Cefixime",]
    antibiotik6 <- antibiotik6 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik6 <- antibiotik6 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(cefixime = sum(contain))
    
    antibiotik7 <- final_antibiotik[final_antibiotik$antibiotik == "Cefoperazone",]
    antibiotik7 <- antibiotik7 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik7 <- antibiotik7 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(cefoperazone = sum(contain))
    
    antibiotik8 <- final_antibiotik[final_antibiotik$antibiotik == "Cefotaxime",]
    antibiotik8 <- antibiotik8 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik8 <- antibiotik8 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(cefotaxime = sum(contain))
    
    antibiotik9 <- final_antibiotik[final_antibiotik$antibiotik == "Cefpirome",]
    antibiotik9 <- antibiotik9 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik9 <- antibiotik9 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(cefpirome = sum(contain))
    
    antibiotik10 <- final_antibiotik[final_antibiotik$antibiotik == "Cefprozil",]
    antibiotik10 <- antibiotik10 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik10 <- antibiotik10 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(cefprozil = sum(contain))
    
    antibiotik11 <- final_antibiotik[final_antibiotik$antibiotik == "Ceftazidime",]
    antibiotik11 <- antibiotik11 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik11 <- antibiotik11 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(ceftazidime = sum(contain))
    
    antibiotik12 <- final_antibiotik[final_antibiotik$antibiotik == "Ceftriaxone",]
    antibiotik12 <- antibiotik12 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik12 <- antibiotik12 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(ceftriaxone = sum(contain))
    
    antibiotik13 <- final_antibiotik[final_antibiotik$antibiotik == "Cefuroxime",]
    antibiotik13 <- antibiotik13 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik13 <- antibiotik13 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(cefuroxime = sum(contain))
    
    antibiotik14 <- final_antibiotik[final_antibiotik$antibiotik == "Amoxicillin",]
    antibiotik14 <- antibiotik14 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik14 <- antibiotik14 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(amoxicillin = sum(contain))
    
    antibiotik15 <- final_antibiotik[final_antibiotik$antibiotik == "Ampicilin",]
    antibiotik15 <- antibiotik15 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik15 <- antibiotik15 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(ampicilin= sum(contain))
    
    antibiotik16 <- final_antibiotik[final_antibiotik$antibiotik == "Procain penicillin",]
    antibiotik16 <- antibiotik16 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik16 <- antibiotik16 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(procain_penicillin  = sum(contain))
    
    antibiotik17 <- final_antibiotik[final_antibiotik$antibiotik == "Piperacillin",]
    antibiotik17 <- antibiotik17 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik17 <- antibiotik17 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(piperacillin = sum(contain))
    
    antibiotik18 <- final_antibiotik[final_antibiotik$antibiotik == "Meropenem",]
    antibiotik18 <- antibiotik18 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik18 <- antibiotik18 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(meropenem = sum(contain))
    
    antibiotik19 <- final_antibiotik[final_antibiotik$antibiotik == "Kloramfenikol",]
    antibiotik19 <- antibiotik19 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik19 <- antibiotik19 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(Kloramfenikol = sum(contain))
    
    antibiotik20 <- final_antibiotik[final_antibiotik$antibiotik == "Tiamfenikol",]
    antibiotik20 <- antibiotik20 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik20 <- antibiotik20 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(tiamfenikol = sum(contain))
    
    antibiotik21 <- final_antibiotik[final_antibiotik$antibiotik == "Azithromycin",]
    antibiotik21 <- antibiotik21 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik21 <- antibiotik21 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(azithromycin = sum(contain))
    
    antibiotik22 <- final_antibiotik[final_antibiotik$antibiotik == "Erythromycin",]
    antibiotik22 <- antibiotik22 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik22 <- antibiotik22 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(erythromycin = sum(contain))
    
    antibiotik23 <- final_antibiotik[final_antibiotik$antibiotik == "Spiramycin",]
    antibiotik23 <- antibiotik23 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik23 <- antibiotik23 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(spiramycin = sum(contain))
    
    antibiotik24 <- final_antibiotik[final_antibiotik$antibiotik == "Ciprofloxacin",]
    antibiotik24 <- antibiotik24 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik24 <- antibiotik24 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(ciprofloxacin = sum(contain))
    
    antibiotik25 <- final_antibiotik[final_antibiotik$antibiotik == "Levofloxacin",]
    antibiotik25 <- antibiotik25 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik25 <- antibiotik25 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(levofloxacin = sum(contain))
    
    antibiotik26 <- final_antibiotik[final_antibiotik$antibiotik == "Ofloxacine",]
    antibiotik26 <- antibiotik26 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik26 <- antibiotik26 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(ofloxacine = sum(contain))
    
    antibiotik27 <- final_antibiotik[final_antibiotik$antibiotik == "Doxicyclin",]
    antibiotik27 <- antibiotik27 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik27 <- antibiotik27 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(doxicyclin = sum(contain))
    
    antibiotik28 <- final_antibiotik[final_antibiotik$antibiotik == "Cotrimoxazole (Sulfametoxazole 400mg &Trimetoprim 80mg)",]
    antibiotik28 <- antibiotik28 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik28 <- antibiotik28 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(cotrimoxazole = sum(contain))
    
    antibiotik29 <- final_antibiotik[final_antibiotik$antibiotik == "Clindamycin",]
    antibiotik29 <- antibiotik29 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik29 <- antibiotik29 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(clindamycin = sum(contain))
    
    antibiotik30 <- final_antibiotik[final_antibiotik$antibiotik == "Fosfomycin Na",]
    antibiotik30 <- antibiotik30 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik30 <- antibiotik30 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(fosfomycin_na = sum(contain))
    
    antibiotik31 <- final_antibiotik[final_antibiotik$antibiotik == "Metronidazole",]
    antibiotik31 <- antibiotik31 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik31 <- antibiotik31 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(metronidazole = sum(contain))
    
    antibiotik32 <- final_antibiotik[final_antibiotik$antibiotik == "Vancomycin",]
    antibiotik32 <- antibiotik32 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik32 <- antibiotik32 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(vancomycin = sum(contain))
    
    antibiotik33 <- final_antibiotik[final_antibiotik$antibiotik == "Ethambutol",]
    antibiotik33 <- antibiotik33 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik33 <- antibiotik33 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(ethambutol = sum(contain))
    
    antibiotik34 <- final_antibiotik[final_antibiotik$antibiotik == "Isoniazid (INH)",]
    antibiotik34 <- antibiotik34 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik34 <- antibiotik34 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(isoniazid = sum(contain))
    
    antibiotik35 <- final_antibiotik[final_antibiotik$antibiotik == "Pirazinamide",]
    antibiotik35 <- antibiotik35 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik35 <- antibiotik35 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(pirazinamide = sum(contain))
    
    antibiotik36 <- final_antibiotik[final_antibiotik$antibiotik == "Rifampicin",]
    antibiotik36 <- antibiotik36 %>%
      mutate(tanggal=as.Date(tanggal, format = "%Y-%m-%d"))
    antibiotik36 <- antibiotik36 %>% mutate(year = format(tanggal, "%Y"),month = format(tanggal, "%m")) %>%
      group_by(year, month)%>%
      summarise(rifampicin = sum(contain))
  }
  final_antibiotik <- full_join(antibiotik1,antibiotik2)
  final_antibiotik <- full_join(final_antibiotik, antibiotik3)
  final_antibiotik <- full_join(final_antibiotik, antibiotik4)
  final_antibiotik <- full_join(final_antibiotik, antibiotik5)
  final_antibiotik <- full_join(final_antibiotik, antibiotik6)
  final_antibiotik <- full_join(final_antibiotik, antibiotik7)
  final_antibiotik <- full_join(final_antibiotik, antibiotik8)
  final_antibiotik <- full_join(final_antibiotik, antibiotik9)
  final_antibiotik <- full_join(final_antibiotik, antibiotik10)
  final_antibiotik <- full_join(final_antibiotik, antibiotik11)
  final_antibiotik <- full_join(final_antibiotik, antibiotik12)
  final_antibiotik <- full_join(final_antibiotik, antibiotik13)
  final_antibiotik <- full_join(final_antibiotik, antibiotik14)
  final_antibiotik <- full_join(final_antibiotik, antibiotik15)
  final_antibiotik <- full_join(final_antibiotik, antibiotik16)
  final_antibiotik <- full_join(final_antibiotik, antibiotik17)
  final_antibiotik <- full_join(final_antibiotik, antibiotik18)
  final_antibiotik <- full_join(final_antibiotik, antibiotik19)
  final_antibiotik <- full_join(final_antibiotik, antibiotik20)
  final_antibiotik <- full_join(final_antibiotik, antibiotik21)
  final_antibiotik <- full_join(final_antibiotik, antibiotik22)
  final_antibiotik <- full_join(final_antibiotik, antibiotik23)
  final_antibiotik <- full_join(final_antibiotik, antibiotik24)
  final_antibiotik <- full_join(final_antibiotik, antibiotik25)
  final_antibiotik <- full_join(final_antibiotik, antibiotik26)
  final_antibiotik <- full_join(final_antibiotik, antibiotik27)
  final_antibiotik <- full_join(final_antibiotik, antibiotik28)
  final_antibiotik <- full_join(final_antibiotik, antibiotik29)
  final_antibiotik <- full_join(final_antibiotik, antibiotik30)
  final_antibiotik <- full_join(final_antibiotik, antibiotik31)
  final_antibiotik <- full_join(final_antibiotik, antibiotik32)
  final_antibiotik <- full_join(final_antibiotik, antibiotik33)
  final_antibiotik <- full_join(final_antibiotik, antibiotik34)
  final_antibiotik <- full_join(final_antibiotik, antibiotik35)
  final_antibiotik <- full_join(final_antibiotik, antibiotik36)
  final_antibiotik$tanggal <- paste(final_antibiotik$year,final_antibiotik$month,sep="/")
  final_antibiotik <- subset(final_antibiotik, select=-c(year,month))
  final_antibiotik <- final_antibiotik %>%
    select(tanggal, everything())
  final_antibiotik <- final_antibiotik[order(final_antibiotik$tanggal),]
  final_antibiotik[is.na(final_antibiotik)] <- 0
  final_antibiotik$total <- rowSums(final_antibiotik[,2:37])
  antibio <- as.data.frame(final_antibiotik)
  names(antibio) <- c("Tahun/Bulan","Amikasin Sulfat","Gentamycin","Streptomisin sulfat","Cefadroxil","Cefepime","Cefixime","Cefoperazone","Cefotaxime","Cefpirome","Cefprozil","Ceftazidime","Ceftriaxone","Cefuroxime","Amoxicillin","Ampicilin","Procain penicillin","Piperacillin","Meropenem","Kloramfenikol","Tiamfenikol","Azithromycin","Erythromycin","Spiramycin","Ciprofloxacin","Levofloxacin","Ofloxacine","Doxicyclin","Cotrimoxazole","Clindamycin","Fosfomycin Na","Metronidazole","Vancomycin","Ethambutol","Isoniazid (INH)","Pirazinamide","Rifampicin","Total")
  return(antibio)
}
get_data_antibiotic<-function(json,tanggal){
  df <- json[json[,1]==tanggal,]
  df <- subset(df, select=-c(`Tahun/Bulan`,Total))
  name <- names(df)
  df <- as.data.frame(t(df))
  df$antibiotik <- name
  df <- df[order(df[,1],decreasing = TRUE),]
  df <- as.data.frame(t(df))
  df <- df[1,1:3]
  df <- mutate_all(df, function(x) as.numeric(as.character(x)))
  row.names(df) <- c(tanggal)
  return(df)
}
antibiotic <- get_antibiotic(data_json)
shinyServer(function(input, output){
  output$yearmonth <- renderUI({
    if(input$dataset=='Antibiotics'){
      df <- antibiotic[,1]
      selectInput(inputId = 'filtermonth',
                  label = "Select year/month",
                  choices = df)
    }
  })
  datasetInput <- reactive({
    switch(input$dataset,
           "Surveillance SSI" = ssi <- get_ssi(data_json),
           "Antibiotics" = get_data_antibiotic(antibiotic,input$filtermonth),
           "ASA Class" = get_asa(data_json),
           "Surgical Wounds Class" = get_wound(data_json),
           "Urgency of Operation" = get_urgency(data_json),
    )
  })
  tableInput <- reactive({
    switch(input$dataset,
           "Surveillance SSI" = get_report_ssi(get_ssi(data_json),7),
           "Antibiotics" = as.data.frame(t(get_data_antibiotic(antibiotic,input$filtermonth))),
           #"Antibiotics" = get_report(antibiotic,38),
           "ASA Class" = get_report(get_asa(data_json),2,7),
           "Surgical Wounds Class" = get_report(get_wound(data_json),2,6),
           "Urgency of Operation" = get_report(get_urgency(data_json),2,6),
    )
  })
  fileext <- reactive({
    switch (input$type,
            "Excel (CSV)" = "csv","Text (TSV)" = "txt","Text (Space Separated)" = "txt","Doc"="doc"
    )
  })
  output$downloadData <- downloadHandler(
    filename = function(){
      paste(input$dataset, fileext(), sep = ".")
    },
    content = function(file){
      sep <- switch(input$type, "Excel (CSV)"=",","Text (TSV)"="\t","Text (Space Separated)"=" ", "Doc"=" ")
      write.table(datasetInput(), file, sep = sep,
                  row.names = FALSE)
    }
  )
  output$mytable = DT::renderDataTable({
    datasetInput()
  })
  observeEvent(tableInput(),
             {
		if(input$dataset=="Antibiotics"){
                   renderBarChart(div_id = "linechart",axis.x.name = "Tahun/Bulan",axis.y.name = "y",
                                  data = tableInput())
                   }
                 else if(input$dataset=="Surveillance SSI"){
                   renderLineChart(div_id = "linechart",data = tableInput(),axis.x.name = "Tahun/Bulan",axis.y.name = "Overall SSI Risk")
                 }
                 else if(input$dataset=="ASA Class"){
                   renderLineChart(div_id = "linechart",data = tableInput(),axis.x.name = "Tahun/Bulan",axis.y.name = "Total ASA Class")
                 }
                 else if(input$dataset=="Surgical Wounds Class"){
                   renderLineChart(div_id = "linechart",data = tableInput(),axis.x.name = "Tahun/Bulan",axis.y.name = "Total Surgical Wounds Class")
                 }
                 else if(input$dataset=="Urgency of Operation"){
                   renderLineChart(div_id = "linechart",data = tableInput(),axis.x.name = "Tahun/Bulan",axis.y.name = "Total Urgency of Operation")
                 }
		
	})  
})
