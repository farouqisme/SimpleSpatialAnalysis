
###SET WORKING DIRECTORY
setwd("C:/Users/muham/OneDrive/KeRjAaaAAAaaAaAAA/adhoc/Lab Kewil - Workshop R/Materials/Dataset")

###INSTALL AND IMPORT LIBRARY
library(sf)         ##read .shp
library(haven)      ##read .dta
library(ggplot2)    ##DatViz
library(tidyverse)  ##Data cleaning
library(dplyr)      ##Data cleaning
library(geosphere)  ##inverse distance matrix
library(spdep)      ##Moran

###IMPORT DATA
smp <- read_dta("SMP_DIY.dta")

wd_shp <- "C:/Users/muham/OneDrive/KeRjAaaAAAaaAaAAA/adhoc/Lab Kewil - Workshop R/Materials/shp"
indo_shp <- st_read(paste0(wd_shp,"/idn_admbnda_adm4_bps_20200401.shp"))


###PREPARING SHP DATA
indo_shp <- indo_shp %>% mutate(R101 = as.numeric(substr(ADM1_PCODE,3,4))) %>% ##Create new variable menghilangkan "ID" pada ADM1_PCODE
  ##Keep Provinsi DIY
  filter(., R101 == 34) %>% 
  ##Drop variabel yang baru dibuat
  dplyr::select(., -c(R101)) %>%
  ##Keep variabel yang sekiranya penting
  dplyr::select(., c(ADM1_EN, ADM2_EN, ADM3_EN, ADM4_EN, ADM4_PCODE, geometry))

###PREPARING DATA SMP
smp$id <- paste0(smp$R101,smp$R102,smp$R103,smp$R104) ##Membuat ID yang sesuai dengan shp
smp$ADM4_PCODE <- paste0("ID",smp$id) ##Membuat ID yang sesuai dengan shp


smp_clean <- smp %>% 
  ##Menjumlah SMP negeri dan swasta
  mutate(jum_SMP = rowSums(dplyr::select(.,c(R701FK2:R701FK3)))) %>% 
  ##Keep variabel yang dibutuhkan
  dplyr::select(., c(jum_SMP, ADM4_PCODE))

###MERGING
datfix <- left_join(indo_shp, smp_clean, by = "ADM4_PCODE")
datfix <- datfix[!is.na(datfix$jum_SMP),]

###MAP VISUALIZATION
datfix %>% 
  ggplot() +
  ##Fungsi untuk membuat visualisasi peta
  geom_sf(aes(fill = jum_SMP), color = NA) + ##color = NA <- untuk menghapus border
  ##Customize judul, add subtitle, dan legend
  labs(title = "Jumlah SMP di Provinsi DIY",
       subtitle = "Sumber data: Podes 2021, diolah",
       fill = "Jumlah SMP") +
  ##Customize tema
  theme_light()



###MAP VISUALIZATION
datfix %>% 
  ggplot() +
  ##Fungsi untuk membuat visualisasi peta
  geom_sf()
###SETTING UP DISTANCE MATRIX 
oid <- order(datfix$ADM4_PCODE) ##<- Untuk memastikan urutan data

jogja_shp <- st_coordinates(st_centroid(datfix$geometry)) ##<- Mendapatkan koordinat dari centroid

###Calculate Queen Contiguity
queen <- poly2nb(datfix, queen = T)
listw_q <- nb2listw(queen)

###Calculate Rook Contiguity
rook <- poly2nb(datfix, queen = F)
listw_r <- nb2listw(rook)

###Calculate Inverse distance matrix
distance_matrix <- 1/(distm(jogja_shp[, c(1,2)], fun = distHaversine))*1000
inf.detect <- which(distance_matrix == Inf, arr.ind = T) 
distance_matrix[inf.detect] <- 0

listw <- mat2listw(distance_matrix, style = "W") ##Transforming  matrix into list

###MORAN TEST

###Global Moran
moran_test_rook <- moran.test(datfix$jum_SMP, listw_r)
moran_test_queen <- moran.test(datfix$jum_SMP, listw_q)
moran_test_inv <- moran.test(datfix$jum_SMP, listw)

moran_test_rook
moran_test_queen
moran_test_inv

###Local Moran
localmor <-localmoran(datfix$jum_SMP, listw)
localmor

localmor_res <- data.frame(localmor[oid,], "ADM4_PCODE" = datfix$ADM4_PCODE[oid]) ##<-pairing with ID data using orderID

datlocmor <- left_join(datfix,localmor_res, by = "ADM4_PCODE") ##<-merge with master data

datlocmor <- datlocmor %>% mutate(sign = if_else(Pr.z....E.Ii.. <= 0.05,Ii,NA)) ##<-Membuat variabel untuk plot local moran

###Plotting local moran cluster pada local moran statistic
datlocmor %>% 
  ggplot() +
  geom_sf(aes(fill = sign), color = NA) +
  labs(title = "Local Moran",
       subtitle = "Data: Podes 2021, Diolah",
       fill = "Ii")
