#######################################################

library(openxlsx)
library(lubridate)

#######################################################

setwd("C:\\aaa_lavori\\lav_campionamenti")

#######################################################

data_veg_capellino=read.xlsx("monitoraggio_capellino_2023.xlsx",2)

data_meteo_capellino=read.xlsx("dati_firenze_stazioni_orari_analisi_2023.xlsx")
data_meteo_capellino$date=convertToDate(data_meteo_capellino$date,origin="1899-12-30")
data_meteo_capellino$datetime=convertToDateTime(data_meteo_capellino$datetime,origin="1899-12-30")


data_meteo_stazioni=read.xlsx("dati_firenze_stazioni_orari_analisi_2023.xlsx",3)

#######################################################

mese_campionamento=unique(data_veg_capellino$Mese)
  
date_campionamento=c(dmy("03/05/2023"),
                     dmy("09/06/2023"),
                     dmy("11/07/2023"),
                     dmy("08/08/2023"),
                     dmy("20/09/2023"),
                     dmy("22/11/2023"))

######################################################
# stabilisci il lag

lagged=7 # giorno

date_campionamento_seq=date_campionamento-lagged

ls_seq_data=list()
ls_seq_data_day=list()

for ( i in 1:length(date_campionamento))
{
  seq_days_7=seq.Date(date_campionamento_seq[i],date_campionamento[i],1)
  
  ls_seq_data[[i]]=data_meteo_capellino[which(data_meteo_capellino$date  %in%  head(seq_days_7,(lagged)) ==T),]
  ls_seq_data_day[[i]]=data_meteo_capellino[which(data_meteo_capellino$date  %in%  tail(seq_days_7,1) ==T),]
  
  }

##############################################################################
# extract climate variables

mw_camp=list()

for ( i in 1:length(date_campionamento))
{
  mw_camp[[i]]=cbind(c_before7_max=max(ls_seq_data[[i]]$FIRENZEUNI_tair),
                     c_before7_mean=mean(ls_seq_data[[i]]$FIRENZEUNI_tair),
                     c_before7_min=min(ls_seq_data[[i]]$FIRENZEUNI_tair),
                     c_before7_over27=length(which(ls_seq_data[[i]]$FIRENZEUNI_tair >27)),
                     c_before7_over34=length(which(ls_seq_data[[i]]$FIRENZEUNI_tair >34)),
                     c_data_max=max(ls_seq_data_day[[i]]$FIRENZEUNI_tair[9:12]),
                     c_data_mean=mean(ls_seq_data_day[[i]]$FIRENZEUNI_tair[9:12]),
                     c_data_min=min(ls_seq_data_day[[i]]$FIRENZEUNI_tair))
}


###########################################################################
data_veg_capellino$c_before7_max=NA
data_veg_capellino$c_before7_mean=NA
data_veg_capellino$c_before7_min=NA
data_veg_capellino$c_before7_over27=NA
data_veg_capellino$c_before7_over34=NA
data_veg_capellino$c_data_max=NA
data_veg_capellino$c_data_mean=NA
data_veg_capellino$c_data_min=NA
###########################################################################
                     
                     
for ( i in 1:6) {
                  d=data.frame(t(as.numeric(mw_camp[[i]]))) 
                
                 data_veg_capellino[which(data_veg_capellino$Mese ==mese_campionamento[i]),21:28]=d
  
                }



write.xlsx(data_veg_capellino,"data_veg_capellino_wclimate.xlsx")
