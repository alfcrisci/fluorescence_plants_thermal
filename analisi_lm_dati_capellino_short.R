library(openxlsx)
library(lubridate)
library(tidyverse)
library(lme4)
library(jtools)
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(effects)
library(sjstats) #use for r2 functions
library(ggpubr) 
library(ggeffects) 
library(huxtable)
library(flextable)
library(officer)

#######################################################
setwd("")
#######################################################


data_veg_clim=read.xlsx("data_veg_capellino_wclimate.xlsx")

################################################################################
# fotosintesi: Fv_Fm v ed m sono maiuscole un pedice

names(data_veg_clim)[7]="ratio_Fv_Fm"
names(data_veg_clim)[12]="Pi_abs"
names(data_veg_clim)[17]="Flv"
names(data_veg_clim)[8]="psieo_1_vj"

# Efficienza fotosintetica: Piabs abs è minuscolo in pedice
# Clorofille: Chl
# Flavonoli: Flv
# Nitrogen Balance Index: NBI 

names(data_veg_clim)

###########################################################################à
# [1] "Mese"         "MacroArea"    "Transetto"    "Specie"       "F0"           "ratio_Fv_Fm"  "PSIo.(1-Vj)" 
# [8] "PSIreo(1-Vi)" "Sm"           "ABS/RC"       "Pi_abs"       "Vi/Vj"        "TRo/RC"       "ETo/RC"      
# [15] "REo/RC"       "Flv"          "Chl"          "Flavonoidi"   "Antociani"    "NBI"          "Tmax_lag7"   
# [22] "Tmean_lag7"   "Tmin_lag7"    "H_lag_o27"    "H_lag_o34"    "Tmax_lag0"    "Tmean_lag0"   "Tmin_lag0"  "      
#
# PSIE0 [8]
# NBI ratio chl e flv

#
################################################################################
# splitting del database per macro aree

data_veg_clim_species=split(data_veg_clim,data_veg_clim$Specie)


#############################################################################################################################
# clean names

names(data_veg_clim_species[[3]])<-janitor::make_clean_names(names(data_veg_clim_species[[3]])) # arundo
names(data_veg_clim_species[[2]])<-janitor::make_clean_names(names(data_veg_clim_species[[2]])) # artemisia
names(data_veg_clim_species[[1]])<-janitor::make_clean_names(names(data_veg_clim_species[[1]])) # alloro

####################################################################################################
titleplot_arundo_html<- c(bquote("Pi<sub>abs</sub>  Linear modeling about <i>Arundo donax</i>"),
                          bquote("F<sub>v</sub>/F<sub>m</sub> Linear modeling about <i>Arundo donax</i>"),
                          bquote("PSIEO 1-V<sub>j</sub> Linear modeling about <i>Arundo donax</i>"),
                          bquote("V<sub>k</sub> Linear modeling about <i>Arundo donax</i>"),
                          bquote("NBI Linear modeling about <i>Arundo donax</i>"),
                          bquote("Flv Linear modeling about <i>Arundo donax</i>"),
                          bquote("Chl Linear modeling about <i>Arundo donax</i>"))

titleplot_alloro_html<- c(bquote("Pi<sub>abs</sub>  Linear modeling about <i>Laurus nobilis</i>"),
                          bquote("F<sub>v</sub>/F<sub>m</sub> Linear modeling about <i>Laurus nobilis</i>"),
                          bquote("PSIEO 1-V<sub>j</sub> Linear modeling about <i>Laurus nobilis</i>"),
                          bquote("V<sub>k</sub> Linear modeling about <i>Laurus nobilis</i>"),
                          bquote("NBI Linear modeling about <i>Laurus nobilis</i>"),
                          bquote("Flv Linear modeling about <i>Laurus nobilis</i>"),
                          bquote("Chl Linear modeling about <i>Laurus nobilis</i>"))

titleplot_artemisia_html<- c(bquote("Pi<sub>abs</sub>  Linear modeling about <i>Artemisia vulgaris</i>"),
                             bquote("F<sub>v</sub>/F<sub>m</sub> Linear modeling about <i>Artemisia vulgaris</i>"),
                             bquote("PSIEO 1-V<sub>j</sub> Linear modeling about <i>Artemisia vulgaris</i>"),
                             bquote("V<sub>k</sub> Linear modeling about <i>Artemisia vulgaris</i>"),
                             bquote("NBI Linear modeling about <i>Artemisia vulgaris</i>"),
                             bquote("Flv Linear modeling about <i>Artemisia vulgaris</i>"),
                             bquote("Chl Linear modeling about <i>Artemisia vulgaris</i>"))

model_names<- c(bquote("Pi<sub>abs</sub>"),
                bquote("F<sub>v</sub>/F<sub>m</sub>"),
                bquote("PSI EO 1-V<sub>j</sub>"),
                bquote("V<sub>k</sub>"),
                bquote("NBI"),
                bquote("Flv"),
                bquote("Chl"))

yaxis_html<- c(bquote("Pi<sub>abs</sub>"),
               bquote("F<sub>v</sub>/F<sub>m</sub>"),
               bquote("PSI EO 1-V<sub>j</sub>"),
               bquote("V<sub>k</sub>"),
               bquote("NBI"),
               bquote("Flv"),
               bquote("Chl"))

yaxis_Rexp<- c(expression("Pi"[abs]),
               expression('F'[v]/F[m]~""),
               bquote("PSI EO 1-V"[j]),
               bquote("V"[k]),
               bquote("NBI"),
               bquote("Flv"),
               bquote("Chl"))




####################################################################################################

setwd("C:\\aaa_lavori\\lav_campionamenti\\results")
file.remove(dir())

#############################################################################################################################
# arundo modeling mixed and linear


arundo_lm_piabs=lm( pi_abs~ tmin_lag7+h_lag_o34+tmax_lag0,data=data_veg_clim_species[[3]])
arundo_lm_phi=lm( ratio_fv_fm~ tmin_lag7+h_lag_o34+tmax_lag0, data=data_veg_clim_species[[3]])
arundo_lm_psie0=lm( psieo_1_vj~ tmin_lag7+h_lag_o34+tmax_lag0,data=data_veg_clim_species[[3]])
arundo_lm_vk=lm( vk~ tmin_lag7+h_lag_o34+tmax_lag0, data=data_veg_clim_species[[3]])
arundo_lm_nbi=lm( nbi~ tmin_lag7+h_lag_o34+tmax_lag0,data=data_veg_clim_species[[3]])
arundo_lm_flav=lm( flavonoidi~ tmin_lag7+h_lag_o34+tmax_lag0,data=data_veg_clim_species[[3]])
arundo_lm_chl=lm( chl~ tmin_lag7+h_lag_o34+tmax_lag0,data=data_veg_clim_species[[3]])

################################################################################
# plotting

arundo_coefplot_piabs<-sjPlot::plot_model(arundo_lm_piabs,show.values=TRUE, show.p=TRUE,title="")
arundo_coefplot_phi<-sjPlot::plot_model(arundo_lm_phi,show.values=TRUE, show.p=TRUE,title="")
arundo_coefplot_psie0<-sjPlot::plot_model(arundo_lm_psie0,show.values=TRUE, show.p=TRUE,title="")
arundo_coefplot_vk<-sjPlot::plot_model(arundo_lm_vk,show.values=TRUE, show.p=TRUE,title="")
arundo_coefplot_nbi<-sjPlot::plot_model(arundo_lm_nbi,show.values=TRUE, show.p=TRUE,title="" )
arundo_coefplot_flav<-sjPlot::plot_model(arundo_lm_flav,show.values=TRUE, show.p=TRUE,title="")
arundo_coefplot_chl<-sjPlot::plot_model(arundo_lm_chl,show.values=TRUE, show.p=TRUE,title="")




arundo_modeltab_piabs<-sjPlot::tab_model(arundo_lm_piabs,title=titleplot_arundo_html[1],dv.labels = " ",show.aic =T,show.fstat=T,file="arundo_piabs_model.htm")
arundo_modeltab_phi<-sjPlot:: tab_model(arundo_lm_phi,title=titleplot_arundo_html[2],dv.labels = " ",show.aic =T,show.fstat=T,file="arundo_phi_model.htm")
arundo_modeltab_psie0<-sjPlot:: tab_model(arundo_lm_psie0,title=titleplot_arundo_html[3],dv.labels = " ",show.aic =T,show.fstat=T,file="arundo__psie0_model.htm")
arundo_modeltab_vk<-sjPlot:: tab_model(arundo_lm_vk,title=titleplot_arundo_html[4],dv.labels = " ",show.aic =T,show.fstat=T,file="arundo_vk_model.htm")
arundo_modeltab_nbi<-sjPlot:: tab_model(arundo_lm_nbi,title=titleplot_arundo_html[5],dv.labels = " ",show.aic =T,show.fstat=T,file="arundo_nbi_model.htm")
arundo_modeltab_flav<-sjPlot:: tab_model(arundo_lm_flav,title=titleplot_arundo_html[6],dv.labels = " ",show.aic =T,show.fstat=T,file="arundo_flv_model.htm")
arundo_modeltab_chl<-sjPlot:: tab_model(arundo_lm_chl,title=titleplot_arundo_html[7],dv.labels = " ",show.aic =T,show.fstat=T,file="arundo_chl_model.htm")

##############################################################################################################################################################
# esportazione modelli

quick_docx(export_summs(arundo_lm_piabs,
                        arundo_lm_phi,
                        arundo_lm_psie0,
                        arundo_lm_vk,
                        arundo_lm_nbi,
                        arundo_lm_flav,
                        arundo_lm_chl, 
                        scale = TRUE,model.names =yaxis_html,digits = 3,statistics = c("N. obs." = "nobs", 
                                                                                       "R squared" = "r.squared", "F statistic" = "statistic",
                                                                                       "P value" = "p.value")),
                        file="arundo_all_models.docx")


##############################################################################################################################################################
# Visualizzazione effetti libreria effects


eall_piabs <- predictorEffects(arundo_lm_piabs)
for ( i in 1:3) {
png(paste0("arundo_effect_all_piabs_",i,".png"))
plot(eall_piabs[i],ylab=yaxis_Rexp[1],main=NULL,grid=TRUE)
dev.off()
}

eall_phi <- predictorEffects(arundo_lm_phi)
for ( i in 1:3) {
  png(paste0("arundo_effect_all_phi_",i,".png"))
  plot(eall_phi[i],ylab=yaxis_Rexp[2],main=NULL,grid=TRUE)
  dev.off()
}


eall_psie0 <- predictorEffects(arundo_lm_psie0)
for ( i in 1:3) {
  png(paste0("arundo_effect_all_psie0_",i,".png"))
  plot(eall_psie0[i],ylab=yaxis_Rexp[3],main=NULL,grid=TRUE)
  dev.off()
}


eall_vk <- predictorEffects(arundo_lm_vk)
for ( i in 1:3) {
  png(paste0("arundo_effect_all_vk_",i,".png"))
  plot(eall_vk[i],ylab=yaxis_Rexp[4],main=NULL,grid=TRUE)
  dev.off()
}

eall_nbi <- predictorEffects(arundo_lm_nbi)
for ( i in 1:3) {
  png(paste0("arundo_effect_all_nbi_",i,".png"))
  plot(eall_nbi[i],ylab=yaxis_Rexp[5],main=NULL,grid=TRUE)
  dev.off()
}

eall_flav <- predictorEffects(arundo_lm_flav)
for ( i in 1:3) {
  png(paste0("arundo_effect_all_flav_",i,".png"))
  plot(eall_flav[i],ylab=yaxis_Rexp[6],main=NULL,grid=TRUE)
  dev.off()
}


eall_chl <- predictorEffects(arundo_lm_chl)
for ( i in 1:3) {
  png(paste0("arundo_effect_all_chl_",i,".png"))
  plot(eall_chl[i],ylab=yaxis_Rexp[7],main=NULL,grid=TRUE)
  dev.off()
}

###############################################################################################


############################################################################################################################
# artemisia modeling mixed and linear


artemisia_lm_piabs=lm( pi_abs~ tmin_lag7+h_lag_o34+tmax_lag0,data=data_veg_clim_species[[2]])
artemisia_lm_phi=lm( ratio_fv_fm~ tmin_lag7+h_lag_o34+tmax_lag0, data=data_veg_clim_species[[2]])
artemisia_lm_psie0=lm( psieo_1_vj~ tmin_lag7+h_lag_o34+tmax_lag0,data=data_veg_clim_species[[2]])
artemisia_lm_vk=lm( vk~ tmin_lag7+h_lag_o34+tmax_lag0, data=data_veg_clim_species[[2]])
artemisia_lm_nbi=lm( nbi~ tmin_lag7+h_lag_o34+tmax_lag0,data=data_veg_clim_species[[2]])
artemisia_lm_flav=lm( flavonoidi~ tmin_lag7+h_lag_o34+tmax_lag0,data=data_veg_clim_species[[2]])
artemisia_lm_chl=lm( chl~ tmin_lag7+h_lag_o34+tmax_lag0,data=data_veg_clim_species[[2]])

################################################################################
# plotting

artemisia_coefplot_piabs<-sjPlot::plot_model(artemisia_lm_piabs,show.values=TRUE, show.p=TRUE,title="")
artemisia_coefplot_phi<-sjPlot::plot_model(artemisia_lm_phi,show.values=TRUE, show.p=TRUE,title="")
artemisia_coefplot_psie0<-sjPlot::plot_model(artemisia_lm_psie0,show.values=TRUE, show.p=TRUE,title="")
artemisia_coefplot_vk<-sjPlot::plot_model(artemisia_lm_vk,show.values=TRUE, show.p=TRUE,title="")
artemisia_coefplot_nbi<-sjPlot::plot_model(artemisia_lm_nbi,show.values=TRUE, show.p=TRUE,title="" )
artemisia_coefplot_flav<-sjPlot::plot_model(artemisia_lm_flav,show.values=TRUE, show.p=TRUE,title="")
artemisia_coefplot_chl<-sjPlot::plot_model(artemisia_lm_chl,show.values=TRUE, show.p=TRUE,title="")




artemisia_modeltab_piabs<-sjPlot::tab_model(artemisia_lm_piabs,title=titleplot_artemisia_html[1],dv.labels = " ",show.aic =T,show.fstat=T,file="artemisia_piabs_model.htm")
artemisia_modeltab_phi<-sjPlot:: tab_model(artemisia_lm_phi,title=titleplot_artemisia_html[2],dv.labels = " ",show.aic =T,show.fstat=T,file="artemisia_phi_model.htm")
artemisia_modeltab_psie0<-sjPlot:: tab_model(artemisia_lm_psie0,title=titleplot_artemisia_html[3],dv.labels = " ",show.aic =T,show.fstat=T,file="artemisia_psie0_model.htm")
artemisia_modeltab_vk<-sjPlot:: tab_model(artemisia_lm_vk,title=titleplot_artemisia_html[4],dv.labels = " ",show.aic =T,show.fstat=T,file="artemisia_vk_model.htm")
artemisia_modeltab_nbi<-sjPlot:: tab_model(artemisia_lm_nbi,title=titleplot_artemisia_html[5],dv.labels = " ",show.aic =T,show.fstat=T,file="artemisia_nbi_model.htm")
artemisia_modeltab_flav<-sjPlot:: tab_model(artemisia_lm_flav,title=titleplot_artemisia_html[6],dv.labels = " ",show.aic =T,show.fstat=T,file="artemisia_flv_model.htm")
artemisia_modeltab_chl<-sjPlot:: tab_model(artemisia_lm_chl,title=titleplot_artemisia_html[7],dv.labels = " ",show.aic =T,show.fstat=T,file="artemisia_chl_model.htm")

##############################################################################################################################################################
# esportazione modelli

quick_docx(export_summs(artemisia_lm_piabs,
                        artemisia_lm_phi,
                        artemisia_lm_psie0,
                        artemisia_lm_vk,
                        artemisia_lm_nbi,
                        artemisia_lm_flav,
                        artemisia_lm_chl, 
                        scale = TRUE,model.names =yaxis_html,digits = 3,statistics = c("N. obs." = "nobs", 
                                                                                       "R squared" = "r.squared", "F statistic" = "statistic",
                                                                                       "P value" = "p.value")),
           file="artemisia_all_models.docx")


##############################################################################################################################################################
# Visualizzazione effetti libreria effects


eall_piabs <- predictorEffects(artemisia_lm_piabs)
for ( i in 1:3) {
  png(paste0("artemisia_effect_all_piabs_",i,".png"))
  plot(eall_piabs[i],ylab=yaxis_Rexp[1],main=NULL,grid=TRUE)
  dev.off()
}

eall_phi <- predictorEffects(artemisia_lm_phi)
for ( i in 1:3) {
  png(paste0("artemisia_effect_all_phi_",i,".png"))
  plot(eall_phi[i],ylab=yaxis_Rexp[2],main=NULL,grid=TRUE)
  dev.off()
}


eall_psie0 <- predictorEffects(artemisia_lm_psie0)
for ( i in 1:3) {
  png(paste0("artemisia_effect_all_psie0_",i,".png"))
  plot(eall_psie0[i],ylab=yaxis_Rexp[3],main=NULL,grid=TRUE)
  dev.off()
}


eall_vk <- predictorEffects(artemisia_lm_vk)
for ( i in 1:3) {
  png(paste0("artemisia_effect_all_vk_",i,".png"))
  plot(eall_vk[i],ylab=yaxis_Rexp[4],main=NULL,grid=TRUE)
  dev.off()
}

eall_nbi <- predictorEffects(artemisia_lm_nbi)
for ( i in 1:3) {
  png(paste0("artemisia_effect_all_nbi_",i,".png"))
  plot(eall_nbi[i],ylab=yaxis_Rexp[5],main=NULL,grid=TRUE)
  dev.off()
}

eall_flav <- predictorEffects(artemisia_lm_flav)
for ( i in 1:3) {
  png(paste0("artemisia_effect_all_flav_",i,".png"))
  plot(eall_flav[i],ylab=yaxis_Rexp[6],main=NULL,grid=TRUE)
  dev.off()
}


eall_chl <- predictorEffects(artemisia_lm_chl)
for ( i in 1:3) {
  png(paste0("artemisia_effect_all_chl_",i,".png"))
  plot(eall_chl[i],ylab=yaxis_Rexp[7],main=NULL,grid=TRUE)
  dev.off()
}
###############################################################################################


#############################################################################################################################
# alloro modeling mixed and linear


alloro_lm_piabs=lm( pi_abs~ tmin_lag7+h_lag_o34+tmax_lag0,data=data_veg_clim_species[[1]])
alloro_lm_phi=lm( ratio_fv_fm~ tmin_lag7+h_lag_o34+tmax_lag0, data=data_veg_clim_species[[1]])
alloro_lm_psie0=lm( psieo_1_vj~ tmin_lag7+h_lag_o34+tmax_lag0,data=data_veg_clim_species[[1]])
alloro_lm_vk=lm( vk~ tmin_lag7+h_lag_o34+tmax_lag0, data=data_veg_clim_species[[1]])
alloro_lm_nbi=lm( nbi~ tmin_lag7+h_lag_o34+tmax_lag0,data=data_veg_clim_species[[1]])
alloro_lm_flav=lm( flavonoidi~ tmin_lag7+h_lag_o34+tmax_lag0,data=data_veg_clim_species[[1]])
alloro_lm_chl=lm( chl~ tmin_lag7+h_lag_o34+tmax_lag0,data=data_veg_clim_species[[1]])

################################################################################
# plotting

alloro_coefplot_piabs<-sjPlot::plot_model(alloro_lm_piabs,show.values=TRUE, show.p=TRUE,title="")
alloro_coefplot_phi<-sjPlot::plot_model(alloro_lm_phi,show.values=TRUE, show.p=TRUE,title="")
alloro_coefplot_psie0<-sjPlot::plot_model(alloro_lm_psie0,show.values=TRUE, show.p=TRUE,title="")
alloro_coefplot_vk<-sjPlot::plot_model(alloro_lm_vk,show.values=TRUE, show.p=TRUE,title="")
alloro_coefplot_nbi<-sjPlot::plot_model(alloro_lm_nbi,show.values=TRUE, show.p=TRUE,title="" )
alloro_coefplot_flav<-sjPlot::plot_model(alloro_lm_flav,show.values=TRUE, show.p=TRUE,title="")
alloro_coefplot_chl<-sjPlot::plot_model(alloro_lm_chl,show.values=TRUE, show.p=TRUE,title="")




alloro_modeltab_piabs<-sjPlot::tab_model(alloro_lm_piabs,title=titleplot_alloro_html[1],dv.labels = " ",show.aic =T,show.fstat =T,file="alloro_piabs_model.htm")
alloro_modeltab_phi<-sjPlot:: tab_model(alloro_lm_phi,title=titleplot_alloro_html[2],dv.labels = " ",show.aic =T,show.fstat =T,file="alloro_phi_model.htm")
alloro_modeltab_psie0<-sjPlot:: tab_model(alloro_lm_psie0,title=titleplot_alloro_html[3],dv.labels = " ",show.aic =T,show.fstat =T,file="alloro_psie0_model.htm")
alloro_modeltab_vk<-sjPlot:: tab_model(alloro_lm_vk,title=titleplot_alloro_html[4],dv.labels = " ",show.aic =T,show.fstat =T,file="alloro_vk_model.htm")
alloro_modeltab_nbi<-sjPlot:: tab_model(alloro_lm_nbi,title=titleplot_alloro_html[5],dv.labels = " ",show.aic =T,show.fstat =T,file="alloro_nbi_model.htm")
alloro_modeltab_flav<-sjPlot:: tab_model(alloro_lm_flav,title=titleplot_alloro_html[6],dv.labels = " ",show.aic =T,show.fstat =T,file="alloro_flv_model.htm")
alloro_modeltab_chl<-sjPlot:: tab_model(alloro_lm_chl,title=titleplot_alloro_html[7],dv.labels = " ",show.aic =T,show.fstat =T,file="alloro_chl_model.htm")

##############################################################################################################################################################
# esportazione modelli

quick_docx(export_summs(alloro_lm_piabs,
                        alloro_lm_phi,
                        alloro_lm_psie0,
                        alloro_lm_vk,
                        alloro_lm_nbi,
                        alloro_lm_flav,
                        alloro_lm_chl, 
                        scale = TRUE,model.names =yaxis_html,digits = 3,statistics = c("N. obs." = "nobs", 
                                                                                       "R squared" = "r.squared", "F statistic" = "statistic",
                                                                                       "P value" = "p.value")),
           file="alloro_all_models.docx")


##############################################################################################################################################################
# Visualizzazione effetti libreria effects


eall_piabs <- predictorEffects(alloro_lm_piabs)
for ( i in 1:3) {
  png(paste0("alloro_effect_all_piabs_",i,".png"))
  plot(eall_piabs[i],ylab=yaxis_Rexp[1],main=NULL,grid=TRUE)
  dev.off()
}

eall_phi <- predictorEffects(alloro_lm_phi)
for ( i in 1:3) {
  png(paste0("alloro_effect_all_phi_",i,".png"))
  plot(eall_phi[i],ylab=yaxis_Rexp[2],main=NULL,grid=TRUE)
  dev.off()
}


eall_psie0 <- predictorEffects(alloro_lm_psie0)
for ( i in 1:3) {
  png(paste0("alloro_effect_all_psie0_",i,".png"))
  plot(eall_psie0[i],ylab=yaxis_Rexp[3],main=NULL,grid=TRUE)
  dev.off()
}


eall_vk <- predictorEffects(alloro_lm_vk)
for ( i in 1:3) {
  png(paste0("alloro_effect_all_vk_",i,".png"))
  plot(eall_vk[i],ylab=yaxis_Rexp[4],main=NULL,grid=TRUE)
  dev.off()
}

eall_nbi <- predictorEffects(alloro_lm_nbi)
for ( i in 1:3) {
  png(paste0("alloro_effect_all_nbi_",i,".png"))
  plot(eall_nbi[i],ylab=yaxis_Rexp[5],main=NULL,grid=TRUE)
  dev.off()
}

eall_flav <- predictorEffects(alloro_lm_flav)
for ( i in 1:3) {
  png(paste0("alloro_effect_all_flav_",i,".png"))
  plot(eall_flav[i],ylab=yaxis_Rexp[6],main=NULL,grid=TRUE)
  dev.off()
}


eall_chl <- predictorEffects(alloro_lm_chl)
for ( i in 1:3) {
  png(paste0("alloro_effect_all_chl_",i,".png"))
  plot(eall_chl[i],ylab=yaxis_Rexp[7],main=NULL,grid=TRUE)
  dev.off()
}


###############################################################################################




#################################################################################################################################
# https://lmudge13.github.io/sample_code/mixed_effects.html





