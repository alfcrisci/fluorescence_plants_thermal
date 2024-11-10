library(magick)

setwd("C:\\aaa_lavori\\lav_campionamenti\\results")

alloro_files=dir(pattern="alloro")
artemisia_files=dir(pattern="artemisia")
arundo_files=dir(pattern="arundo")

vars=c("piabs","phi","psie0","nbi","flav","chl")

#################################################################################
#
alloro_files_img=sapply(alloro_files,image_read)

alloro_list=list()

for ( i in 1:6) {
  img=c(alloro_files_img[grep(vars[i],alloro_files)])
  alloro_list[[i]]=image_append(image_join(img))
  
}

alloro_imgs=c(alloro_list[[1]],alloro_list[[2]],
              alloro_list[[3]],alloro_list[[4]],
              alloro_list[[5]],alloro_list[[6]])

image_write(image_append(alloro_imgs,stack = T), path = paste("alloro_all_effect.png"), format = "png")
#################################################################################
#################################################################################
#
artemisia_files_img=sapply(artemisia_files,image_read)

artemisia_list=list()

for ( i in 1:6) {
  img=c(artemisia_files_img[grep(vars[i],alloro_files)])
        artemisia_list[[i]]=image_append(image_join(img))
  
}

artemisia_imgs=c(artemisia_list[[1]],artemisia_list[[2]],
                 artemisia_list[[3]],artemisia_list[[4]],
                 artemisia_list[[5]],artemisia_list[[6]])

image_write(image_append(artemisia_imgs,stack = T), path = paste("artemisia_all_effect.png"), format = "png")
#################################################################################
#################################################################################
#
arundo_files_img=sapply(arundo_files,image_read)

arundo_list=list()

for ( i in 1:6) {
  img=c(arundo_files_img[grep(vars[i],alloro_files)])
  
  arundo_list[[i]]=image_append(image_join(img))
  
}


arundo_imgs=c(arundo_list[[1]],arundo_list[[2]],
              arundo_list[[3]],arundo_list[[4]],
              arundo_list[[5]],arundo_list[[6]])

image_write(image_append(arundo_imgs,stack = T), path = paste("arundo_all_effect.png"), format = "png")
#################################################################################
#####################################################################
# https://stackoverflow.com/questions/49612276/how-can-i-image-read-multiple-images-at-once


# library(purrr)
# library(magick)
# capturas <- list.files("./path/to/images/", pattern = "\\.png$")
# 
# # get all images in a list
# images <- map(capturas, image_read)
# images <- image_join(images)
# 
# image_animate(images, fps = 1, dispose = "previous")