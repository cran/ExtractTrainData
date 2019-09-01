#' Add a Point shapefile and raster image.
#' @author  Subhadip Datta
#' @param ras_img Raster image
#' @param point_shp Point shapefile with class info
#' @param res_Col_name Name of the colomn contain class id's
#' @import raster
#' @import rgeos
#' @import rgdal
#' @examples
#' library(raster)
#' library(ExtractTrainData)
#' ras_img<-brick(system.file("extdata","ras.tif",package = "ExtractTrainData"))
#' point_shp<-shapefile(system.file("extdata","poin.shp",package = "ExtractTrainData"))
#' res_Col_name<-"Id"
#' ExValue_from_Pointshp(ras_img,point_shp,res_Col_name)
#' @export
ExValue_from_Pointshp<-function(ras_img,point_shp,res_Col_name){
  message("No. of bands - ",length(names(ras_img)))
  aLl<-data.frame()
  point_shp[[res_Col_name]]<-as.integer(point_shp[[res_Col_name]])
  val<-unique(point_shp[[res_Col_name]])
  message("No. of classes - ",length(val))
  for(i in 1:length(val)){
    message(".", appendLF=FALSE)
    trp<-extract(ras_img,point_shp[point_shp[[res_Col_name]]==val[i],],df=T)[,-1]
    message(".", appendLF=FALSE)
    trp$class_id<-val[i]
    message(".", appendLF=FALSE)
    aLl<-rbind(aLl,trp)
    message(".", appendLF=FALSE)
    remove(trp)
    message(".",round(i/length(val)*100),"% ", appendLF=FALSE)
  }
  message("\nExtraction Completed")
  aLl
}
