#' Add a Polygon shapefile and raster image.
#' @author  Subhadip Datta
#' @param ras_img Raster image
#' @param poly_shp Polygon shapefile with class info
#' @param res_Col_name Name of the colomn contain class id's
#' @import sp
#' @import raster
#' @import rgeos
#' @import rgdal
#' @examples
#' library(raster)
#' library(ExtractTrainData)
#' ras_img<-brick(system.file("extdata","ras.tif",package = "ExtractTrainData"))
#' poly_shp<-shapefile(system.file("extdata","poly_shp.shp",package = "ExtractTrainData"))
#' res_Col_name<-"Id"
#' ExValue_from_Polyshp(ras_img,poly_shp,res_Col_name)
#' @export
ExValue_from_Polyshp<-function(ras_img,poly_shp,res_Col_name){
  message("No. of bands - ",length(names(ras_img)))
  aLl<-data.frame()
  poly_shp[[res_Col_name]]<-as.integer(poly_shp[[res_Col_name]])
  val<-unique(poly_shp[[res_Col_name]])
  message("No. of classes - ",length(val))
  for(i in 1:length(val)){
    message(".", appendLF=FALSE)
    ext_val<-extract(ras_img,poly_shp[poly_shp[[res_Col_name]]==val[i],],df=T)[,-1]
    message(".", appendLF=FALSE)
    ext_val$class_id<-val[i]
    message(".", appendLF=FALSE)
    aLl<-rbind(aLl,ext_val)
    message(".", appendLF=FALSE)
    remove(ext_val)
    message(".",round((i/length(val))*100),"% ", appendLF=FALSE)
  }
  message("\nExtraction Completed")
  aLl
}
