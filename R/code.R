#' Add a Polygon shapefile and raster image.
#' @author  Subhadip Datta
#' @param img Raster image
#' @param poly.shp Polygon shapefile with class info
#' @param In.colName Name of the column contain polygon id's
#' @param Out.colName Name of the output column contain polygon id's
#' @import sp
#' @import raster
#' @import rgeos
#' @import rgdal
#' @examples
#' library(raster)
#' library(ExtractTrainData)
#' img<-brick(system.file("extdata","ras.tif",package = "ExtractTrainData"))
#' poly.shp<-shapefile(system.file("extdata","poly_shp.shp",package = "ExtractTrainData"))
#' Out.colName<-In.colName<-"Id"
#'
#' ExtractByPoly(img,poly.shp,In.colName,Out.colName)
#' @export

ExtractByPoly<-function(img,poly.shp,In.colName,Out.colName){
  message("No. of bands - ",length(names(img)))
  aLl<-data.frame()
  poly.shp[[In.colName]]<-as.integer(poly.shp[[In.colName]])
  val<-unique(poly.shp[[In.colName]])
  message("No. of classes - ",length(val))
  for(i in 1:length(val)){
    message(".", appendLF=FALSE)
    ext_val<-extract(img,poly.shp[poly.shp[[In.colName]]==val[i],],df=T)[,-1]
    message(".", appendLF=FALSE)
    ext_val[Out.colName]<-val[i]
    message(".", appendLF=FALSE)
    aLl<-rbind(aLl,ext_val)
    message(".", appendLF=FALSE)
    remove(ext_val)
    message(".",round((i/length(val))*100),"% ", appendLF=FALSE)
  }
  message("\nExtraction Completed")
  return(aLl)
}
