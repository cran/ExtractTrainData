#' Add a Point shapefile and raster image.
#' @author  Subhadip Datta
#' @param img Raster image
#' @param point.shp Point shapefile with class info
#' @param In.colName Name of the column contain point id's
#' @param Out.colName Name of the output column contain point id's
#' @import raster
#' @examples
#' library(raster)
#' library(ExtractTrainData)
#' img<-brick(system.file("extdata","ras.tif",package = "ExtractTrainData"))
#' point.shp<-shapefile(system.file("extdata","poin.shp",package = "ExtractTrainData"))
#' Out.colName<-In.colName<-"Id"
#' ExtractByPoint(img,point.shp,In.colName,Out.colName)
#' @export

ExtractByPoint<-function(img,point.shp,In.colName,Out.colName){
  message("No. of bands - ",length(names(img)))
  aLl<-data.frame()
  point.shp[[In.colName]]<-as.integer(point.shp[[In.colName]])
  val<-unique(point.shp[[In.colName]])
  message("No. of classes - ",length(val))
  for(i in 1:length(val)){
    message(".", appendLF=FALSE)
    trp<-extract(img,point.shp[point.shp[[In.colName]]==val[i],],df=T)[,-1]
    message(".", appendLF=FALSE)
    trp[Out.colName]<-val[i]
    message(".", appendLF=FALSE)
    aLl<-rbind(aLl,trp)
    message(".", appendLF=FALSE)
    remove(trp)
    message(".",round(i/length(val)*100),"% ", appendLF=FALSE)
  }
  message("\nExtraction Completed")
  return(aLl)
}
