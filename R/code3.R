#' Add a Line shapefile and raster image.
#' @author  Subhadip Datta
#' @param img Raster image
#' @param line.shp Line shapefile with class info
#' @param In.colName Name of the column contain line id's
#' @param Out.colName Name of the output column contain line id's
#' @import raster
#' @examples
#' library(raster)
#' library(ExtractTrainData)
#' img<-brick(system.file("extdata","ras.tif",package = "ExtractTrainData"))
#' line.shp<-shapefile(system.file("extdata","lines.shp",package = "ExtractTrainData"))
#' Out.colName<-In.colName<-"Id"
#' ExtractByLine(img,line.shp,In.colName,Out.colName)
#' @export

ExtractByLine<-function(img,line.shp,In.colName,Out.colName){
  line.shp[[In.colName]]<-as.integer(line.shp[[In.colName]])
  val<-unique(line.shp[[In.colName]])
  ltrl<-c()
  for(i in unique(val)){
    ltr<-rasterize(line.shp[line.shp[[In.colName]]==val[i],],img,field=i)
    ltrl<-append(ltrl,ltr)
  }
  ltrl<-sum(stack(ltrl),na.rm=T)
  ltrl[ltrl==0]<-NA
  rtp<-rasterToPoints(ltrl,spatial = T)
  ev<-ExtractByPoint(img,rtp,"layer",Out.colName)
  return(ev)
}
