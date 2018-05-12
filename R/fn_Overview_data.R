#' Function to generate the overview of a data frame
#'
#' @description This function used to produce summaries of data structure and overview of the data frame.
#' @usage ExpData(data,type=1,DV=NULL)
##' @param data a data frame
##' @param type Type 1 is overview of the data; Type 2 is structure of the data
##' @param DV specifiy the target variable name if any. this is not mandatory
##' @details
##' This function provides overview and structure of the data frames.
##'
##' IF Type=1,overeview of the data (column names are "Descriptions", "Obs")
##'
##' If Type=2,structure of the data (column names are "S.no", "VarName", "VarClass", "VarType")
##'
##' @examples
##' # Overview of the data
##' ExpData(data=mtcars,type=1,DV=NULL)
##' # Structure of the data
##' ExpData(data=mtcars,type=2,DV=NULL)
##' @export ExpData

ExpData <- function(data,type=1,DV=NULL){
  if(!is.data.frame(data)) stop("'data must be a numeric vector or data.frame'")
  xx <- as.data.frame(data)

  dd = sapply(sapply(xx, function(x){round(length(x[is.na(x)])/length(x),2)}),
              function(y) {if(y==0.0) xx=1
              else if(y>0.9) xx=2
              else if(y>=0.5) xx=3
              else if(y<0.5) xx=4
              }
  )

  p1 = paste0(round(length(dd[dd==4])/length(dd)*100,2),"%")
  p2 = paste0(round(length(dd[dd==3])/length(dd)*100,2),"%")
  p3 = paste0(round(length(dd[dd==2])/length(dd)*100,2),"%")
  p4 = paste0(round(length(dd[dd==1])/length(dd)*100,2),"%")

  Date_cnt = length(names(xx)[unlist(sapply(xx, function(x) class(x)%in%c("Date","POSIXct","POSIXt")))])
  Unif_cnt = length(names(xx)[unlist(sapply(xx, function(x) length(unique(x[!is.na(x)]))==1))])
  switch (type,

          {Out_put <-data.frame(rbind(
            c("Total Sample",nrow(xx)),
            c("No. of Variables", ncol(xx)),
            c("No. of Numeric Variables",length(names(xx)[sapply(xx, is.numeric)])),
            c("No. of Factor Variables",length(names(xx)[sapply(xx, is.factor)])),
            c("No. of Text Variables",length(names(xx)[sapply(xx, is.character)])),
            c("No. of Date Variables",Date_cnt),
            c("No. of Zero variance Variables (Uniform)" ,Unif_cnt),
            c("%. of Variables having complete cases" ,p4),
            c("%. of Variables having <50% missing cases" ,p1),
            c("%. of Variables having >50% missing cases", p2),
            c("%. of Variables having >90% missing cases" , p3)))


          names(Out_put) <- c("Descriptions","Obs")
          return(Out_put)
          },
          {
            ## Data Structure
            name_var = names(xx)
            tt = sapply(name_var, function(x){
              cla_var <- as.character(paste0(class(xx[,x])))

              if(is.null(DV)){Typ <- "Independent variable"} else {
                if(x==DV){Typ <- "Dependent variable"} else {Typ <- "Independent variable"}
              }

              if(cla_var!="numeric") {x=paste0(x,"*")}

              mydata = c(S.no=1,VarName=x,VarClass=cla_var,VarType=Typ,VarDescriptions=NULL)
              return(mydata)

            })

            op = data.frame(t(tt))
            op$S.no = seq(1,length(name_var),1)
            rownames(op)<-NULL
            return(op)
          }
  )

}
