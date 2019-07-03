#' Distributions of numeric variables
#'
#'
#' @description This function automatically scans through each variable and creates density plot, scatter plot and box plot for continuous variable.
#' @usage ExpNumViz (data,target=NULL,type=1,nlim=NULL,fname=NULL,col=NULL,Page=NULL,
#' sample=NULL,scatter=FALSE,gtitle=NULL,theme="Default")
##' @param data dataframe or matrix
##' @param target target variable
##' @param type 1 (boxplot by category and overall), 2 (boxplot by category only), 3 (boxplot for overall)
##' @param nlim numeric variable unique limit. Default nlim is 3, graph will exclude the numeric variable which is having less than 'nlim' unique value
##' @param fname output file name
##' @param col define the fill color for box plot. Number of color should be equal to number of categories in target variable
##' @param Page output pattern. if Page=c(3,2), It will generate 6 plots with 3 rows and 2 columns
##' @param sample random selection of plots
##' @param scatter option to run scatter plot between all the numerical variables (default scatter=FALSE)
##' @param gtitle chart title
##' @param theme extra themes, geoms, and scales for 'ggplot2' (use theme from ggthemes package)
##' @seealso \code{\link[ggplot2:geom_boxplot]{geom_boxplot}}
##'
##' @details
##' This function automatically scan each variables and generate a graph based on the user inputs. Graphical representation includes scatter plot, box plot and density plots.
##' If input "target" is continuous then output is scatter plots
##'
##' If input "target" is categorical then output is box plot.
##'
##' If input "target" is NULL, means there is no target variable and this will generate density plot for all numeric features
##'
##' To plot only scatter plot : target is categorical or NULL and scatter = TRUE
##'
##' @return returns collated graphs in PDF or JPEG format
##'
##' Scatter plot for numeric data
##'
##' Density plot for numeric data
##'
##' Boxplot – by overall
##'
##' Boxplot – by group (target variable)
##'
##' Boxplot – by overall and group (target variable)
##'
##' @importFrom grDevices colors
##' @importFrom gridExtra marrangeGrob
##' @importFrom sampling srswor
##' @importFrom utils combn
##' @examples
#' ## Generate Boxplot by category
#' ExpNumViz(mtcars,target="gear",type=2,nlim=25,Page = c(2,2),sample=4)
#' ## Generate Density plot
#' ExpNumViz(mtcars,target=NULL,type=3,nlim=25,Page = c(2,2),sample=4)
#' ## Generate Scatter plot by Dependent variable
#' ExpNumViz(mtcars,target="carb",type=3,nlim=25,Page = c(2,2),sample=4)
#' ## Generate Scatter plot for all the numerical variables
#' ExpNumViz(mtcars,target="gear",scatter=TRUE,gtitle="Scatter plot",theme="Default",sample=2)
#' ExpNumViz(mtcars,target=NULL,scatter=TRUE,gtitle="Scatter plot",theme="Default",sample=2)
##' @export ExpNumViz

ExpNumViz = function(data,target=NULL,type=1,nlim=NULL,fname=NULL,col=NULL,Page=NULL,sample=NULL,scatter=FALSE,gtitle=NULL,theme="Default") {

  if(!is.data.frame(data)) stop("'data must be a numeric vector or data.frame'")
  xx <- as.data.frame(data)
  comma <-NULL
  XX <- YY <- ZZ <- NULL
  num_var = names(xx)[sapply(xx, is.numeric)]
  if(length(num_var)==0) stop("there is no numeric variable in the data frame")
  if(is.null(nlim))
    {
    num_var <- num_var[sapply(xx[,num_var], function(x){length(unique(na.omit(x)))>3})]
    }
  else
    {
      num_var <- num_var[sapply(xx[,num_var], function(x){length(unique(na.omit(x)))>=nlim})]
    }
  if(!is.null(sample))
    {
    if(sample>length(num_var))
      {
      num_var <- num_var
      }
    else
    {
    num_var <- num_var[srswor(sample,length(num_var))==1]
    }
  }
  wrap_40 <- wrap_format(40)
  if(theme=="Default")
    {
    mytheme <- theme(axis.text.x=element_text(angle=90,vjust=.5,hjust=0.95,size=8,colour='grey20'),
                     axis.text.y=element_text(vjust=.5,hjust=0.95,size=8,colour='grey20'),
                     plot.title = element_text(hjust = 0.5, face = "bold", colour = "#5F9EA0", size = 12))+
      theme(axis.line = element_line(size=1, colour = "black"),
            panel.grid.major = element_line(colour = "#d3d3d3",linetype = "dashed"), panel.grid.minor = element_blank(),
            panel.border = element_blank(), panel.background = element_blank())
    } else {mytheme <- theme }

  if(!is.null(gtitle)) {chart_title <- gtitle} else chart_title<-NULL

  #------------------------ Adding scatter plot option for all the numeric features

  if(isTRUE(scatter)) {
    if(length(num_var) < 2) {stop("To plot scatter diagram, the sample number of variables must be 2 or more")}
    else {plot_comb <- combn(num_var,2)}
  }

  #--------------------------------------------------------------------------------

  if(is.null(target)){
    if(isTRUE(scatter)){
      if(!is.null(col) & length(col)==1) {colr <- col} else {colr <- "blue"}
      plot_l <- lapply(c(1:ncol(plot_comb)),function(x)
      {
        xi <- plot_comb[,x][1]; yi <- plot_comb[,x][2]
        xd <- na.omit(subset(xx,select=c(xi,yi)))
        names(xd) <- c("XX","YY")
        ggplot(xd, aes(x=XX, y=YY)) +
          geom_point(colour = colr, size = 2) +
          xlab(xi)+ylab(yi)+ggtitle(wrap_40(paste(chart_title," ",xi," vs ",yi))) + mytheme

      })
    }
    else {
      ## Histogram for all numeric variable - Univariate graph
      plot_l <- lapply(num_var, function(j){
        x <- na.omit(subset(xx,select= j))
        y = xx[,j]
        p = ggplot(data= x,aes_string(x = names(x))) + geom_line(stat = 'density', size = 1,alpha = 1.0) +
          xlab(paste0((colnames(x)), '\n', 'Skewness: ',round(ExpSkew(y,type="moment"), 2), ' Kurtosis: ',
                      round(ExpKurtosis(y,type="excess"), 2)))+ mytheme
        return(p)
      })
    }
  }
  else
    {
    if(isTRUE(scatter)){
      target1 = xx[,target]
      nlent <- length(unique(target1))
      if(!is.null(col) & (nlent==length(col))) {colr=col} else {
        mclor <- colors()[c(11:150,350:640)]
        colr <- mclor[srswor(nlent,length(mclor))==1]
      }
        if(length(unique(na.omit(target1)))<=10)
          {
            plot_l <- lapply(c(1:ncol(plot_comb)),function(x){
            xi <- plot_comb[,x][1]; yi <- plot_comb[,x][2]
            xd <- na.omit(subset(xx,select=c(xi,yi,target)))
            names(xd) <- c("XX","YY","ZZ")
            xd$ZZ <- as.factor(paste0(xd$ZZ))
            ggplot(xd, aes(x=XX, y=YY)) +
              geom_point( size = 2,aes(color=ZZ)) +
              scale_color_manual(name=target,values=colr)+
                xlab(xi)+ylab(yi)+ggtitle(wrap_40(paste(chart_title," ",xi," vs ",yi)))+ mytheme
            })
            }
      else stop("IF scatter option is TRUE then 'target should be categorical' else 'change scatter = FALSE' ")
      } else
        {
    ## Box plot or scatter depends on target variable - Bivariate graph
    target1 = xx[,target]
    if(is.numeric(target1)& length(unique(na.omit(target1)))>=6)
      {
      ## Scatter plot
      if(is.null(col)){col="#5F9EA0"}
      num_var1 = num_var[!num_var %in% target]
      plot_l <- lapply(num_var1, function(j){
        x <- na.omit(subset(xx,select=c(j,target)))
        ggplot(x, aes_string(x = names(x)[2],y=names(x)[1])) +
          geom_point(colour = col, size = 2) +
          scale_x_continuous(labels = comma) +
          scale_y_continuous(labels = comma) + mytheme})
      }
    else
      {
        target1 <- as.factor(as.character(paste0(target1)))
      nlev_tar <- nlevels(target1)
      ## Box plot by target - Bivariate
      plot_l <- lapply(num_var, function(j){
        mdat = subset(xx,select = c(target,j))
        GP <- NULL
        NV <- NULL
        names(mdat) = c("GP","NV")
        switch (type,
                {mdat$GP = as.character(paste0(mdat$GP))
                if (anyNA(mdat$GP)) {mdat$GP = addNA(mdat$GP)}
                mdat1 = mdat
                mdat1$GP = "All"
                gdata = rbind(mdat,mdat1)
                nlevel = length(unique(gdata$GP))
                },
                {gdata = mdat
                gdata$GP = as.character(paste0(gdata$GP))
                if (anyNA(gdata$GP)) {gdata$GP = addNA(mdat$GP)}
                nlevel = length(unique(gdata$GP))},
                {gdata = mdat
                gdata$GP = as.character(paste0(gdata$GP))
                gdata$GP ="ALL"
                nlevel = length(unique(gdata$GP))})

        cp=with(gdata,aggregate(NV,by=list(GP), function(x)sum(x,na.rm=T)))
          if(!is.null(col) & nlevel==1){fill_1 <- col} else
            if(is.null(col) & nlevel==1){fill_1 <- c("orange")} else
              if(is.null(col) & nlevel==2){fill_1 <- c("#5F9EA0", "#E1B378")} else
                if(is.null(col) & nlevel>2){
                  mclor <- colors()[c(11:150,350:640)]
                  fill_1 <- mclor[srswor(nlevel,length(mclor))==1]} else
                  if(!is.null(col) & nlevel>=2){fill_1 <- col} else {fill_1 <- col}

          gg1 <- ggplot(gdata, aes(y=NV, x=GP)) +
          geom_boxplot(fill=fill_1)+xlab(target)+ylab(j)+ggtitle(wrap_40(paste(chart_title," ",j," vs ",target)))+
          scale_x_discrete(labels = wrap_format(8))+
          theme(axis.text.x=element_text(angle=0,vjust=.5,size=8,colour='grey20'),
                plot.title = element_text(hjust = 0.5, face = "bold", colour = "#5F9EA0", size = 12))+ mytheme
        return(gg1)})
      }
    }
  }
  if (!is.null(fname))
    {
      filname <- paste(fname,"pdf",sep = ".")
      if (!is.null(Page))
      {
        pn = length(plot_l)
        nc = Page[2]
        nr = Page[1]
        if((nc*nr)>pn+3)stop("reduce the matrix dimension from Page(r,c)")
        pg = ceiling((nc*nr)/pn)
        gspl <- split(plot_l, (seq_along(plot_l)-1) %/% pn)
        gplt <- lapply(gspl, function(g) marrangeGrob(grobs = g, layout_matrix = matrix(data = seq(1,pn),nrow = nr,ncol = nc)))
        options(warn = -1)
        pdf(file=filname,width=10,height = 14,onefile=T,title="Summary analysis",paper = "a4r")
        print(gplt)
        dev.off()
      }
    else
      {
        pdf(file=filname,width=10,height = 6,onefile=T,paper = "USr")
        print(plot_l)
        dev.off()
        message(paste0("\n\n",filname," is generated at \"", getwd(),"\"."))
      }
    }
  else
    {
    if (!is.null(Page))
      {
      pn = length(plot_l)
      nc = Page[2]
      nr = Page[1]
      if((nc*nr)>pn+3)stop("reduce the matrix dimension from Page(r,c)")
      pg = ceiling((nc*nr)/pn)
      gspl <- split(plot_l, (seq_along(plot_l)-1) %/% pn)
      gplt <- lapply(gspl, function(g) marrangeGrob(grobs = g, layout_matrix = matrix(data = seq(1,pn),nrow = nr,ncol = nc)))
      return(gplt)
    }
      else
        {
          return(plot_l)
        }
    }
  }
