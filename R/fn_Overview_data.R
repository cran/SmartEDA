#' Function to generate data dictionary of a data frame
#'
#' This function used to produce the metadata information and data summary
#'
#' @param data a data frame
#' @param type Type 1 is overall data summary; Type 2 is variable level summary
#' @param fun to add any additional statistics into metadata type 2 output, for example: mean, sum, etc..
#' @details
#' This function provides overall and variable level data summary like percentage of missing, variable types etc..
#' \itemize{
#' \item Type = 1, overall data summary (column names are "Descriptions Value")
#' \item Type = 2, variable level summary (column names are "Index Variable_Name Variable_Type Sample_n Missing_count Per_of_Missing No_of_distinct_values" and other statistics)
#'}
#' @examples
#' # Overall data summary
#' ExpData(data=mtcars,type=1)
#' # Variable level data summary
#' ExpData(data=mtcars,type=2)
#' @export ExpData

ExpData <- function(data, type = 1, fun=NULL){
  if (!is.data.frame(data)) stop("data must be a numeric vector or data.frame")
  xx <- as.data.frame(data)
  dd <- sapply( sapply(xx, function(x){
      if(is.character(x)) {
        x[x == ''] <- "missing_row"
        x[is.na(x)] <- "missing_row"
        #round((length(x[is.na(x)]) + length(x[x == '']))/ length(x), 5)
        round(length(x[x == "missing_row"])/ length(x), 5)
      } else {
        round((length(x[is.na(x)]))/ length(x), 5)
      }
    }),
    function(y) {
      if (y == 0.0) xx <- 1
      else if (y > 0.9) xx <- 2
      else if (y >= 0.5) xx <- 3
      else if (y < 0.5) xx <- 4
      }
    )

  p1 <- paste0(round(length(dd[dd == 4]) / length(dd) * 100, 2), "%", " (", length(dd[dd == 4]), ")")
  p2 <- paste0(round(length(dd[dd == 3]) / length(dd) * 100, 2), "%", " (", length(dd[dd == 3]), ")")
  p3 <- paste0(round(length(dd[dd == 2]) / length(dd) * 100, 2), "%", " (", length(dd[dd == 2]), ")")
  p4 <- paste0(round(length(dd[dd == 1]) / length(dd) * 100, 2), "%", " (", length(dd[dd == 1]), ")")

  Date_cnt <- length(names(xx)[unlist(sapply(xx, function(x) {
    clsv <- class(x)[1]
    clsv %in% c("Date", "POSIXct", "POSIXt")
      }))])
  Unif_cnt <- length(names(xx)[unlist(sapply(xx, function(x) length(unique(x[!is.na(x)])) == 1))])
  Unvar_cnt <- length(names(xx)[unlist(sapply(xx, function(x) length(unique(x)) == length(x)))])

     if (type == 1){
       Out_put <- data.frame (rbind(
            c("Sample size (nrow)", nrow(xx)),
            c("No. of variables (ncol)", ncol(xx)),
            c("No. of numeric/interger variables", length(names(xx)[sapply(xx, is.numeric)])),
            c("No. of factor variables", length(names(xx)[sapply(xx, is.factor)])),
            c("No. of text variables", length(names(xx)[sapply(xx, is.character)])),
            c("No. of logical variables", length(names(xx)[sapply(xx, is.logical)])),
            c("No. of identifier variables", Unvar_cnt),
            c("No. of date variables", Date_cnt),
            c("No. of zero variance variables (uniform)", Unif_cnt),
            c("%. of variables having complete cases", p4),
            c("%. of variables having >0% and <50% missing cases", p1),
            c("%. of variables having >=50% and <90% missing cases", p2),
            c("%. of variables having >=90% missing cases", p3)
            )
            )
          names(Out_put) <- c("Descriptions", "Value")
          return(Out_put)
          }
     if (type == 2){
            ## Data Structure
            name_var <- names(xx)
            tt <- sapply(name_var, function(x){
              expdatatype2(xx, x, myfun=fun)
            }
            )

            op <- data.frame(t(tt))
            op$Index <- seq(1, length(name_var), 1)
            rownames(op) <- NULL
            op[, 2] <- as.character(paste0(op[, 2]))
            op[, 3] <- as.character(paste0(op[, 3]))
            op[, 4] <- as.integer(paste0(op[, 4]))
            op[, 5] <- as.integer(paste0(op[, 5]))
            op[, 6] <- as.numeric(paste0(op[, 6]))
            op[, 7] <- as.integer(paste0(op[, 7]))

            if(!is.null(fun)) {
              for(j in 1:length(fun)) op[, 7+j] <- as.numeric(paste0(op[, 7+j]))
            }

            names(op) <- c("Index", "Variable_Name", "Variable_Type","Sample_n","Missing_Count", "Per_of_Missing", "No_of_distinct_values", fun)

            if(!is.null(fun)) op[!op$Variable_Type %in% c("integer", "numeric"), fun] <- NA

            return(op)
        }
 }
