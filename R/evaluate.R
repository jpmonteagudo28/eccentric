evaluate_data <- function(data,ci){

  stopifnot(
             is.numeric(data)
            ,is.numeric(ci)
           )

  clt_evaluated_data <- double(length(data))

  if(is.data.frame(data)){
  clt_evaluated_data <- lapply(data,function(col){
                                sum(ifelse(
                                            abs(col) > ci, 1, 0
                                          )
                                  ) / nrow(data)
                                }
                             )
  return(clt_evaluated_data)
  }
}
