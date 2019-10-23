#' calculate_DRG
#'
#' @param type
#'
#' @return list of double
#' @export
#'
#' @examples
calculate_DRG <- function(type) {
  DRG_spread <- DRG %>%
    select(DRG.Definition, Provider.Id, Provider.State, Average.Medicare.Payments) %>%
    spread(DRG.Definition, Average.Medicare.Payments)
  switch(type,
         mean = apply(DRG_spread[,3:102], MARGIN = 2, mean, na.rm = T),
         median = apply(DRG_spread[,3:102], MARGIN = 2, median, na.rm = T),
         sd = apply(DRG_spread[,3:102], MARGIN = 2, sd, na.rm = T))
}
#calculate_DRG("mean")
