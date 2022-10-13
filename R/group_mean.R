#' Summarise mean value for each group
#' Like summarise(), group_mean()creates a new data frame. It's a combination of grouping by one variable and caculating measurements in each group. It's also useful in calculating an average for technical repeats, such as immunohistochemitry quantification (where you have multiple measurements for one sample).
#'
#' @param data input dataset, including at least a column of grouping information and a column of measurements
#' @param var1 the variable to group by, which can be condition/genotype/treament..
#' @param var2 the variable to calculate means for
#'
#' @return a tibble with the group names, sample size of each group, and means for each group.
#' @export
#'
#' @examples
#' group_mean(data = PlantGrowth, var1 = group, var2 = weight)
group_mean <- function(data, var1, var2) {

  data  %>%
    dplyr::group_by({{var1}})  %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean({{var2}}),
      .groups = "drop")

  }
