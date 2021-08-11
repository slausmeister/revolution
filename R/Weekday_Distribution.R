ggplot2::ggplot#'Comparison of offices from different federal states regarding the number of reported cases on the different weekdays.
#'
#'\code{office_case_distribution()} is used to create a plot showing the mean number of reported cases in Germany regarding the specific weekdays
#'as well as the number of reported cases regarding the specific weekdays of the two given federal states. To see how fast or slow offices of different
#'federal states are reporting cases and how they differ to the mean number of reported cases overall.
#'All values are standardized.
#'
#'@param bundesland_1 A string of the desired federal state.
#'
#'@param bundesland_2 A string of the other desired federal state for the comparison.
#'
#'@return A plot that shows the standardized amount of cases that get reported per day in the given federal states
#'and the standardized amount of all reported cases per day.
#'
#'@examples office_case_distribution("Hamburg","Niedersachsen")

#'@import ggplot2 ggstream
#'@export
office_case_distribution <- function(bundesland_1 = "Baden-Württemberg", bundesland_2 = "Schleswig-Holstein"){
  rki_data$weekday <- weekdays(as.Date(rki_data$Meldedatum))
  rki_data %>%
    dplyr::group_by(weekday) %>%
    dplyr::summarize(count_weekday = dplyr::n()) -> weekday_distribution
  total_rows <- dim(rki_data)[1]
  weekday_distribution %>%
    dplyr::mutate(dplyr::across(count_weekday, ~ ./total_rows)) -> mean_distribution

  mean_distribution$weekday <- factor(mean_distribution$weekday, levels= c("Sunday", "Monday",
                                                                               "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

  mean_distribution <- mean_distribution[order(mean_distribution$weekday), ]

  rki_data %>%
    dplyr::group_by(Bundesland, weekday) %>%
    dplyr::summarize(count_weekday = dplyr::n()) -> bundesland_distribution

  bundesland_distribution %>% dplyr::filter(Bundesland == bundesland_1) %>%
    dplyr::mutate(dplyr::across(count_weekday, ~ ./sum(count_weekday))) -> bundesland_1
  bundesland_1

  bundesland_distribution %>% dplyr::filter(Bundesland == bundesland_2) %>%
    dplyr::mutate(dplyr::across(count_weekday, ~ ./sum(count_weekday))) -> bundesland_2
  bundesland_2

  office_case_distribution_plot <- ggplot2::ggplot()+
    #geom_line(data=mean_distribution, ggplot2::aes(x = weekday, y = count_weekday, group = 1, color= "Durchschnitt"))+
    geom_line(data=bundesland_1,ggplot2::aes(x = weekday, y = count_weekday, group = 1, color=Bundesland))+
    geom_line(data=bundesland_2,ggplot2::aes(x = weekday, y = count_weekday, group = 1, color=Bundesland))

  return(office_case_distribution_plot)
}
