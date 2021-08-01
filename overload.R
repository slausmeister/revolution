# OLD FILE


source("incidence_lk.R")

# infections time series
tibble(date=days_since_2020) %>%
  left_join(rki_data, by=c("date"="Meldedatum"))  %>%
  group_by(date) %>%
  summarise(cases=sum(AnzahlFall), not_traced=1-sum(IstErkrankungsbeginn*AnzahlFall)/sum(AnzahlFall),
    traced_total=sum(IstErkrankungsbeginn*AnzahlFall)) %>%
  # the days for which we have no infection data for are days with 0 infections
  mutate(cases=replace_na(cases, 0), not_traced=replace_na(not_traced, 0), sti=calc_sti_germany(),
    traced_total=replace_na(traced_total, 0)) %>%
  mutate(not_traced=stats::filter(not_traced, rep(1/10, 10), sides=1)) ->
  traced_cases

# (ggplot(data=traced_cases, aes(x=date, y=not_traced)) + geom_line(color="Orange") +
#   geom_line(aes(x=date, y=sti/200), color="Blue")) %>% print()

rki_data %>%
  filter(IstErkrankungsbeginn==1) %>%
  mutate(diff=as.numeric(Meldedatum - Refdatum)) %>%
  group_by(diff) %>%
  count() %>%
  filter(abs(diff)<30) %>%
  ungroup() ->
  report_diffs



(ggplot(data=report_diffs, aes(x=diff, y=n)) + geom_bar(stat='identity', fill="Blue")) %>% print()
