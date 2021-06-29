library('tidyverse')
library('weathercan')
library('ggridges')

# x = stations_search('Calgary', interval = 'day')
# 2205 # old YYC
# 50430 # new yyc
yyc = weather_dl(station_ids = c(2205, 50430), interval = 'day')

# Get labels
maxYr = seq(max(yyc$year),min(yyc$year), -30)
minYr = maxYr - 31
minYr = ifelse(minYr < min(yyc$year), min(yyc$year), minYr)
labs = paste(minYr, maxYr, sep = '-')

# Fix names and clean up data
plot_dat = yyc %>% 
  mutate(year = as.numeric(year), 
         Period = cut(year, breaks = c(rev(as.numeric(minYr)), Inf), labels = rev(labs))) %>% 
  select(Period, date, `Maximum Daily` = max_temp, `Minimum Daily` = min_temp) %>% 
  filter(complete.cases(.))

# 2nd test 
# plot_dat = read_csv('/Users/mattchernos/Desktop/climate-daily.csv') %>% 
#   mutate(year = as.numeric(LOCAL_YEAR), 
#          Period = cut(LOCAL_YEAR, breaks = c(rev(as.numeric(minYr)), Inf), labels = rev(labs))) %>% 
#   select(Period, date = LOCAL_DATE, `Maximum Daily` = MAX_TEMPERATURE, 
#          `Minimum Daily` = MIN_TEMPERATURE) %>% 
#   filter(complete.cases(.))


# Make the plot
plot_dat %>% 
  filter(lubridate::month(date) %in% 6:8) %>%
  gather(Var, Value, -Period, -date) %>% 
  ggplot(aes(x = Value, y = Period)) + 
  geom_density_ridges_gradient(aes(fill = stat(x)), show.legend = F) +
  # stat_density_ridges(quantile_lines = TRUE, quantiles = 5, alpha = 1, fill = NA) + 
  facet_wrap(~Var, scales = 'free_x', strip.position = 'top') + 
  # scale_x_sqrt() + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = expand_scale(mult = c(0.01, 0.25))) +
  scale_fill_viridis_c(option = "C") +
  labs(title = paste(unique(yyc$station_name)[1]), 
       subtitle = "Summer Air Temperatures",
       caption = 'Data obtained from ECCC using the weathercan R package. ') +
  theme_ridges(font_size = 13, grid = T) + 
  theme(axis.title = element_blank(), strip.placement = 'outside', strip.background = element_blank())
ggsave('airtemps.png', width = 8, height = 4)
