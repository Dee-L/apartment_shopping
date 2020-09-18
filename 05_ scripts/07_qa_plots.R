source("D:/Coding/R/Public/pkgs_and_fxns/load_pkgs_and_fxns.R")

setwd("D:/Coding/R/Apartment shopping/script output/") #setwd

features_engineered <-
  read.csv(file = "04 features engineered/2019-08-21.csv", header = T)

working_data <- features_engineered

#Reordering day_of_week
working_data[["dayofweek_sold"]] %<>%
  factor(x = .,levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun"))


###Plots


dir.create(paste0("05 qa plots/", now() %>% as.Date), recursive = T)
setwd(paste0("05 qa plots/", now() %>% as_date))


##price


#Density plot
price_density <- my_basic_density_plot(
  df = working_data[!is.na(working_data[["price"]]), ],
  x = "price",
  title = "Major left skew indicates that for the parameters I scraped, more
  properties go for higher prices.")

price_density

save_plot_as_html(price_density)


##price by city


#Stripchart
city_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "city",
  y = "price",
  title = "Clearly Solna and Sundbeberg are more expensive.")

city_stripchart

save_plot_as_html(city_stripchart)

#Violin
city_violin <- my_violin_plot(
  df = working_data,
  x = "city",
  y = "price",
  title = "This shows that every city hits a 'ceiling' at the price
  cutoff I used for scraping.")

city_violin

save_plot_as_html(city_violin)

#Conditional density
city_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "city",
  title = "Most of the cheapest apartments are in Nacka.")

city_conditionaldensity

save_plot_as_html(city_conditionaldensity)


##price by year_sold


#Stripchart
year_sold_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "year_sold",
  y = "price",
  title = "I have much fewer data points from 2014.")

year_sold_stripchart

save_plot_as_html(year_sold_stripchart)

#Violin
year_sold_violin <- my_violin_plot(
  df = working_data,
  x = "year_sold",
  y = "price",
  title = "All recent years are left-skewed and hitting a ceiling
  where I cut off the upper limit on properties to search.")

year_sold_violin

save_plot_as_html(year_sold_violin)

#Conditional density
year_sold_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "year_sold",
  title = "Most of the properties for 2.5m are from 2016 or later.
  More than 1/3 of the properties for 1.5m are from before 2016.")

year_sold_conditionaldensity

save_plot_as_html(year_sold_conditionaldensity)


##price by year_built


#Stripchart
year_built_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "year_built",
  y = "price",
  title = "Apartments built a very long time ago tend are rarely inexpensive.")

year_built_stripchart

save_plot_as_html(year_built_stripchart)

#Violin
year_built_violin <- my_violin_plot(
  df = working_data,
  x = "year_built",
  y = "price",
  title = "It may be a bit easier to see in this chart that especially
  older apartments are less likely to be inexpensive.")

year_built_violin

save_plot_as_html(year_built_violin)

#Conditional density
year_built_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "year_built",
  title = "The most expensive aparments are from 1943 or later. Most of this price
  range is made up of apartments from 1958 or later.")

year_built_conditionaldensity

save_plot_as_html(year_built_conditionaldensity)


##price by age_when_sold


#Stripchart
age_when_sold_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "age_when_sold",
  y = "price",
  title = "Looks like very old apartments tend only to be expensive.")

age_when_sold_stripchart

save_plot_as_html(age_when_sold_stripchart)

#Violin
age_when_sold_violin <- my_violin_plot(
  df = working_data,
  x = "age_when_sold",
  y = "price",
  title = "Apartments with the greatest range in prices are 40 to 60 years old.")

age_when_sold_violin

save_plot_as_html(age_when_sold_violin)

#Conditional density
age_when_sold_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "age_when_sold",
  title = "Most apartments at 2.5m SEK are 59 years or older. Most apartments
  from 1.5m to 2.0m SEK are 45 years or older.")

age_when_sold_conditionaldensity

save_plot_as_html(age_when_sold_conditionaldensity)


##price by quarterofyear_sold


#Stripchart
quarterofyear_sold_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "quarterofyear_sold",
  y = "price",
  title = "No obvious relationship.")

quarterofyear_sold_stripchart

save_plot_as_html(quarterofyear_sold_stripchart)

#Violin
quarterofyear_sold_violin <- my_violin_plot(
  df = working_data,
  x = "quarterofyear_sold",
  y = "price",
  title = "No obvious relationship.")

quarterofyear_sold_violin

save_plot_as_html(quarterofyear_sold_violin)

#Conditional density
quarterofyear_sold_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "quarterofyear_sold",
  title = "Nearly 2/3 of apartments from 1.75m SEK and up are sold in Q1 or Q2.")

quarterofyear_sold_conditionaldensity

save_plot_as_html(quarterofyear_sold_conditionaldensity)


##price by monthofyear_sold


#Stripchart
monthofyear_sold_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "monthofyear_sold",
  y = "price",
  title = "Prices aparently go up in the summer.")

monthofyear_sold_stripchart

save_plot_as_html(monthofyear_sold_stripchart)

#Violin
monthofyear_sold_violin <- my_violin_plot(
  df = working_data,
  x = "monthofyear_sold",
  y = "price",
  title = "The most inexpensive apartments are in January and December.")

monthofyear_sold_violin

save_plot_as_html(monthofyear_sold_violin)

#Conditional density
monthofyear_sold_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "monthofyear_sold",
  title = "Fewer apartments are sold in July.")

monthofyear_sold_conditionaldensity

save_plot_as_html(monthofyear_sold_conditionaldensity)


##price by monthofquarter_sold


#Stripchart
monthofquarter_sold_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "monthofquarter_sold",
  y = "price",
  title = "No obvious relationship.")

monthofquarter_sold_stripchart

save_plot_as_html(monthofquarter_sold_stripchart)

#Violin
monthofquarter_sold_violin <- my_violin_plot(
  df = working_data,
  x = "monthofquarter_sold",
  y = "price",
  title = "No obvious relationship.")

monthofquarter_sold_violin

save_plot_as_html(monthofquarter_sold_violin)

#Conditional density
monthofquarter_sold_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "monthofquarter_sold",
  title = "Tendency for more aparments to be sold in the beginning of the quarter
  rather than the end.")

monthofquarter_sold_conditionaldensity

save_plot_as_html(monthofquarter_sold_conditionaldensity)


##price by weekofyear_sold


#Stripchart
weekofyear_sold_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "weekofyear_sold",
  y = "price",
  title = "No obvious relationship.")

weekofyear_sold_stripchart

save_plot_as_html(weekofyear_sold_stripchart)

#Violin
weekofyear_sold_violin <- my_violin_plot(
  df = working_data,
  x = "weekofyear_sold",
  y = "price",
  title = "No obvious relationship.")

weekofyear_sold_violin

save_plot_as_html(weekofyear_sold_violin)

#Conditional density
weekofyear_sold_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "weekofyear_sold",
  title = "This shows there is basically no relationship- basically even
  number of sales of apartments in each week of the year.")

weekofyear_sold_conditionaldensity

save_plot_as_html(weekofyear_sold_conditionaldensity)


##price by weekofquarter_sold


#Stripchart
weekofquarter_sold_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "weekofquarter_sold",
  y = "price",
  title = "Fewer cheap apartments just after mid-quarter (agents are greedy and
  not yet desperate?)")

weekofquarter_sold_stripchart

save_plot_as_html(weekofquarter_sold_stripchart)

#Violin
weekofquarter_sold_violin <- my_violin_plot(
  df = working_data,
  x = "weekofquarter_sold",
  y = "price",
  title = "Any relationships are very weak.")

weekofquarter_sold_violin

save_plot_as_html(weekofquarter_sold_violin)

#Conditional density
weekofquarter_sold_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "weekofquarter_sold",
  title = "This shows there is basically no relationship. Each week of the quarter
  has approximately the same number of sales.")

weekofquarter_sold_conditionaldensity

save_plot_as_html(weekofquarter_sold_conditionaldensity)


##price by weekofmonth_sold


#Stripchart
weekofmonth_sold_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "weekofmonth_sold",
  y = "price",
  title = "Perhaps cheapest at week 3 (before paycheck).")

weekofmonth_sold_stripchart

save_plot_as_html(weekofmonth_sold_stripchart)

#Violin
weekofmonth_sold_violin <- my_violin_plot(
  df = working_data,
  x = "weekofmonth_sold",
  y = "price",
  title = "Relationship is very weak.")

weekofmonth_sold_violin

save_plot_as_html(weekofmonth_sold_violin)

#Conditional density
weekofmonth_sold_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "weekofmonth_sold",
  title = "Perhaps fewer sales in week 1 of the month.")

weekofmonth_sold_conditionaldensity

save_plot_as_html(weekofmonth_sold_conditionaldensity)


##price by dayofyear_sold


#Stripchart
dayofyear_sold_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "dayofyear_sold",
  y = "price",
  title = "No obvious relationship.")

dayofyear_sold_stripchart

save_plot_as_html(dayofyear_sold_stripchart)

#Violin
dayofyear_sold_violin <- my_violin_plot(
  df = working_data,
  x = "dayofyear_sold",
  y = "price",
  title = "Apartments sold from day 200-215 tend to never be cheap.
  This is early July.")

dayofyear_sold_violin

save_plot_as_html(dayofyear_sold_violin)

#Conditional density
dayofyear_sold_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "dayofyear_sold",
  title = "This replicates the earlier finding regarding Q sales- there are more
  sales in the first quarter since more than 1/3 of apartments are sold by day 100.")

dayofyear_sold_conditionaldensity

save_plot_as_html(dayofyear_sold_conditionaldensity)


##price by dayofmonth_sold


#Stripchart
dayofmonth_sold_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "dayofmonth_sold",
  y = "price",
  title = "If there are patterns, they are not obvious.")

dayofmonth_sold_stripchart

save_plot_as_html(dayofmonth_sold_stripchart)

#Violin
dayofmonth_sold_violin <- my_violin_plot(
  df = working_data,
  x = "dayofmonth_sold",
  y = "price",
  title = "The least expensive apartments were sold before the 22 of the month.
  This relationship is tenuous, though.")

dayofmonth_sold_violin

save_plot_as_html(dayofmonth_sold_violin)

#Conditional density
dayofmonth_sold_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "dayofmonth_sold",
  title = "This shows there is basically no relationship.")

dayofmonth_sold_conditionaldensity

save_plot_as_html(dayofmonth_sold_conditionaldensity)


##price by dayofweek_sold


#Stripchart
dayofweek_sold_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "dayofweek_sold",
  y = "price",
  title = "No cheap apartments on Mondays (agents are not yet desperate?).")

dayofweek_sold_stripchart

save_plot_as_html(dayofweek_sold_stripchart)

#Violin
dayofweek_sold_violin <- my_violin_plot(
  df = working_data,
  x = "dayofweek_sold",
  y = "price",
  title = "No cheap apartments on Mondays.")

dayofweek_sold_violin

save_plot_as_html(dayofweek_sold_violin)

#Conditional density
dayofweek_sold_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "dayofweek_sold",
  title = "Most sales occur on wed or thu, with very few on sat, sun, or mon.")

dayofweek_sold_conditionaldensity

save_plot_as_html(dayofweek_sold_conditionaldensity)


##price by swedish_red_or_pink_day


#Stripchart
swedish_red_or_pink_day_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "swedish_red_or_pink_day",
  y = "price",
  title = "Fewer cheap apartments on holidays.")

swedish_red_or_pink_day_stripchart

save_plot_as_html(swedish_red_or_pink_day_stripchart)

#Violin
swedish_red_or_pink_day_violin <- my_violin_plot(
  df = working_data,
  x = "swedish_red_or_pink_day",
  y = "price",
  title = "Fewer cheap apartments on holidays.")

swedish_red_or_pink_day_violin

save_plot_as_html(swedish_red_or_pink_day_violin)

#Conditional density
swedish_red_or_pink_day_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "swedish_red_or_pink_day",
  title = "Practically no aparments occur on red or pink days.")

swedish_red_or_pink_day_conditionaldensity

save_plot_as_html(swedish_red_or_pink_day_conditionaldensity)


##price by swedish_pinch_day


#Stripchart
swedish_pinch_day_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "swedish_pinch_day",
  y = "price",
  title = "Fewer inexpensive apartments on pinch days.")

swedish_pinch_day_stripchart

save_plot_as_html(swedish_pinch_day_stripchart)

#Violin
swedish_pinch_day_violin <- my_violin_plot(
  df = working_data,
  x = "swedish_pinch_day",
  y = "price",
  title = "Fewer inexpensive apartments on pinch days.")

swedish_pinch_day_violin

save_plot_as_html(swedish_pinch_day_violin)

#Conditional density
swedish_pinch_day_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "swedish_pinch_day",
  title = "Practically no sales occur on pinch days.")

swedish_pinch_day_conditionaldensity

save_plot_as_html(swedish_pinch_day_conditionaldensity)


##price by swedish_day_off


#Stripchart
swedish_day_off_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "swedish_day_off",
  y = "price",
  title = "Fewer inexpensive apartments on day off.")

swedish_day_off_stripchart

save_plot_as_html(swedish_day_off_stripchart)

#Violin
swedish_day_off_violin <- my_violin_plot(
  df = working_data,
  x = "swedish_day_off",
  y = "price",
  title = "Fewer inexpensive apartments on day off.")

swedish_day_off_violin

save_plot_as_html(swedish_day_off_violin)

#Conditional density
swedish_day_off_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "swedish_day_off",
  title = "Practically no apartments are sold on days off.")

swedish_day_off_conditionaldensity

save_plot_as_html(swedish_day_off_conditionaldensity)


##price by floor


#Stripchart
floor_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "floor",
  y = "price",
  title = "There is a large price range in the first 6 floors, and thereafter
  it gets more expensive the higher one lives.")

floor_stripchart

save_plot_as_html(floor_stripchart)

#Violin
floor_violin <- my_violin_plot(
  df = working_data,
  x = "floor",
  y = "price",
  title = "Even on the lower floors, the typical price is greater than 2m.")

floor_violin

save_plot_as_html(floor_violin)

#Conditional density
floor_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "floor",
  title = "Most apartments are floor 4 or lower.")

floor_conditionaldensity

save_plot_as_html(floor_conditionaldensity)


##price by rooms


#Stripchart
rooms_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "rooms",
  y = "price",
  title = "There is no chance of getting more than 2 rooms without
  paying at least 1.5m.")

rooms_stripchart

save_plot_as_html(rooms_stripchart)

#Violin
rooms_violin <- my_violin_plot(
  df = working_data,
  x = "rooms",
  y = "price",
  title = "There is almost no chance of getting an apartment under 1.5m SEK
  with more than 1 room.")

rooms_violin

save_plot_as_html(rooms_violin)

#Conditional density
rooms_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "rooms",
  title = "Throughout the entire price range, 1/3 or more apartments are 1
  room only.")

rooms_conditionaldensity

save_plot_as_html(rooms_conditionaldensity)


##price by kvm


#Stripchart
kvm_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "kvm",
  y = "price",
  title = "There appears to be a gradual increase in price per square meter.")

kvm_stripchart

save_plot_as_html(kvm_stripchart)

#Violin
kvm_violin <- my_violin_plot(
  df = working_data,
  x = "kvm",
  y = "price",
  title = "This does not reveal much more than the stripchart.")

kvm_violin

save_plot_as_html(kvm_violin)

#Conditional density
kvm_conditionaldensity <- my_conditional_density_plot(
  df = working_data,
  x = "price",
  layers = "kvm",
  title = "2.5m will probably buy you 36 kvm. 2.0m will probably buy you 33
  kvm, and 1.5m will buy you 28 kvm.")

kvm_conditionaldensity

save_plot_as_html(kvm_conditionaldensity)


##price by area


#Stripchart
area_stripchart <- my_stripchart_plot(
  df = working_data,
  x = "area",
  y = "price",
  title = "This shows that there are areas that have more variable prices,
  but without grouping them by city, it is hard to really see anything interesting.")

area_stripchart

save_plot_as_html(area_stripchart)

#Violin
area_violin <- my_violin_plot(
  df = working_data,
  x = "area",
  y = "price",
  title = "Like the last chart, one can see that some areas have a larger
  price range than others, but it is hard to see trends without grouping into cities.")

area_violin

save_plot_as_html(area_violin)


##price by asking_price


#Scatter
asking_price_scatter <- my_scatter_plot(
  df = working_data,
  x = "asking_price",
  y = "price",
  title = "There are some outliers, but the vast majority have a final price
  greater than the asking price.")

asking_price_scatter

save_plot_as_html(asking_price_scatter)


##price by running_costs


#Scatter
running_costs_scatter <- my_scatter_plot(
  df = working_data,
  x = "running_costs",
  y = "price",
  title = "Running costs are pretty concentrated below 7000 SEK, but there is
  a slight positive corrlation to price.")

running_costs_scatter

save_plot_as_html(running_costs_scatter)


##price by avgift

#Scatter
avgift_scatter <- my_scatter_plot(
  df = working_data,
  x = "avgift",
  y = "price",
  title = "Perhaps a positive correlation.")

avgift_scatter

save_plot_as_html(avgift)


##price by kvm_per_room

#Scatter
kvm_per_room_scatter <- my_scatter_plot(
  df = working_data,
  x = "kvm_per_room",
  y = "price",
  title = "There may be a small positive correlation.")

kvm_per_room_scatter

save_plot_as_html(kvm_per_room)


##price by avgift_per_kvm

#Scatter
avgift_per_kvm_scatter <- my_scatter_plot(
  df = working_data,
  x = "avgift_per_kvm",
  y = "price",
  title = "There are some strange looking outliers here - perhaps from the data
  imputation and/or interpolation.")

avgift_per_kvm_scatter

save_plot_as_html(avgift_per_kvm)


##price by avgift_per_rooms

#Scatter
avgift_per_rooms_scatter <- my_scatter_plot(
  df = working_data,
  x = "avgift_per_rooms",
  y = "price",
  title = "There may be a small positive correlation.")

avgift_per_rooms_scatter

save_plot_as_html(avgift_per_rooms)


##price by avgift_per_asking_price

#Scatter
avgift_per_asking_price_scatter <- my_scatter_plot(
  df = working_data,
  x = "avgift_per_asking_price",
  y = "price",
  title = "There may be a small negative correlation.")

avgift_per_asking_price_scatter

save_plot_as_html(avgift_per_asking_price)


##price by running_costs_per_kvm

#Scatter
running_costs_per_kvm_scatter <- my_scatter_plot(
  df = working_data,
  x = "running_costs_per_kvm",
  y = "price",
  title = "No obvious correlation.")

running_costs_per_kvm_scatter

save_plot_as_html(running_costs_per_kvm)


##price by running_costs_per_rooms

#Scatter
running_costs_per_rooms_scatter <- my_scatter_plot(
  df = working_data,
  x = "running_costs_per_rooms",
  y = "price",
  title = "There may be a small positive correlation.")

running_costs_per_rooms_scatter

save_plot_as_html(running_costs_per_rooms)


##price by running_costs_per_asking_price

#Scatter
running_costs_per_asking_price_scatter <- my_scatter_plot(
  df = working_data,
  x = "running_costs_per_asking_price",
  y = "price",
  title = "No obvious correlation.")

running_costs_per_asking_price_scatter

save_plot_as_html(running_costs_per_asking_price)


##price by dayofweek_sold and weekofmonth_sold


#Heatmap
dayofweek_sold_by_weekofmonth_sold_median_heatmap <- my_heatmap(
  df = working_data,
  x = "dayofweek_sold",
  y = "weekofmonth_sold",
  z = "price",
  aggfxn = "median",
  title = "Median price is lowest Week 5 Saturday or Monday.
  Prices tend to be lowest in Week 5 and highest in week 2.")

dayofweek_sold_by_weekofmonth_sold_median_heatmap

save_plot_as_html(dayofweek_sold_by_weekofmonth_sold_median_heatmap)


##price by weekofmonth_sold and monthofquarter_sold


#Heatmap
weekofmonth_sold_by_monthofquarter_sold_median_heatmap <- my_heatmap(
  df = working_data,
  x = "weekofmonth_sold",
  y = "monthofquarter_sold",
  z = "price",
  aggfxn = "median",
  title = "Median price is lowest in Week 5, beginning of quarter. Prices tend
  to be higher week 2, lower week 1.")

weekofmonth_sold_by_monthofquarter_sold_median_heatmap

save_plot_as_html(weekofmonth_sold_by_monthofquarter_sold_median_heatmap)


##price by monthofquarter_sold and quarterofyear_sold


#Heatmap
monthofquarter_sold_by_quarterofyear_sold_median_heatmap <- my_heatmap(
  df = working_data,
  x = "monthofquarter_sold",
  y = "quarterofyear_sold",
  z = "price",
  aggfxn = "median",
  title = "Median price is highest third quarter, lowest in the first quarter.")

monthofquarter_sold_by_quarterofyear_sold_median_heatmap

save_plot_as_html(monthofquarter_sold_by_quarterofyear_sold_median_heatmap)


##price by dayofweek_sold, weekofmonth_sold, monthofyear_sold and year_sold


#Calendar heatmap
calendar_heatmap <- my_calendar_heatmap(
  df = working_data,
  day_col = "dayofweek_sold",
  week_col = "weekofmonth_sold",
  month_col = "monthofyear_sold",
  year_col = "year_sold",
  z = "price",
  title = "No very obvious trends.")

calendar_heatmap

save_plot_as_html(calendar_heatmap)


##price by rooms and floor


#Heatmap
rooms_by_floor_median_heatmap <- my_heatmap(
  df = working_data,
  x = "rooms",
  y = "floor",
  z = "price",
  aggfxn = "median",
  title = "Median price is lowest for 1.5 rooms on the 22 floor. Prices tend to
  be lowest for 1-room apartments, although there are some inexpensive at 3 rooms.")

rooms_by_floor_median_heatmap

save_plot_as_html(rooms_by_floor_median_heatmap)




#PAUSED HERE

#Should I engineer more date columns for combinations of date parts?
#yq
#ym
# moy
# moq
# 
#yw
# woy
# woq
# wom
#yd
# doy
# doq
# dom
# dow
#
#yqm
#yqw
#  woy
#  woq
#  wom
#yqd
#  doy
#  doq
#  dom
#  dow
#
#ymw
# moy
#  woy
#  woq
#  wom
# moq
#  woy
#  woq
#  wom
#ymd
# moy
#  doy
#  doq
#  dom
#  dow
# moq
#  doy
#  doq
#  dom
#  dow
#
#ywd
# woq
#  doy
#  doq
#  dom
# wom
#
#yqmw
#yqmd
#
#ymwd
#
#qm
#qw
#qd
#
#qmw
#qmd
#
#qwd
#
#qmwd
#
#mw
#md
#
#mwd
#
#wd

#Timelapse (continous vs continuous vs time):
#
#Building the model:
#I think I will use Elastic Net regression





df <- working_data
x <- "avgift"
y <- "price"
color <- "running_costs"
size <- "rooms"
time_frames <- "year_sold" #date_sold is computationally heavy
# ids <-
# group <- "age_when_sold"
# stroke <- "age_when_sold"
opaqueness <- "rooms"
shape <- "city"


# motion_plot <-
ggplotly(
  ggplot(df) +
    geom_point(
      aes(
        x = df[[x]],
        y = df[[y]],
        color = df[[color]],
        size = df[[size]],
        frame = df[[time_frames]],
        # id
        # group = df[[group]],
        # stroke = df[[stroke]],
        alpha = 1 - 1/df[[opaqueness]],
        shape = df[[shape]])
      ) +
    scale_color_viridis_c(option = "magma")
    )


         








#Considerations:
#After doing some apartment touring, maybe I can match certain föreningen to quality on a 1-5 pt scale
#
#Other critical data I am missing is # of bathrooms, although this will probably only be 1 for any apartment I am looking at anyway.
#
#Another piece of data not scraped from hemnet is if the apartment has washer/dryer hookups.
#


#Things to do:

#transform/normalize skewed data by transformations (cube root, log, box-cox)
#scale/standardize the data by minmax (x-min/(max-min)) or z-transformation (scale function)
#
#In doing elastic net, see if can program it to regress on all predictors as well as all two-way interactions (if not all possible interactions)
#
#Questions to answer:
#What is predicted selling price of an apartment?
#What is predicted selling price of same apartment in 1-5 years?
#Which apartments on the market now have the greatest potential increase in value over 1-5 years?
#Are there agencies/agents that consistently sell apartments above/below predicted value?
#Which day of the week sells highest/lowest?
#Which week of the month sells highest/lowest?
#Whichc month of the quarter sells highest/lowest?
#Which quarter of the year sells highest/lowest?
#Is there an effect of Swedish day off (holiday or pinch day) on sales price?
#Perhaps if my model is not great overall, it may still work in certain areas (Kungsholmen or Solna, for example). Check out doing subset analysis.
#
#How many of the datapoints I harvested are from apartments that have been bought and re-sold (same address and city) and how did their price development follow what I would have predicted?
#
#Can I get more information that would improve the model?:
#Was apartment sold before/after viewings (and is there an effect on price)?
#What was the date the the apartment was announced, and thereby the time on the market (date sold minus date announced)?
#How many bathrooms in the apartment?
#Time/distance to a T-bana