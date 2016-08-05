# import packages

library(dplyr)
library(googleVis)
library(sqldf)
library("RJSONIO")
library(leaflet)

# import data files and convert types

# wine agriculture data from nass.usda.gov
agro = read.csv("wine_agro_data.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
#str(agro)
#head(agro,1)
agro$tons_crushed = as.numeric(agro$tons_crushed)
agro$tons_purchased = as.numeric(agro$tons_purchased)
agro$year = as.character(agro$year)

# wine api data from winecom
json_data = fromJSON('wine_api_data.json')
A = lapply(json_data, function(lst){sapply(lst, function(x){x})})
api = as.data.frame(A)
rm(A)
rm(json_data)

# create summary rows for all reds, all whites, and all districts, and by varietal

summary_1 = agro %>% group_by(year, type, reporting_district) %>% summarise(total_tons_crushed = sum(tons_crushed, na.rm = TRUE), total_tons_purchased = sum(tons_purchased, na.rm = TRUE))
summary_2 = agro %>% group_by(year, type) %>% summarise(total_tons_crushed = sum(tons_crushed, na.rm = TRUE), total_tons_purchased = sum(tons_purchased, na.rm = TRUE))
summary_3 = agro %>% group_by(year, type, varietal) %>% summarise(total_tons_crushed = sum(tons_crushed, na.rm = TRUE), total_tons_purchased = sum(tons_purchased, na.rm = TRUE))

# rows for all whites by district, by year

white_bydist = sqldf('
select
agro.year,
agro.type,
NULL as varietal,
agro.reporting_district,
sum(agro.tons_crushed) as tons_crushed,
sum(agro.avg_brix_crushed * (agro.tons_crushed / summary_1.total_tons_crushed)) as avg_brix_crushed,
sum(agro.tons_purchased) as tons_purchased,
sum(agro.avg_brix_purchased * (agro.tons_purchased / summary_1.total_tons_purchased)) as avg_brix_purchased,
sum(agro.avg_dollars_per_ton * (agro.tons_purchased / summary_1.total_tons_purchaseD)) as avg_dollars_per_ton,
agro.district_name
from agro
left outer join summary_1 on (agro.year=summary_1.year and agro.type=summary_1.type and agro.reporting_district=summary_1.reporting_district)
where agro.type = "White"
group by agro.year, agro.type, agro.reporting_district, agro.district_name')

white_bydist$varietal = "All Grapes"

# rows for all reds by district, by year

red_bydist = sqldf('
select
agro.year,
agro.type,
NULL as varietal,
agro.reporting_district,
sum(agro.tons_crushed) as tons_crushed,
sum(agro.avg_brix_crushed * (agro.tons_crushed / summary_1.total_tons_crushed)) as avg_brix_crushed,
sum(agro.tons_purchased) as tons_purchased,
sum(agro.avg_brix_purchased * (agro.tons_purchased / summary_1.total_tons_purchased)) as avg_brix_purchased,
sum(agro.avg_dollars_per_ton * (agro.tons_purchased / summary_1.total_tons_purchaseD)) as avg_dollars_per_ton,
agro.district_name
from agro
left outer join summary_1 on (agro.year=summary_1.year and agro.type=summary_1.type and agro.reporting_district=summary_1.reporting_district)
where agro.type = "Red"
group by agro.year, agro.type, agro.reporting_district, agro.district_name')

red_bydist$varietal = "All Grapes"

# rows for all whites by year only

white_byyr = sqldf('
select
agro.year,
agro.type,
NULL as varietal,
NULL as reporting_district,
sum(agro.tons_crushed) as tons_crushed,
sum(agro.avg_brix_crushed * (agro.tons_crushed / summary_2.total_tons_crushed)) as avg_brix_crushed,
sum(agro.tons_purchased) as tons_purchased,
sum(agro.avg_brix_purchased * (agro.tons_purchased / summary_2.total_tons_purchased)) as avg_brix_purchased,
sum(agro.avg_dollars_per_ton * (agro.tons_purchased / summary_2.total_tons_purchaseD)) as avg_dollars_per_ton,
NULL as district_name
from agro
left outer join summary_2 on (agro.year=summary_2.year and agro.type=summary_2.type)
where agro.type = "White"
group by agro.year, agro.type')

white_byyr$varietal = "All Grapes"
white_byyr$reporting_district = -1
white_byyr$district_name = "All Districts"

# rows for all reds by year only

red_byyr = sqldf('
select
agro.year,
agro.type,
NULL as varietal,
NULL as reporting_district,
sum(agro.tons_crushed) as tons_crushed,
sum(agro.avg_brix_crushed * (agro.tons_crushed / summary_2.total_tons_crushed)) as avg_brix_crushed,
sum(agro.tons_purchased) as tons_purchased,
sum(agro.avg_brix_purchased * (agro.tons_purchased / summary_2.total_tons_purchased)) as avg_brix_purchased,
sum(agro.avg_dollars_per_ton * (agro.tons_purchased / summary_2.total_tons_purchaseD)) as avg_dollars_per_ton,
NULL as district_name
from agro
left outer join summary_2 on (agro.year=summary_2.year and agro.type=summary_2.type)
where agro.type = "Red"
group by agro.year, agro.type')

red_byyr$varietal = "All Grapes"
red_byyr$reporting_district = -1
red_byyr$district_name = "All Districts"




# rows for all districts by varietal

by_varietal = sqldf('
select
agro.year,
agro.type,
agro.varietal as varietal,
NULL as reporting_district,
sum(agro.tons_crushed) as tons_crushed,
sum(agro.avg_brix_crushed * (agro.tons_crushed / summary_3.total_tons_crushed)) as avg_brix_crushed,
sum(agro.tons_purchased) as tons_purchased,
sum(agro.avg_brix_purchased * (agro.tons_purchased / summary_3.total_tons_purchased)) as avg_brix_purchased,
sum(agro.avg_dollars_per_ton * (agro.tons_purchased / summary_3.total_tons_purchaseD)) as avg_dollars_per_ton,
NULL as district_name
from agro
left outer join summary_3 on (agro.year=summary_3.year and agro.type=summary_3.type and agro.varietal=summary_3.varietal)
group by agro.year, agro.type, agro.varietal')

by_varietal$reporting_district = -1
by_varietal$district_name = "All Districts"

agro_all = rbind(agro, white_bydist)
agro_all = rbind(agro_all, red_bydist)
agro_all = rbind(agro_all, white_byyr)
agro_all = rbind(agro_all, red_byyr)
agro_all = rbind(agro_all, by_varietal)

# ------------------ #

api_c = filter(api, region_area_name == 'California', vintage %in% c('2010', '2011', '2012', '2013', '2014', '2015'), geo_long != -360, prod_type == "Wine", geo_long > -124.4 & geo_long < -114.15 & geo_lat < 42 & geo_lat > 32.5)


api_c[api_c$rating_highest_score==0,23] = NA
# determine 90th percentile and add to rows, used to scale price later
api_c$ninety_pct_price = quantile(api_c$price_retail, prob = seq(0, 1, length = 11), type = 5)[10]

# determine value ratio and remove for rows with rating under 93
api_c$value_score = (api_c$rating_highest_score / api_c$price_retail)
api_c$value_score[api_c$rating_highest_score<93]=NA



# put min and max value ratios on every row to scale value later
api_c$min_value_score = min(api_c$value_score, na.rm = TRUE)
api_c$max_value_score = max(api_c$value_score, na.rm = TRUE)


# northern border = 42
# western border= -124.4
# eastern border = -114.15
# southern border = 32.5


# create summary tables by winery

api_summary1 = api_c %>% group_by(vintage, appellation_name, vineyard_name, geo_lat, geo_long) %>% summarise(avg_price = mean(price_retail), avg_score = mean(rating_highest_score, na.rm = TRUE), wine_count = n(), ninety_pct_price = mean(ninety_pct_price), min_value_score = mean(min_value_score), max_value_score = mean(max_value_score), avg_value_score = mean(value_score, na.rm = TRUE))
api_summary1a = api_summary1 %>% group_by(vintage, appellation_name) %>% summarise(max_wine_count = max(wine_count))

#write.csv(api_c, "test3.csv", row.names = TRUE)

# summary by winery, by wine type (red / white)

api_summary2 = api_c %>% group_by(vintage, appellation_name, vineyard_name, geo_lat, geo_long, wine_type_name) %>% summarise(avg_price = mean(price_retail), avg_score = mean(rating_highest_score, na.rm = TRUE), wine_count = n(), ninety_pct_price = mean(ninety_pct_price), min_value_score = mean(min_value_score), max_value_score = mean(max_value_score), avg_value_score = mean(value_score, na.rm = TRUE))
api_summary2a = api_summary2 %>% group_by(vintage, appellation_name, wine_type_name) %>% summarise(max_wine_count = max(wine_count))

# summary by winery, by wine type (red / white), by varietal

api_summary3 = api_c %>% group_by(vintage, appellation_name, vineyard_name, geo_lat, geo_long, wine_type_name, varietal_name) %>% summarise(avg_price = mean(price_retail), avg_score = mean(rating_highest_score, na.rm = TRUE), wine_count = n(), ninety_pct_price = mean(ninety_pct_price), min_value_score = mean(min_value_score), max_value_score = mean(max_value_score), avg_value_score = mean(value_score, na.rm = TRUE))
api_summary3a = api_summary3 %>% group_by(vintage, appellation_name, wine_type_name, varietal_name) %>% summarise(max_wine_count = max(wine_count))

api_summary4 = api_c %>% group_by(vintage, appellation_name, vineyard_name, geo_lat, geo_long, varietal_name) %>% summarise(avg_price = mean(price_retail), avg_score = mean(rating_highest_score, na.rm = TRUE), wine_count = n(), ninety_pct_price = mean(ninety_pct_price), min_value_score = mean(min_value_score), max_value_score = mean(max_value_score), avg_value_score = mean(value_score, na.rm = TRUE))
api_summary4a = api_summary4 %>% group_by(vintage, appellation_name, varietal_name) %>% summarise(max_wine_count = max(wine_count))


api_by1 = sqldf('
select
api_summary1.vintage,
api_summary1.appellation_name,
api_summary1.geo_lat,
api_summary1.geo_long,
api_summary1.vineyard_name,
NULL as varietal,
NULL as product_name,
NULL as wine_type_name,
avg(api_summary1.avg_price) as avg_price,
avg(api_summary1.avg_score) as avg_score,
sum(api_summary1.wine_count) as wine_count,
avg(api_summary1.avg_value_score) as avg_value_score,
min(cast((cast(api_summary1.avg_price as FLOAT) / cast(api_summary1.ninety_pct_price as FLOAT)) as FLOAT), 1) as price_scale,
cast(ifnull((cast(api_summary1.avg_value_score as FLOAT) - cast(api_summary1.min_value_score as FLOAT)) / (cast(api_summary1.max_value_score as FLOAT) - cast(api_summary1.min_value_score as FLOAT)),0) as FLOAT) as score_scale,
cast(api_summary1.wine_count as float) / cast(api_summary1a.max_wine_count as float) as volume_scale
from api_summary1 
left outer join api_summary1a on (api_summary1.vintage = api_summary1a.vintage and api_summary1.appellation_name = api_summary1a.appellation_name)
group by api_summary1.vintage, api_summary1.appellation_name, api_summary1.geo_lat, api_summary1.geo_long, api_summary1.vineyard_name')

api_by1$varietal = "All Grapes"
api_by1$product_name = "All Products"
api_by1$wine_type_name = "All Types"


api_by2 = sqldf('
select
api_summary2.vintage,
api_summary2.appellation_name,
api_summary2.geo_lat,
api_summary2.geo_long,
api_summary2.vineyard_name,
NULL as varietal,
NULL as product_name,
api_summary2.wine_type_name,
avg(api_summary2.avg_price) as avg_price,
avg(api_summary2.avg_score) as avg_score,
sum(api_summary2.wine_count) as wine_count,
avg(api_summary2.avg_value_score) as avg_value_score,
min(cast((cast(api_summary2.avg_price as FLOAT) / cast(api_summary2.ninety_pct_price as FLOAT)) as FLOAT), 1) as price_scale,
cast(ifnull((cast(api_summary2.avg_value_score as FLOAT) - cast(api_summary2.min_value_score as FLOAT)) / (cast(api_summary2.max_value_score as FLOAT) - cast(api_summary2.min_value_score as FLOAT)),0) as FLOAT) as score_scale,
cast(api_summary2.wine_count as float) / cast(api_summary2a.max_wine_count as float) as volume_scale
from api_summary2
left outer join api_summary2a on (api_summary2.vintage = api_summary2a.vintage and api_summary2.appellation_name = api_summary2a.appellation_name and api_summary2.wine_type_name = api_summary2a.wine_type_name)
group by api_summary2.vintage, api_summary2.appellation_name, api_summary2.geo_lat, api_summary2.geo_long, api_summary2.vineyard_name, api_summary2.wine_type_name')

api_by2$varietal = "All Grapes"
api_by2$product_name = "All Products"



api_by3 = sqldf('
select
api_summary3.vintage,
api_summary3.appellation_name,
api_summary3.geo_lat,
api_summary3.geo_long,
api_summary3.vineyard_name,
api_summary3.varietal_name as varietal,
NULL as product_name,
api_summary3.wine_type_name,
avg(api_summary3.avg_price) as avg_price,
avg(api_summary3.avg_score) as avg_score,
sum(api_summary3.wine_count) as wine_count,
avg(api_summary3.avg_value_score) as avg_value_score,
min(cast((cast(api_summary3.avg_price as FLOAT) / cast(api_summary3.ninety_pct_price as FLOAT)) as FLOAT), 1) as price_scale,
cast(ifnull((cast(api_summary3.avg_value_score as FLOAT) - cast(api_summary3.min_value_score as FLOAT)) / (cast(api_summary3.max_value_score as FLOAT) - cast(api_summary3.min_value_score as FLOAT)),0) as FLOAT) as score_scale,
cast(api_summary3.wine_count as float) / cast(api_summary3a.max_wine_count as float) as volume_scale
from api_summary3
left outer join api_summary3a on (api_summary3.vintage = api_summary3a.vintage and 
  api_summary3.appellation_name = api_summary3a.appellation_name and 
  api_summary3.wine_type_name = api_summary3a.wine_type_name and api_summary3.varietal_name = api_summary3a.varietal_name)
group by api_summary3.vintage, api_summary3.appellation_name, api_summary3.geo_lat, api_summary3.geo_long, api_summary3.vineyard_name, api_summary3.wine_type_name, api_summary3.varietal_name')

api_by3$product_name = "All Products"


api_by4 = sqldf('
select
api_summary4.vintage,
api_summary4.appellation_name,
api_summary4.geo_lat,
api_summary4.geo_long,
api_summary4.vineyard_name,
api_summary4.varietal_name as varietal,
NULL as product_name,
NULL as wine_type_name,
avg(api_summary4.avg_price) as avg_price,
avg(api_summary4.avg_score) as avg_score,
sum(api_summary4.wine_count) as wine_count,
avg(api_summary4.avg_value_score) as avg_value_score,
min(cast((cast(api_summary4.avg_price as FLOAT) / cast(api_summary4.ninety_pct_price as FLOAT)) as FLOAT), 1) as price_scale,
cast(ifnull((cast(api_summary4.avg_value_score as FLOAT) - cast(api_summary4.min_value_score as FLOAT)) / (cast(api_summary4.max_value_score as FLOAT) - cast(api_summary4.min_value_score as FLOAT)),0) as FLOAT) as score_scale,
cast(api_summary4.wine_count as float) / cast(api_summary4a.max_wine_count as float) as volume_scale
from api_summary4
left outer join api_summary4a on (api_summary4.vintage = api_summary4a.vintage and 
  api_summary4.appellation_name = api_summary4a.appellation_name and 
  api_summary4.varietal_name = api_summary4a.varietal_name)
group by api_summary4.vintage, api_summary4.appellation_name, api_summary4.geo_lat, api_summary4.geo_long, api_summary4.vineyard_name, api_summary4.varietal_name')

api_by4$product_name = "All Products"
api_by4$wine_type_name = 'All Types'
head(api_by4,5)

api_all = rbind(api_by1, api_by2)
api_all = rbind(api_all, api_by3)
api_all = rbind(api_all, api_by4)

api_all$tooltip =
  paste(sep = '<br/>',
       paste(paste('<b>',api_all$vineyard_name),'</b>'),
       paste('Appellation:',api_all$appellation_name),
       paste('Average Price:',sprintf("$ %3.2f", api_all$avg_price)),
       paste('Average Score:',round(api_all$avg_score, digits = 1)),
       paste('Average Value Rating:',round(api_all$avg_value_score, digits = 1)),
       paste('Number of Wines:',api_all$wine_count)
      )
  
# ------------------------ # 


# add mapping columns to agro data in order to pass filter from api data

# add appellation names; since there are many to many instances, wlil need to filter several in map object (Carneros : Napa, North Coast and Russian River : Sonoma, Santa Maria Valley: Central Coast)

agro_all$api_appellation[agro_all$district_name %in% c("Mendocino", "Lake", "Solano", "San Bernadino", "Riverside")] = "Other California"
agro_all$api_appellation[agro_all$district_name %in% c("Yolo", "Placer", "Sacramento", "San Joaquin", "Fresno", "Yolo_Sacramento")] = "Sierra Foothills"
agro_all$api_appellation[agro_all$district_name %in% c("Alameda_Santa Clara_Santa Cruz", "Kern")] = "Santa Cruz Mountains"
agro_all$api_appellation[agro_all$district_name %in% c("Monterey", "Santa Barbara")] = "Central Coast"
agro_all$api_appellation[agro_all$district_name %in% c("Sonoma")] = "Sonoma County"
agro_all$api_appellation[agro_all$district_name %in% c("Napa")] = "Napa Valley"
agro_all$api_appellation[agro_all$district_name %in% c("All Districts")] = "All"

# add wine type

agro_all$api_wine_type_name[agro_all$type == "Red"] = "Red Wines"
agro_all$api_wine_type_name[agro_all$type == "White"] = "White Wines"

# add varietal names

varietal_xwalk = read.csv("agro_api_varietal_xwalk.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
agro_all = merge(agro_all, varietal_xwalk, by.x = c("varietal"), by.y = c("agro_varietal"))


#linetest = gvisLineChart(filter(agro_all, type == "White", varietal == "Chardonnay", reporting_district == 5), "year", c("avg_dollars_per_ton"))
#plot(linetest)

# write.csv(unique(select(agro_all, varietal, type)), "agro_varietal_lookup.csv", row.names = TRUE)
# write.csv(unique(api_all$varietal), "api_varietal.csv", row.names = TRUE)
# write.csv(unique(agro_all$district_name), "agro_district.csv", row.names = TRUE)
# write.csv(unique(api_all$appellation_name), "api_appellation.csv", row.names = TRUE)

# now create new agro grouping tables incorporating api columns.  Be careful to exclude existing summary rows (else will be double counted)

#head(agro_all,1)

c_summary1 = agro_all %>% filter(varietal != "All Grapes" & reporting_district != -1) %>% group_by(year) %>% summarise(total_tons_crushed = sum(tons_crushed), total_tons_purchased = sum(tons_purchased))
c_summary2 = agro_all %>% filter(varietal != "All Grapes" & reporting_district != -1) %>% group_by(year, api_appellation) %>% summarise(total_tons_crushed = sum(tons_crushed), total_tons_purchased = sum(tons_purchased))
c_summary3 = agro_all %>% filter(varietal != "All Grapes" & reporting_district != -1) %>% group_by(year, api_appellation, api_wine_type_name) %>% summarise(total_tons_crushed = sum(tons_crushed), total_tons_purchased = sum(tons_purchased))
c_summary4 = agro_all %>% filter(varietal != "All Grapes" & reporting_district != -1) %>% group_by(year, api_appellation, api_wine_type_name, api_varietal) %>% summarise(total_tons_crushed = sum(tons_crushed), total_tons_purchased = sum(tons_purchased))
c_summary5 = agro_all %>% filter(varietal != "All Grapes" & reporting_district != -1) %>% group_by(year, api_wine_type_name, api_varietal) %>% summarise(total_tons_crushed = sum(tons_crushed), total_tons_purchased = sum(tons_purchased))
c_summary6 = agro_all %>% filter(varietal != "All Grapes" & reporting_district != -1) %>% group_by(year, api_varietal) %>% summarise(total_tons_crushed = sum(tons_crushed), total_tons_purchased = sum(tons_purchased))
c_summary7 = agro_all %>% filter(varietal != "All Grapes" & reporting_district != -1) %>% group_by(year, api_appellation, api_varietal) %>% summarise(total_tons_crushed = sum(tons_crushed), total_tons_purchased = sum(tons_purchased))
c_summary8 = agro_all %>% filter(varietal != "All Grapes" & reporting_district != -1) %>% group_by(year, api_wine_type_name) %>% summarise(total_tons_crushed = sum(tons_crushed), total_tons_purchased = sum(tons_purchased))

# correct total tons crushed = 22795763.3
#sum(c_summary3$total_tons_crushed)

# rows for all appellations, all wine types, and all varietals by year

c_byyear = sqldf('
                 select
                 NULL as varietal,
                 agro_all.year,
                 NULL as type,
                 NULL as reporting_district,
                 sum(agro_all.tons_crushed) as tons_crushed,
                 sum(agro_all.avg_brix_crushed * (agro_all.tons_crushed / c_summary1.total_tons_crushed)) as avg_brix_crushed,
                 sum(agro_all.tons_purchased) as tons_purchased,
                 sum(agro_all.avg_brix_purchased * (agro_all.tons_purchased / c_summary1.total_tons_purchased)) as avg_brix_purchased,
                 sum(agro_all.avg_dollars_per_ton * (agro_all.tons_purchased / c_summary1.total_tons_purchaseD)) as avg_dollars_per_ton,
                 NULL as district_name,
                 NULL as api_appellation,
                 NULL as api_varietal,
                 NULL as api_wine_type_name
                 from agro_all
                 left outer join c_summary1 on agro_all.year=c_summary1.year
                 where (agro_all.varietal <> "All Grapes" and agro_all.reporting_district <> -1)
                 group by agro_all.year')

c_byyear$api_appellation = "All"
c_byyear$api_wine_type_name = "All Types"
c_byyear$api_varietal = "All Grapes"

#sum(c_byyear$tons_crushed)

# rows for all wine types, and all varietals by year, by appellation

c_byappellation = sqldf('
                        select
                        NULL as varietal,
                        agro_all.year,
                        NULL as type,
                        NULL as reporting_district,
                        sum(agro_all.tons_crushed) as tons_crushed,
                        sum(agro_all.avg_brix_crushed * (agro_all.tons_crushed / c_summary2.total_tons_crushed)) as avg_brix_crushed,
                        sum(agro_all.tons_purchased) as tons_purchased,
                        sum(agro_all.avg_brix_purchased * (agro_all.tons_purchased / c_summary2.total_tons_purchased)) as avg_brix_purchased,
                        sum(agro_all.avg_dollars_per_ton * (agro_all.tons_purchased / c_summary2.total_tons_purchaseD)) as avg_dollars_per_ton,
                        NULL as district_name,
                        agro_all.api_appellation,
                        NULL as api_varietal,
                        NULL as api_wine_type_name
                        from agro_all
                        left outer join c_summary2 on (agro_all.year=c_summary2.year and agro_all.api_appellation = c_summary2.api_appellation)
                        where (agro_all.varietal <> "All Grapes" and agro_all.reporting_district <> -1)
                        group by agro_all.year, agro_all.api_appellation')

#sum(c_byappellation$tons_crushed)

c_byappellation$api_wine_type_name = "All Types"
c_byappellation$api_varietal = "All Grapes"

# rows for all varietals by year, by appellation, by wine type

c_bywinetype = sqldf('
                     select
                     NULL as varietal,
                     agro_all.year,
                     NULL as type,
                     NULL as reporting_district,
                     sum(agro_all.tons_crushed) as tons_crushed,
                     sum(agro_all.avg_brix_crushed * (agro_all.tons_crushed / c_summary3.total_tons_crushed)) as avg_brix_crushed,
                     sum(agro_all.tons_purchased) as tons_purchased,
                     sum(agro_all.avg_brix_purchased * (agro_all.tons_purchased / c_summary3.total_tons_purchased)) as avg_brix_purchased,
                     sum(agro_all.avg_dollars_per_ton * (agro_all.tons_purchased / c_summary3.total_tons_purchaseD)) as avg_dollars_per_ton,
                     NULL as district_name,
                     agro_all.api_appellation,
                     NULL as api_varietal,
                     agro_all.api_wine_type_name
                     from agro_all
                     left outer join c_summary3 on (agro_all.year=c_summary3.year and agro_all.api_appellation = c_summary3.api_appellation and agro_all.api_wine_type_name = c_summary3.api_wine_type_name)
                     where (agro_all.varietal <> "All Grapes" and agro_all.reporting_district <> -1)
                     group by agro_all.year, agro_all.api_appellation, agro_all.api_wine_type_name')

c_bywinetype$api_varietal = "All Grapes"

#sum(c_bywinetype$tons_crushed)

# rows for all by year, by appellation, by wine type, by varietal

c_byvarietal = sqldf('
                     select
                     NULL as varietal,
                     agro_all.year,
                     NULL as type,
                     NULL as reporting_district,
                     sum(agro_all.tons_crushed) as tons_crushed,
                     sum(agro_all.avg_brix_crushed * (agro_all.tons_crushed / c_summary4.total_tons_crushed)) as avg_brix_crushed,
                     sum(agro_all.tons_purchased) as tons_purchased,
                     sum(agro_all.avg_brix_purchased * (agro_all.tons_purchased / c_summary4.total_tons_purchased)) as avg_brix_purchased,
                     sum(agro_all.avg_dollars_per_ton * (agro_all.tons_purchased / c_summary4.total_tons_purchaseD)) as avg_dollars_per_ton,
                     NULL as district_name,
                     agro_all.api_appellation,
                     agro_all.api_varietal,
                     agro_all.api_wine_type_name
                     from agro_all
                     left outer join c_summary4 on (agro_all.year=c_summary4.year and agro_all.api_appellation = c_summary4.api_appellation and agro_all.api_wine_type_name = c_summary4.api_wine_type_name and agro_all.api_varietal = c_summary4.api_varietal)
                     where (agro_all.varietal <> "All Grapes" and agro_all.reporting_district <> -1)
                     group by agro_all.year, agro_all.api_appellation, agro_all.api_wine_type_name, agro_all.api_varietal')

#sum(c_byvarietal$tons_crushed)

# rows for all appellations by year, by wine type, by varietal

c_bytypevarietal = sqldf('
                         select
                         NULL as varietal,
                         agro_all.year,
                         NULL as type,
                         NULL as reporting_district,
                         sum(agro_all.tons_crushed) as tons_crushed,
                         sum(agro_all.avg_brix_crushed * (agro_all.tons_crushed / c_summary5.total_tons_crushed)) as avg_brix_crushed,
                         sum(agro_all.tons_purchased) as tons_purchased,
                         sum(agro_all.avg_brix_purchased * (agro_all.tons_purchased / c_summary5.total_tons_purchased)) as avg_brix_purchased,
                         sum(agro_all.avg_dollars_per_ton * (agro_all.tons_purchased / c_summary5.total_tons_purchaseD)) as avg_dollars_per_ton,
                         NULL as district_name,
                         NULL as api_appellation,
                         agro_all.api_varietal,
                         agro_all.api_wine_type_name
                         from agro_all
                         left outer join c_summary5 on (agro_all.year=c_summary5.year and agro_all.api_wine_type_name = c_summary5.api_wine_type_name and agro_all.api_varietal = c_summary5.api_varietal)
                         where (agro_all.varietal <> "All Grapes" and agro_all.reporting_district <> -1)
                         group by agro_all.year, agro_all.api_wine_type_name, agro_all.api_varietal')

c_bytypevarietal$api_appellation = "All"

#sum(c_byvarietal$tons_crushed)

# rows for all appellations and wine types by year, by varietal

c_byvarietalonly = sqldf('
                         select
                         NULL as varietal,
                         agro_all.year,
                         NULL as type,
                         NULL as reporting_district,
                         sum(agro_all.tons_crushed) as tons_crushed,
                         sum(agro_all.avg_brix_crushed * (agro_all.tons_crushed / c_summary6.total_tons_crushed)) as avg_brix_crushed,
                         sum(agro_all.tons_purchased) as tons_purchased,
                         sum(agro_all.avg_brix_purchased * (agro_all.tons_purchased / c_summary6.total_tons_purchased)) as avg_brix_purchased,
                         sum(agro_all.avg_dollars_per_ton * (agro_all.tons_purchased / c_summary6.total_tons_purchaseD)) as avg_dollars_per_ton,
                         NULL as district_name,
                         NULL as api_appellation,
                         agro_all.api_varietal,
                         NULL as api_wine_type_name
                         from agro_all
                         left outer join c_summary6 on (agro_all.year=c_summary6.year and agro_all.api_varietal = c_summary6.api_varietal)
                         where (agro_all.varietal <> "All Grapes" and agro_all.reporting_district <> -1)
                         group by agro_all.year, agro_all.api_varietal')

c_byvarietalonly$api_appellation = "All"
c_byvarietalonly$api_wine_type_name = "All Types"

#sum(c_byvarietalonly$tons_crushed)

# rows for all wine types by year, by appellation, by varietal

c_byappvarietal = sqldf('
                        select
                        NULL as varietal,
                        agro_all.year,
                        NULL as type,
                        NULL as reporting_district,
                        sum(agro_all.tons_crushed) as tons_crushed,
                        sum(agro_all.avg_brix_crushed * (agro_all.tons_crushed / c_summary7.total_tons_crushed)) as avg_brix_crushed,
                        sum(agro_all.tons_purchased) as tons_purchased,
                        sum(agro_all.avg_brix_purchased * (agro_all.tons_purchased / c_summary7.total_tons_purchased)) as avg_brix_purchased,
                        sum(agro_all.avg_dollars_per_ton * (agro_all.tons_purchased / c_summary7.total_tons_purchaseD)) as avg_dollars_per_ton,
                        NULL as district_name,
                        agro_all.api_appellation,
                        agro_all.api_varietal,
                        NULL as api_wine_type_name
                        from agro_all
                        left outer join c_summary7 on (agro_all.year=c_summary7.year and agro_all.api_appellation = c_summary7.api_appellation and agro_all.api_varietal = c_summary7.api_varietal)
                        where (agro_all.varietal <> "All Grapes" and agro_all.reporting_district <> -1)
                        group by agro_all.year, agro_all.api_appellation, agro_all.api_varietal')

c_byappvarietal$api_wine_type_name = "All Types"

#sum(c_byvarietal$tons_crushed)

# rows for all varietals and appellations, by year, by wine type

c_bytype = sqldf('
                        select
                        NULL as varietal,
                        agro_all.year,
                        NULL as type,
                        NULL as reporting_district,
                        sum(agro_all.tons_crushed) as tons_crushed,
                        sum(agro_all.avg_brix_crushed * (agro_all.tons_crushed / c_summary8.total_tons_crushed)) as avg_brix_crushed,
                        sum(agro_all.tons_purchased) as tons_purchased,
                        sum(agro_all.avg_brix_purchased * (agro_all.tons_purchased / c_summary8.total_tons_purchased)) as avg_brix_purchased,
                        sum(agro_all.avg_dollars_per_ton * (agro_all.tons_purchased / c_summary8.total_tons_purchaseD)) as avg_dollars_per_ton,
                        NULL as district_name,
                        NULL as api_appellation,
                        NULL as api_varietal,
                        agro_all.api_wine_type_name
                        from agro_all
                        left outer join c_summary8 on (agro_all.year=c_summary8.year and agro_all.api_wine_type_name = c_summary8.api_wine_type_name)
                        where (agro_all.varietal <> "All Grapes" and agro_all.reporting_district <> -1)
                        group by agro_all.year, agro_all.api_wine_type_name')

#sum(c_bytype$tons_crushed)

c_bytype$api_appellation = "All"
c_bytype$api_varietal = "All Grapes"


c_agro_all = rbind(c_byyear, c_byappellation)
c_agro_all = rbind(c_agro_all, c_bywinetype)
c_agro_all = rbind(c_agro_all, c_byvarietal)
c_agro_all = rbind(c_agro_all, c_bytypevarietal)
c_agro_all = rbind(c_agro_all, c_byvarietalonly)
c_agro_all = rbind(c_agro_all, c_byappvarietal)
c_agro_all = rbind(c_agro_all, c_bytype)


write.csv(api_all, "api_all.csv", row.names = FALSE)
write.csv(c_agro_all, "c_agro_all.csv", row.names = FALSE)

#rm(list = ls())
