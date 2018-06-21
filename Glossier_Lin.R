rm(list = ls())

#READ DATA
borough_names = read.table(file = 'borough_names.tsv', sep = '\t', header = TRUE)
cuisine_names = read.table(file = 'cuisine_names.tsv', sep = '\t', header = TRUE)
res_attributes = read.table(file = 'restaurant_attributes.tsv', sep = '\t', header = TRUE, fill = TRUE)
res_names = read.table(file = 'restaurant_names.tsv', sep = '\t', header = TRUE)
res_violations = read.table(file = 'restaurant_violations.tsv', sep = '\t', header = TRUE)
violation_names = read.table(file = 'violation_names.tsv', sep = '\t', header = TRUE)

#PACKAGES
my_packages = c('sqldf', 'RSQLite', 'RSQLite', 'dplyr', 'lubridate', 'ggplot2', 'RCurl', 'ggmap', 'tidyr')
install.packages(my_packages)
libraries(my_packages)



############## JOIN ALL THE TABLES ###################
#first join the violation info
str(res_attributes) #10,969 unique restaurant in the data
str(res_violations) #24,800 unique restaurant 
res_vio = sqldf('SELECT * FROM res_attributes LEFT JOIN res_violations USING(restaurant_id)')
names(violation_names)[names(violation_names) == 'id'] = 'violation_id'
res_vio_a = sqldf('SELECT * FROM res_vio LEFT JOIN violation_names USING(violation_id)')

#join borough
names(borough_names)[names(borough_names) == 'id'] = 'borough_id'
res_vio_b = sqldf('SELECT * FROM res_vio_a LEFT JOIN borough_names USING(borough_id)')

#join cuisine type
names(cuisine_names)[names(cuisine_names) == 'id'] = 'cuisine_id'
res_vio_c = sqldf('SELECT * FROM res_vio_b LEFT JOIN cuisine_names USING(cuisine_id)')

#join res_name 
names(res_names)[names(res_names) == 'id'] = 'restaurant_id'
res_vio_d = sqldf('SELECT * FROM res_vio_c LEFT JOIN res_names USING(restaurant_id)')

#convert to factors 
fac_name = c('cuisine_id', 'violation_id', 'critical_flag', 'score', 'grade', 'violation_description',
             'borough_name', 'cuisine_description', 'restaurant_name')
res_vio_d[, fac_name] = lapply(res_vio_d[, fac_name], factor)


#remove irrelavant columns 
drop = c('borough_id', 'cuisine_id')
res_vio_e = res_vio_d[,!(names(res_vio_d) %in% drop)]

#join geolocation
zip = read.csv('zip_codes_states.csv', header = T)
names(zip)[names(zip) == 'zip_code'] = 'zipcode'
res_vio_f = sqldf('SELECT * FROM res_vio_e LEFT JOIN zip USING(zipcode)')
drop2 = c('city', 'state', 'county', 'record_date', 'building', 'street')
res_vio_g = res_vio_f[,!(names(res_vio_f) %in% drop2)]
res_vio_h = res_vio_g %>% mutate_if(is.character,as.factor)


######### MORE ANALYSIS ON THE CLEAN DATA ###########
summary(res_vio_h)
str(res_vio_h)
unique(res_vio_h$restaurant_id) #10969 unique
sum(is.na(res_vio_h)) #83570 missing values; the missing information is irrelevant to the analysis; dont delete missings.


## Critical_flag level
res_vio_h  = res_vio_h %>% drop_na(critical_flag)
pie(table(res_vio_h$critical_flag), main= 'Critical_flag')
pie(table(res_vio_h$grade), main = 'grade')
unique(res_vio_h$violation_id)


#dates
str(res_vio_h)
res_vio_h$grade_date = as.Date(res_vio_h$grade_date, "%m/%d/%Y")
res_vio_h$year = year(res_vio_h$grade_date)

num_violation_each_year = res_vio_h %>% group_by(year, borough_name) %>% count(restaurant_id)
nums_violation_each_year = num_violation_each_year %>% group_by(year, borough_name) %>%
  aggregate(n ~ year + borough_name, data = ., sum)

ggplot(data = nums_violation_each_year, aes(year,n)) + 
  geom_bar(stat ='identity', fill = "#FF6666") + 
  geom_text(aes(label = n), vjust=0) +
  facet_wrap(~borough_name) +
  ggtitle('Count of Violations by Year & Borough')

ggplot(data = nums_violation_each_year, aes(borough_name,n)) + 
  geom_bar(stat ='identity', aes(fill = year)) + 
  ggtitle('Count of Violations by Year & Borough')
#64 different types of violation, but only 41 violation discription in 
#the violation_name file; therefore, a lot of the violation don't have descriptions. 
#num 44 is the most frequently occuring violation. let's look at each food categori and see its violation 

#distribution of restaurants
num_res_in_each_borough = res_vio_h %>% group_by(restaurant_id, borough_name) %>% count(restaurant_id)
numres_in_each_borough = num_res_in_each_borough %>% group_by(borough_name) %>% count(borough_name)

ggplot(data = numres_in_each_borough, aes(borough_name,nn)) + 
  geom_bar(stat ='identity', fill = '#56B4E9') + 
  geom_text(aes(label = nn), vjust=0) +
  ggtitle('Count of Distinct Restaurant by Borough')

#grades
grade_by_res = res_vio_h %>% filter(grade != 'Not Yet Graded') %>%
  group_by(restaurant_id, grade) %>% count(restaurant_id)
grades_by_res = grade_by_res %>% group_by(grade) %>% count(grade)
ggplot(data = grades_by_res, aes(grade,nn)) + 
  geom_bar(stat ='identity', fill = '#E69F00') + 
  geom_text(aes(label = nn), vjust=0) +
  ggtitle('Count of Distinct Restaurant by Grade')


#scores
summary(res_vio_h$score)

############################## INSPECTOR PERSPECTIVE ##############################
######### Evaluation Benchmark, Which one #############


#14 16
rats = res_vio_h[which(res_vio_h$violation_id == 14),]
flies = res_vio_h[which(res_vio_h$violation_id == 16),]
unacceptable_material = res_vio_h[which(res_vio_h$violation_id == 44),]

ggplot(rats, aes(x = grade, y = score, fill = critical_flag)) +
  geom_boxplot() + ggtitle('Rats Issue - ID14')
ggplot(flies, aes(x = grade, y = score, fill = critical_flag)) +
  geom_boxplot() + ggtitle('Flies Issue - ID16')
ggplot(unacceptable_material, aes(x = grade, y = score, fill = critical_flag)) +
  geom_boxplot(color = 'blue') + ggtitle('Unacceptable Material - ID44')


#findings: though violation id and its associated critical_flafis diffwerent, the restaurant always receive the same grade and score on the same day
#i see correlation between violation_id(and its type) with being critical or not. ex. violationid 14 is 
#unacceptiple. those are all deemed as critical issues, yet restaurants receive different gradings. therefore, i would 
#disregard grading.

#boxplot score by grades
res_vio_h %>% filter(grade != 'Not Yet Graded' & critical_flag != 'Not Applicable') %>% ggplot(., aes(x = grade, y = score, fill = critical_flag)) + 
  geom_boxplot() + 
  ggtitle('Violation Evaluation by Year')

# grades and critical_flag
res_vio_h %>% filter(grade != 'Not Yet Graded') %>% ggplot(., aes(grade)) +
  geom_bar(aes(fill = critical_flag)) + facet_wrap(~borough_name) +
  ggtitle('Grade and Critical-flag by Borough')

#how many critical violation does each restaurant have 
res_vio_total_count = res_vio_h %>% filter(critical_flag != 'Not Applicable') %>% group_by(restaurant_id, borough_name) %>% count(critical_flag) %>% drop_na(critical_flag) 

ggplot(res_vio_total_count, aes(x = n)) +
  geom_bar(aes(fill = critical_flag), stat = 'count') +
  facet_wrap(~borough_name) +
  ggtitle('Critical Level Count by Borough')

res_vio_count_bygrade = res_vio_h %>% group_by(restaurant_id, borough_name) %>% count(grade) %>% drop_na(grade)
ggplot(res_vio_count_bygrade, aes(x = n)) +
  geom_bar(aes(fill = grade), stat = 'count') +
  facet_wrap(~borough_name)

################## CUSTOMER PERSPECTIVE #######################

# come me up with the top 5 restaurats in each borough with the most critical violation
new = res_vio_h %>% group_by(restaurant_id) %>% count(critical_flag) %>% drop_na(critical_flag)
by_res_name = res_vio_h %>% group_by(restaurant_name) %>% count(critical_flag, sort = TRUE) %>% drop_na(critical_flag)
by_res_name_2 = by_res_name %>% filter(critical_flag == 'Critical') %>% drop_na(restaurant_name) 


# from new2 we can tell that restaurants with the highest violations are mostly chain restaurants such as 
#starbukes, subways, etc
# NEED TO FIGURE OUT THEIR GRADES ASLO
by_cuisine_type = res_vio_h %>% group_by(cuisine_description, borough_name) %>% filter(critical_flag == 'Critical') %>% count(critical_flag) %>% drop_na(critical_flag)
#what type of resturants recived most critical violation violation 
#only choose american, chinese, Italian, Pizza, Latin (Cuban, Dominican, Puerto Rican, South & Central American)

###### GOESPACIAL ANALYSIS ###############

nyc_base <- ggmap::get_map("New York City", zoom = 10)
stats2 = aggregate(res_vio_h$critical_flag ~res_vio_h$zipcode, FUN = length)
names(stats2)[names(stats2) == 'res_vio_h$zipcode'] = 'zipcode'
location = sqldf('SELECT * FROM stats2 LEFT JOIN zip USING(zipcode)')
names(location)[names(location) == 'res_vio_h$critical_flag'] <- 'Total_Num_of_Critical_flag'

ggmap(nyc_base) + geom_point(aes(x=longitude, y=latitude, show_guide = TRUE, 
                                 colour=`Total Num of Critical_flag`), 
                             data=location,alpha = 1, na.rm = T) + geom_jitter() +
  scale_colour_gradientn(colours = terrain.colors(10)) + ggtitle('Violation Map')

#########text mining#
names(res_vio_h)
manhattan_res = res_vio_h %>% filter (borough_name == "MANHATTAN") %>% select(violation_description) %>% drop_na()
library(tm)
install.packages('wordcloud')
library(wordcloud)
mycorpus1 = VCorpus( VectorSource(manhattan_res))
mycorpus2 <- tm_map(mycorpus1, content_transformer(tolower))
lapply(mycorpus2[4:5], as.character)
stopwords("english")
mycorpus3<- tm_map(mycorpus2, removeWords, stopwords("english")) #this steps takes long

mycorpus4 <- tm_map(mycorpus3, removePunctuation)
mycorpus5 <- tm_map(mycorpus4, removeNumbers)
mycorpus6 <- tm_map(mycorpus5, stemDocument, lazy = TRUE)   
wordcloud(mycorpus6)


#staten_island
staten_res = res_vio_h %>% filter (borough_name == "STATEN ISLAND") %>% select(violation_description) %>% drop_na()
stmycorpus1 = VCorpus( VectorSource(staten_res))
stmycorpus2 <- tm_map(stmycorpus1, content_transformer(tolower))
stopwords("english")
stmycorpus3<- tm_map(stmycorpus2, removeWords, stopwords("english")) #this steps takes long

stmycorpus4 <- tm_map(stmycorpus3, removePunctuation)
stmycorpus5 <- tm_map(stmycorpus4, removeNumbers)
stmycorpus6 <- tm_map(stmycorpus5, stemDocument, lazy = TRUE)   
wordcloud(stmycorpus6)


american_food = res_vio_h %>% filter (cuisine_description == "American") %>% select(violation_description) %>% drop_na()
ammycorpus1 = VCorpus( VectorSource(american_food))
ammycorpus2 <- tm_map(ammycorpus1, content_transformer(tolower))
ammycorpus3<- tm_map(ammycorpus2, removeWords, stopwords("english")) #this steps takes long

ammycorpus4 <- tm_map(ammycorpus3, removePunctuation)
ammycorpus5 <- tm_map(ammycorpus4, removeNumbers)
ammycorpus6 <- tm_map(ammycorpus5, stemDocument, lazy = TRUE)   
wordcloud(mycorpus6)




# p = ggmap(nyc_base) + geom_jitter(aes(x=longitude, y=latitude, show_guide = TRUE, 
#                                      colour=`Total Num of Critical_flag`), 
#                                  data=location,alpha = 1, na.rm = T) + 
#   scale_colour_gradientn(colours = terrain.colors(10)) + ggtitle('Violation Map')
# 
# p = plot_geo() %>% 
#   layout(
#     hovermode = 'x',
#     margin = list(
#       t = 20,
#       b = 20,
#       l = 20,
#       r = 20),
#     legend = list(
#       orientation = 'h',
#       x = 0.5,
#       y = 1.01,
#       xanchor = 'center'))
# p <- style(p, hoverinfo = 'none')
# p <- plotly_build(p)
# str(p$x$layout$annotations) 


