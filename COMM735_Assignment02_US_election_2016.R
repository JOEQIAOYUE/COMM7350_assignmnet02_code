if (!require("gtrendsR")) install.packages("gtrendsR")
library (gtrendsR)
encoding = 'utf-8'
#————————————————————————————————————————————————————————————————————
#总统候选人搜索热度
#define multiple keywords
keywords = c("Hillary Clinton","Donald Trump")
#define the time window
#time = ("today 3-m") #try: 
time = ("2016-01-01 2016-12-31")

#define channels 
channel = 'web' #other channel include 'news', 'images','youtube'
#use '?gtrends' to find more description for arguments

geo = "US"

trends = gtrends(keywords, gprop = channel, time = time, geo = geo)
summary(trends)

plot(trends)
#新加的（in-class）
US_election_trend = trends$interest_over_time
#——————————————————————————————————————————————————————————————
#相关性测试
#单独关键词趋势：“希拉里”
keywords = c("Hillary Clinton")
time = ("2016-01-01 2016-12-31")
channel = 'web'
geo = "US"
Hillary_trends = gtrends(keywords, gprop = channel, time = time, geo = geo)
summary(Hillary_trends)
plot(Hillary_trends)
election_Hillary_trend = Hillary_trends$interest_over_time

#___________________
#单独关键词趋势：“川普”
keywords = c("Donald Trump")
time = ("2016-01-01 2016-12-31")
channel = 'web'
geo = "US"
Trump_trends = gtrends(keywords, gprop = channel, time = time, geo = geo)
summary(Trump_trends)
plot(Trump_trends)
election_Trump_trend = Trump_trends$interest_over_time

#相关性测试一

cor.test(election_Hillary_trend$hits,election_Trump_trend$hits)

#——————————————————————————————————————————————————————————————————————
#第二组相对量比较
#政党热度
keywords = c("Democratic Party","Republican Party")
time = ("2016-01-01 2016-12-31")
channel = 'web' 
geo = "US"

parties_trends = gtrends(keywords, gprop = channel, time = time, geo = geo)
summary(parties_trends)

plot(parties_trends)
#新加的（in-class）
US_parties_trend = parties_trends$interest_over_time
#——————————————————————————————————————————————————————————————
#相关性测试
#单独关键词趋势：“民主党”
keywords = c("Democratic Party")
time = ("2016-01-01 2016-12-31")
channel = 'web'
geo = "US"
Democratic_trends = gtrends(keywords, gprop = channel, time = time, geo = geo)
summary(Democratic_trends)
plot(Democratic_trends)
election_Democratic_trend = Democratic_trends$interest_over_time

#___________________
#单独关键词趋势：“共和党”
keywords = c("Republican Party")
time = ("2016-01-01 2016-12-31")
channel = 'web'
geo = "US"
Republican_trends = gtrends(keywords, gprop = channel, time = time, geo = geo)
summary(Republican_trends)
plot(Republican_trends)
election_Republican_trend = Republican_trends$interest_over_time

#相关性测试二

cor.test(election_Democratic_trend$hits,election_Republican_trend$hits)
#———————————————————————————————————————————————————————————————————

#第三组相对量比较
#政治倾向
keywords = c("Elitism","Populism")
time = ("2016-01-01 2016-12-31")
channel = 'web' 
geo = "US"

Political_Bias_trends = gtrends(keywords, gprop = channel, time = time, geo = geo)
summary(Political_Bias_trends)

plot(Political_Bias_trends)
#新加的（in-class）
US_Political_Bias_trend = Political_Bias_trends$interest_over_time
#——————————————————————————————————————————————————————————————
#相关性测试
#单独关键词趋势：“精英主义”
keywords = c("Elitism")
time = ("2016-01-01 2016-12-31")
channel = 'web'
geo = "US"
Elitism_trends = gtrends(keywords, gprop = channel, time = time, geo = geo)
summary(Elitism_trends)
plot(Elitism_trends)
election_Elitism_trend = Elitism_trends$interest_over_time

#___________________
#单独关键词趋势：“民粹主义”
keywords = c("Populism")
time = ("2016-01-01 2016-12-31")
channel = 'web'
geo = "US"
Populism_trends = gtrends(keywords, gprop = channel, time = time, geo = geo)
summary(Populism_trends)
plot(Populism_trends)
election_Populism_trend = Populism_trends$interest_over_time

#相关性测试三

cor.test(election_Elitism_trend$hits,election_Populism_trend$hits)



#总体相关性与三者图像呈现
#1-2，1-3
cor.test(US_election_trend$hits,US_parties_trend$hits)

par(mfrow=c(3,1))
plot(US_election_trend$date,US_election_trend$hits,type="l")
plot(US_parties_trend$date,US_parties_trend$hits,type="l") 
plot(US_Political_Bias_trend$date,US_Political_Bias_trend$hits,type="l") 

cor.test(US_parties_trend$hits,US_Political_Bias_trend$hits)

cor.test(US_election_trend$hits,US_Political_Bias_trend$hits)

