rm( list = ls())
setwd('/media/zheqng/Seagate Backup Plus Drive/zheqng@nwu/文档/teaching lectures/2019年春季商务应用统计分析/作业/上机练习2/')
a = read.table('adult.data',sep =',')
names(a) <-c('age','workclass','fnlwgt','education','education-num','marital-status','occupation','relationship','race','sex','capital-gain','capital-loss','hours-per-week','native-country','level')
a.dis <-a[,-c(1,3,5,12,13)]
a.dis$`capital-gain` =as.vector( as.matrix(a.dis$`capital-gain`))
a.analysis <- a.dis[a.dis$`capital-gain` >0,]
attach(a.analysis)

`captial-gain` <- as.vector(`capital-gain`)
lm1 = lm(`capital-gain`~as.factor(level) + as.factor(occupation))
boxplot(`capital-gain`)
anova(lm1)
summary(lm1)
# age
# workclass
# fnlwgt
# education
# education-num
# marital-status
# occupation
# race
# sex
# capital-gain
# capital-loss
# hours-per-week
# native-country
# 1 3 5 10 11 12