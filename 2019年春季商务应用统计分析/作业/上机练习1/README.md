3.27上机练习

用第二章所学的方差分析2.1-2.4的方法对这两个数据进行分析，并提交报告。

下周一课前交。


spss数据在Rstudio中调用方法：

library(foreign)

a = read.spss("iqdata.sav",to.data.frame = TRUE)
