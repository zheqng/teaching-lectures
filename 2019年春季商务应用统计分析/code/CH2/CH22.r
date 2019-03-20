rm(list=ls())										#��յ�ǰ�����ռ�
a=read.csv("D:/Practical Business Data Analysis/case/CH2/real.csv",header=T)		#����csv��ʽ�����ݣ���ֵΪa
attach(a)										#�����ݼ�a�и�������ӵ������ռ䣬����ֱ�ӵ���
pairs(a[,c(1:6)])									#��a��ǰ6����ɢ��ͼ

boxplot(price~ring)									#����price��ring֮��ĺ�״ͼ

log.price=log(price)									#��price�����仯������ֵ��log.price
boxplot(log.price~ring)									#����log.price��ring֮��ĺ�״ͼ

par(mfrow=c(2,2))									#���û�ͼģʽ2x2�ĸ�ʽ
boxplot(log.price~dis)									#����log.price��dis֮��ĺ�״ͼ
boxplot(log.price~wuye)									#����log.price��wuye֮��ĺ�״ͼ
boxplot(log.price~fitment)								#����log.price��fitment֮��ĺ�״ͼ
boxplot(log.price~contype)								#����log.price��contype֮��ĺ�״ͼ

summary(a[,c(1:5)])									#����a��ǰ5�е������Է���ͳ��

lm1=lm(log.price~as.factor(ring))							#����ɢ����ring�������Ա����������ӷ������
library(car)										#��������car
Anova(lm1,type="III")									#��ģ��lm1�����ͷ������
summary(lm1)										#��ʾģ��lm1�ĸ�����ϸ�ڣ�������������ֵ��Pֵ��

lm2.1=lm(log.price~as.factor(ring)+as.factor(wuye))					#�����������õ�˫���ӷ������
Anova(lm2.1,type="III")									#��ģ��lm2.1�����ͷ������
summary(lm2.1)										#��ʾģ��lm2.1�ĸ�����ϸ�ڣ�������������ֵ��Pֵ��

lm2.2=lm(log.price~as.factor(ring)*as.factor(wuye))					#���������õ�˫���ӷ������
Anova(lm2.2,type="III")									#��ģ��lm2.2�����ͷ������
summary(lm2.2)										#��ʾģ��lm2.2�ĸ�����ϸ�ڣ�������������ֵ��Pֵ��

lm4=lm(log.price~as.factor(dis)*as.factor(ring)						
+as.factor(wuye)+as.factor(fitment)+as.factor(contype))					#�������б�����ȫģ�ͷ������
summary(lm4)										#��ʾģ��lm4�ĸ�����ϸ�ڣ�������������ֵ��Pֵ��

par(mfrow=c(2,2))									#���û�ͼģʽΪ2x2
plot(lm4,which=c(1:4))									#����lm4�ж�Ӧ��ģ�ͼ����4��ͼ�������в�ͼ��QQͼ��Cook����ͼ
par(mfrow=c(1,1))									#���û�ͼģʽΪ1x1

a0=read.csv("D:/Practical Business Data Analysis/case/CH2/new.csv")			#���������ݣ���ֵ��a0
a0=a0[,c(1:5)]										#ȡa0��ǰ5��	
a0											#չʾa0������

y.pred=exp(predict(lm4,a0))								#��ģ��lm4��a0��Ԥ��
a0$y.pred=y.pred									#��Ԥ������ֵ��a0�еı���y.pred
a0											#չʾa0������,����Ԥ��ֵ