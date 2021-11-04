#Hari hara sudhan Venkateswaran

?lm()
Ads_Data = read.csv("D:/ISE 240/R Files/Advertising.csv")
names(Ads_Data)
Ads_Data
attach(Ads_Data)
lm.ads.TV <- lm(sales~TV)
lm.ads.radio <- lm(sales~radio)
lm.ads.newspaper <- lm(sales~newspaper)
lm.ads.radioz
lm.ads.TV
lm.ads.newspaper
summary(lm.ads.radio)
summary(lm.ads.TV)
summary(lm.ads.newspaper)

plot(sales,TV)
plot(TV,sales)
abline(lm.ads.TV,col = "Green")

lm.ads.radio.newspaper = lm(sales~radio+newspaper)
summary(lm.ads.radio.newspaper)
lm.ads.radio.TV = lm(sales~radio+TV)
lm.ads.TV.newspaper = lm(sales~TV+newspaper)
lm.ads.all = lm(sales~.,data = Ads_Data)
lm.ads.all
