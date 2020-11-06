require(fma)

plot(beer)
seasonplot(beer)
?decompose
decompose(beer)

tsdisplay(beer)
acf(beer)
plot(beer-mean(beer),lwd=2)
lines(lag(beer-mean(beer),1),col='red',lwd=1)
lines(lag(beer-mean(beer),-11),col='green',lwd=1)
HoltWinters(dowjones,alpha=0.5,beta=FALSE,gamma=FALSE)#sse
plot(forecast( HoltWinters(dowjones,alpha=0.5, beta = FALSE, gamma = FALSE),nahead=50))
HoltWinters(cowtemp,beta=FALSE,gamma=FALSE)
HoltWinters(dowjones,beta=FALSE,gamma=FALSE)#121 is Fn+1
HoltWinters(dowjones,beta=FALSE,gamma=FALSE)$fitted
#col1 = forecast (xhat), col2 = level, which is equal to the first col for sse

#A multiple time series with one column for the filtered series as well as
#for the level, trend and seasonal components, estimated contemporaneously 
#(that is at time t and not at the end of the series).
?HoltWinters


#plot(dowjones)
#double exponential smoothing algorithim
HoltWinters(dowjones,gamma=FALSE)#gives best alpha and beta
#a 121 is the level ln
#b = -0.3 is the
HoltWinters(dowjones,gamma=FALSE)$fitted
#col1 is forecast, col2 is the level,col3 is the bt
#note some 110.69 - 0.25 gives col1, 110.44
plot(HoltWinters(dowjones,gamma=FALSE)$fitted)
#top graph is bottom and middle graph added
?dowjones
?cowtemp



#HOLTWINTERS WITH SEASONALITY
?pollution
plot(pollution)
tsdisplay(pollution)
#looking at acf, it looks like something is happening every 4 months. 
#looking at pacf, theres a spike at 12... could be seasonal
#to get rid of a trend, you get derivative
tsdisplay(diff(pollution))
frequency(pollution)#gives period
HoltWinters(pollution,seasonal='additive')
HoltWinters(pollution,seasonal='multiplicative')
#gives error. Why? alpha/beta/gamma are decided by r to minimise the SSE
#try optim start
HoltWinters(pollution,seasonal='multiplicative',optim.start=c(alpha = 0.3, beta = 0.1, gamma = 0.1))
#doesnt work - try changing values, start with values in optim start and then try to find
#new optimal values. then it works. 
HoltWinters(pollution,seasonal='multiplicative',optim.start=c(alpha = 0.5, beta = 0.5, gamma = 0.5))$SSE
#use diff to remove trend and visualise seasonal component
tsdisplay(diff(airpass))
tsdisplay(diff(airpass),lag.max=100)
ggtsdisplay(diff(airpass,lag.max=100))
k=1#predict 1 period ahead
s=12 # frequency 12 month
Ln=HoltWinters(beer,seasonal='multiplicative')$coefficients[1]
bn=HoltWinters(beer,seasonal='multiplicative')$coefficients[2]
snk=HoltWinters(beer,seasonal='multiplicative')$coefficients[3]
(Ln+k*bn)*snk
#you can simply change the value of k to be 5 if you want to preidct 5mths ahead
#or the uquivalent
predict(HoltWinters(beer,seasonal='multiplicative'),n.ahead=5)[5]



#when data has no periodicity
acf(beer)$acf # way to see numerical values
tsdisplay(ts(mink,freq=10))#indicates a period
warnings()
frequency(beer)#gives periodicity
frequency(mink)#gives 1 - NO PERIODICITY
plot(mink)#but the plot indicates periodicity
HoltWinters(ts(mink,frequency=1))#doesn;t work as no period
HoltWinters(ts(mink,frequency=10))
seasonplot(ts(mink,freq=10))#wouldn't work if you don't specify period
?mink#number of minks trapped in 50 or so years
plot(decompose(beer))



#LINEAR REGRESSION
time = seq (1:length(dowjones))
myLinearModelwithTime<-lm(dowjones~time)
windows(5,5)
plot(time,dowjones,ylim=c(min(dowjones)-1,max(dowjones)+1))
par(new=TRUE,ann=FALSE)
plot(time,myLinearModelwithTime$fitted.values,col="red",lwd=3,ylim=c(min(dowjones)-1,max(dowjones)+1))
summary(myLinearModelwithTime)
length(dowjones)#78-2=76degrees of freedom

time2=time*time
myLinearModelwithTime<-lm(dowjones~time2)
summary(myLinearModelwithTime)
#residual sse of 1.89, when i used time it was 1.943
#so the time2 is better
plot (airpass)
tsdisplay(airpass)#theres a trend and seasonal component



#AR(p) simulation with R
#time 1-63 only 1 parameter. we've given the SD of the noise. the mean = 0
tsdisplay(arima.sim(n=63,list(ar=c(0.8897),sd=sqrt(0.1796))))
#0.8897 is phi0 and phi1 is -0.4858
tsdisplay(arima.sim(n=63,list(ar=c(0.8897,-0.4858),sd=sqrt(0.1796))))
tsdisplay(arima.sim(n=10000,list(ar=c(0.8897),sd=sqrt(0.1796))))
#we use properties to simulate a fake time series



#QUIZ
?optim
plot(bricksq)
frequency(bricksq)
HoltWinters(bricksq)
HoltWinters(bricksq,seasonal='additive')$SSE
#74994.82
HoltWinters(bricksq,seasonal='multiplicative',optim.start=c(alpha = 0.5, beta = 0.5, gamma = 0.5))$SSE
#77390.13
predict(HoltWinters(bricksq,seasonal='additive'),n.ahead=1)[1]


