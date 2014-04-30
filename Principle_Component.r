Sys.setenv(TZ='UTC')
startDate = as.Date("2009-01-02") #Specify what date to get the prices from
endData = as.Date("2014-01-01")
initDate = as.Date("2009-01-01")
initEq = 1000000
Test_stock = "AAPL"
setwd("D:/Baruch_course/Algo_trading/Project")
source("librarys[F].r")


ticker_names = c("A","AA","AAPL")
for(syms in ticker_names)
{
  getSymbols(syms, src = "yahoo", from = startDate,to = endData)
}
y = y <- Ad(A)
for(ticker in ticker_names[-1])
{
  # first mergen then get rid of na
  x <- Ad(get(ticker))
  y <- merge(y,x,join = "left")
}# merge all the data into one table

v <- which(is.na(y), TRUE)[,2]# what data has the NA value
v <- unique(v)# try to find out the column
if(length(v)!=0)
  y = y[,-v]# remove those columns

y_Return = apply(y,2,diff)/y[-nrow(y),]
Y_Norm = scale(y_Return, center = TRUE, scale = TRUE) # get the normalized matrix

Matrix = cor(Y_Norm)
EI = eigen(Matrix)

Weights = EI$vectors*(matrix(1/apply(y_Return,2,sd),nrow=ncol(y),ncol=ncol(EI$vectors),byrow=F))
Per_Weights = Weights*(matrix(1/apply(Weights,2,sum),nrow=ncol(Weights),ncol=ncol(Weights),byrow=T))

EV_Portfolio_TS = as.xts(as.matrix(y_Return)%*%Per_Weights)

P_Stock = "P_stock"
P_Port = "P_Portfolio"
account.st = "Account"
arbstrat_P<-strategy("Mean_R_10P_Hedge")

currency("USD")
for(st in ticker_names)
{
  stock(st, currency="USD", multiplier=1)
}

########################################################################
index = 3## which stock to choose!!
Mean_Reversion = function(x)
{
  
  if(NROW(x)<60){ result = NA} else 
  {
    #stock = x[,ncol(x)]
    stock = Ad(x) # get the adjusted value
    LM = lm(stock~as.matrix(EV_Portfolio_TS))
    res <- LM$res
    alpha <- (LM$coef)[1]*252
    X_generator = function (n,data)# the n is the X_n which indicate the time
    {
      ans = sum(x[1:n])
      return(ans)
    }
    X = rep(0,60)# the X = [0,0,0,0,...,0]
    X = sapply(X = 1:60,FUN = function(n){sum(as.numeric(res)[1:n])})
    X.lm = lm(X[-length(X)]~X[-1])
    kappa = -log(X.lm$coef[2])*252
    m = X.lm$coef[1]/(1-X.lm$coef[2])
    rvar <- var(X.lm$res)
    sigma <- sqrt(rvar*2*kappa/(1-X.lm$coef[2]^2))
    sigma_eq <- sqrt(rvar/(1-X.lm$coef[2]^2))
    
    result <- -1*m/sigma_eq
    cat('s-score : ', result, '\n')
  }
  return(result)
}
N = 60 # estimation period
Mean_Reversion_Roll_Apply<- function(x)
{
  if(names(Ad(x))=="AAPL.Adjusted")
  {
    ans = rollapply(x,N,FUN = Mean_Reversion,by.column=FALSE)
    return (ans)
  }
  return(-100)
}

########################################################################
arbstrat_P<-add.indicator(
  strategy  =  arbstrat_P,
  name=  "Mean_Reversion_Roll_Apply",arguments	=list(x = quote(mktdata)),
  label		=	"sscore")

################################################ Signals #############################

arbstrat_P<-add.signal(
  strategy  		= arbstrat_P,
  name				= "sigThreshold",
  arguments			= list(
    #sigThreshold:cross  if TRUE, will return TRUE only 
    #for the first observation to cross the threshold in a run
    threshold		= 1.25,
    column			= "sscore",
    relationship	= "gte",
    cross			= TRUE),
  label				= "Selltime")

arbstrat_P<-add.signal(
  strategy			= arbstrat_P,
  name				= "sigThreshold",
  arguments			= list(
    threshold		= 0.1,
    # why this one is 0.1
    column			= "sscore",
    relationship	= "lt",
    cross			= TRUE),
  label				= "cashtime")

arbstrat_P<-add.signal(
  strategy  		= arbstrat_P,
  name				= "sigThreshold",
  arguments			= list(
    threshold		= -0.25,
    column			= "sscore",
    relationship	= "gt",
    cross			= TRUE),
  label				= "cashtime")

arbstrat_P<-add.signal(
  strategy  		= arbstrat_P,
  name				= "sigThreshold",
  arguments			= list(
    threshold		= -1.25,
    column			= "sscore",
    relationship	= "lte",
    cross			= TRUE),
  label				= "Buytime")
##############################################################

ruleSignal1<- function (data = mktdata, timestamp, sigcol, sigval, orderqty = 0, 
                        ordertype, orderside = NULL, threshold = NULL, tmult = FALSE, 
                        replace = TRUE, delay = 1e-04, osFUN = "osNoOp", pricemethod = c("market"), portfolio, symbol, ..., ruletype, 
                        TxnFees = 0, prefer = NULL, sethold = FALSE) 
{
  #View(data)
  if (!is.na(timestamp) && !is.na(data[timestamp][, sigcol]) && 
        data[timestamp][, sigcol] == sigval) # the signal has been trigerd
  {
    orderprice = Ad(get(Test_stock,env = .GlobalEnv))#Ad(data)[timestamp]
    orderqty <- 100
    addOrder(portfolio = portfolio, symbol = symbol, #symbol = symbol, 
             timestamp = timestamp, qty = orderqty, price = as.numeric(orderprice), 
             ordertype = ordertype, side = orderside, threshold = threshold, 
             status = "open", replace = replace, delay = delay, 
             tmult = tmult, ... = ..., TxnFees = TxnFees)
  }
}

arbstrat_P<- add.rule(arbstrat_P,
                      name      	=	"ruleSignal1",
                      arguments			=	list(
                        sigcol			=	"Buytime",
                        sigval			=	TRUE,
                        orderqty		=	1000,
                        ordertype		=	"market",
                        orderside		=	"long",
                        pricemethod		=	"market",
                        replace			=	TRUE,
                        TxnFees				=	1,
                        osFUN				=	osMaxPos), 
                      type				=	"enter",
                      path.dep			=	TRUE,
                      label				=	"Entry")


initPortf(name=P_Port, symbols = ticker_names, initDate=initDate)
initAcct(account.st, portfolios=P_Port, initDate=initDate, initEq=initEq)
#initAcct(account.st, portfolios=c(P_Stock,P_Port), initDate=initDate, initEq=initEq)

initOrders(portfolio=P_Port, initDate=initDate)
out <- applyStrategy(strategy=arbstrat_P, portfolios=P_Port)
