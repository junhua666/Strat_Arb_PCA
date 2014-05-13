#!/usr/bin/env python
#
# Copyright 2013 Quantopian, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import matplotlib.pyplot as plt
import numpy as np
import statsmodels.api as sm
from datetime import datetime
import pytz
import pandas as pd

from zipline.algorithm import TradingAlgorithm
from zipline.finance import trading
from zipline.transforms import batch_transform
from zipline.utils.factory import load_from_yahoo
from zipline.finance.slippage import (
    VolumeShareSlippage,
    SlippageModel,
    FixedSlippage,
    transact_partial
)

def getReturn(Price_Data):
    """
    From the pd datafram price time series we can get to the Return Data Series.
    """
    # the data has been delete the last row but to get the return from price, we have to 
    # delete another row
    R_data = Price_Data.copy()
    for i in Price_Data.columns:
        R_data[i] = (Price_Data[i][1:].values - Price_Data[i][:-1].values)/Price_Data[i][:-1]
    return R_data


def Linear_Regression(R_data,sid1,sid2):# return data
    """
    The R_data is with nXm matrix with n observations and m factors.
    Each column will be the time series for each ticker name
    """
    # even though we change the order of getting data
    #ticker_list = R_data.columns.values
    
    #Depend_sid = ticker_list[sid1]
    #Indep_sids = ticker_list[sid2]
    
    Y = R_data[sid1]
    X = R_data[sid2]
    
    X = sm.add_constant(X, prepend=True)
    
    lm_Result = sm.OLS(Y, X).fit()
    return lm_Result
    

def get_Pridict_Resi(data, Liner_Object,sid1,sid2):
    """
    calculate the residule  by using the linear regression parameter that we passed into
    That is dS/S = alpha + beta * dI/I + dX we calculate the dX for each time.
    """
    
    #ticker_list = data.columns.values
    
    #Depend_sid = ticker_list[sid1]
    #Indep_sids = ticker_list[sid2]
    
    Y = data[sid1]
    X = data[sid2]
        
    resid = Y - Liner_Object.predict(sm.add_constant(X, prepend=True))
    return resid

def Normal_Data(R_data):
    for i in R_data.columns:
        R_data[i] = (R_data[i] - np.mean(R_data[i].values))/np.sqrt(np.square(np.std(R_data[i].values))*len(R_data[i].values)/(len(R_data[i].values)-1))
    return R_data

def PCA(R_data,sid2):
    Norm_data = R_data.copy()
    Norm_data.to_csv("Norm_test.csv",index = True ,cols=sid2)
    Norm_data = Normal_Data(Norm_data)
    Matrix = Norm_data.as_matrix(columns = sid2)
    Cov_M = np.corrcoef(Matrix,rowvar = 0)
    E_value, E_vector = np.linalg.eig(Cov_M)
    idx = (-E_value).argsort()
    E_value = E_value[idx]
    E_vector = E_vector[idx]
    Weights = E_vector.copy() # Weights is the matrix not dataframe
    # E_vector is just a matrix now and so does the Weights
    for i in xrange(0,len(Matrix[1,])):
        Weights[i,] = Weights[i,]/np.std(R_data.take([i],axis=1).values)
        
    Per_Weights = Weights.copy()
    for i in xrange(0,len(Per_Weights[1,])):
        Per_Weights[:,i] = Per_Weights[:,i]/np.sum(Per_Weights[:,i])
    
    Portfolio_Vector = R_data.copy()
    Portfolio_Vector = np.dot(R_data.as_matrix(),Per_Weights)
    return Per_Weights , Portfolio_Vector
    
    
@batch_transform
def ols_transform(data, sid1, sid2, PCA_Weight):

    """Computes regression coefficient (slope and intercept)
    via Ordinary Least Squares between two SIDs.
    """
    # the data will include the current price 
    # since we set the window to 20 and we have to calculate the return before current time
    # so we have 20-2 = 18 observations to calculate the sscore.
    R_data = getReturn(data.price)[:-1]
    regressor = np.dot(R_data.as_matrix(),PCA_Weight) 
    X = sm.add_constant(regressor, prepend=True)
    Y = R_data[sid1].values
    lm_Result = sm.OLS(Y, X).fit()
    resid = Y - lm_Result.predict(X)
    # the PCA_Weight will give you the weight of each stock
    # the R_PCA_Vector will gives you the 
    
    # we do not need to normalize data if we only use them to regression rather than PCA
    #Normal_R_data = Normal_Data(R_data)
    #if counter >= 30 or counter == 1 :
    #lm = Linear_Regression(R_data,sid1,sid2)
    #resid = get_Pridict_Resi(R_data, lm,sid1,sid2)
    Cum_resid = resid.copy()
    for i in xrange(1,len(resid)):
        Cum_resid[i] = resid[i]+Cum_resid[i-1]
    # the last one should really close to 0 since this is the residue of the linear regression
    
    # resid is a pd dataframe and slop and intercept are just double 
    intercept = lm_Result.params[0]
    slopes = lm_Result.params [1:]
    return Cum_resid, intercept, slopes


    
    
    


class Pairtrade(TradingAlgorithm):
    """Pairtrading relies on cointegration of two stocks.

    The expectation is that once the two stocks drifted apart
    (i.e. there is spread), they will eventually revert again. Thus,
    if we short the upward drifting stock and long the downward
    drifting stock (in short, we buy the spread) once the spread
    widened we can sell the spread with profit once they converged
    again. A nice property of this algorithm is that we enter the
    market in a neutral position.

    This specific algorithm tries to exploit the cointegration of
    Pepsi and Coca Cola by estimating the correlation between the
    two. Divergence of the spread is evaluated by z-scoring.
    """

    def initialize(self, stock_list,PCA_Weight, R_PCA_Protfolio, window_length=60):
        self.K_factors = 5
        self.spreads = []
        self.invested = 0
        self.window_length = window_length
        self.ols_transform = ols_transform(refresh_period=1,
                                           window_length=self.window_length)
        self.regressor_name = stock_list
        self.instant_fill = True
        self.slippage = FixedSlippage(spread = 0)
        self.sscore =[] # store those sscores which can be regarded as the hist

        self.beta = [] # keep track of the changing beta
        self.before_sid1_position = [] # keep track of the sid1 position
        self.before_sid2_position = [] # keep track of the sid2 position

        self.current_sid1_price = []
        self.current_sid2_price = []
        
        self.PCA_Weight = PCA_Weight
        self.PCA_R_PCA_Protfolio = R_PCA_Protfolio[:,0:self.K_factors] ##############################
        
        
        #self.slippage = VolumeShareSlippage(volume_limit = 1,price_impact = 0)
        #window_length: when you call the ols_transform function, how long the data from current time
        # you will trace back 
        # refresh_period: How long we recalculate the regression?
        # initial the decorator's refresh_period and window_length
        # make is own function so that the Algorithm can update it
        # generate a handler for the current object
        #refresh_period : int
        #Interval to wait between advances in the window.
        #window_length : int
        #How many days the trailing window should have.

    
    def handle_data(self, data):
        # the data is like: {'PEP': SIDData({'volume': 1000, 'sid': 'PEP', 'source_id': 'DataFrameSource-58d0fde27d9bd802ec3f0563c33696bd', 'dt': Timestamp('2000-01-03 00:00:00+0000', tz='UTC'), 'type': 4, 'price': 26.97}), 'KO': SIDData({'volume': 1000, 'sid': 'KO', 'source_id': 'DataFrameSource-58d0fde27d9bd802ec3f0563c33696bd', 'dt': Timestamp('2000-01-03 00:00:00+0000', tz='UTC'), 'type': 4, 'price': 19.81})}
        # which is a dictionary
        ######################################################
        # 1. Compute regression coefficients between PEP and KO
        
        
        params = self.ols_transform.handle_data(data, 'JPM', self.regressor_name,self.PCA_Weight)# because we @batch_transform so their is handle_data function in it
        if params is None:
            return
        Cum_resid, intercept, slopes = params
        

        zscore = self.compute_zscore(Cum_resid)
        self.record(zscores=zscore)
        sid1 = 'JPM'
        sid2 = self.regressor_name[:-1]
        
        self.place_orders(data, zscore, slopes,sid1,sid2)

    def compute_zscore(self, Cum_resid):
        """1. Compute the spread given slope and intercept.
           2. zscore the spread.
        """
        X_t = Cum_resid[1:len(Cum_resid)].copy() 
        X_t_one = sm.add_constant(Cum_resid[0:len(Cum_resid)-1],prepend=True).copy()
        #lm_Result = sm.OLS(X_t.values, X_t_one.values).fit()
        lm_Result = sm.OLS(X_t, X_t_one).fit()
        intercept,slope = lm_Result.params
        kappa = -1*np.log(slope) * 252
        m = intercept/(1-slope)
        rvar = np.var(X_t-lm_Result.predict(X_t_one))
        sigma = np.sqrt(rvar*2*kappa/(1-np.square(slope)))
        sigma_eq = np.sqrt(rvar/(1-np.square(slope)))
        zscore = -1*m/sigma_eq
        
        return zscore

    def place_orders(self, data, zscore, beta,sid1,sid2):
        """Buy spread if zscore is > 2, sell if zscore < .5.
        """
        #####
        #list_prices = []
        #Portfolio_Price = []
        #for sym in sid2:
        #    list_prices.append(data[sym].price)
        #list_prices.append(data[sid1].price)
        #for i in xrange(0,len(self.PCA_Weight[1,:])):
        #    Portfolio_Price[i] = np.dot(list_prices,self.PCA_Weight[:,1])
        self.beta.append(beta)
        self.sscore.append(zscore)
        #self.before_sid1_position.append(self.portfolio.positions[sid1].amount)
        #self.before_sid2_position.append(self.portfolio.positions[sid2].amount)
        
        
        #P_sid1 = self.trading_client.current_data[sid1].price
        #P_sid2 = self.trading_client.current_data[sid2].price
        #self.current_sid1_price.append(self.trading_client.current_data[sid1].price)
        #self.current_sid2_price.append(self.trading_client.current_data[sid2].price)
        
        
        if zscore >= 1.25 : #and not self.invested:
            if len(self.sscore)< 5 or zscore < np.max(self.sscore[len(self.sscore)-5:len(self.sscore)-1]):  
                self.order(sid1, -int(2000))
                amount = 2000*self.trading_client.current_data[sid1].price
                for index , sym in enumerate(sid2):
                    hedge_position = amount * np.sum(beta[0:self.K_factors] * self.PCA_Weight[index , 0:self.K_factors]) / self.trading_client.current_data[sym].price
                    if abs(hedge_position) > 1 :
                        self.order(sym, hedge_position)
                self.invested = True 

        elif zscore <= -1.25:# and not self.invested:
            if len(self.sscore)< 5 or zscore > np.max(self.sscore[len(self.sscore)-5:len(self.sscore)-1]):
                self.order(sid1, int(2000))
                amount = 2000*self.trading_client.current_data[sid1].price
                for index , sym in enumerate(sid2):
                    hedge_position = amount * np.sum(beta[0:self.K_factors] * self.PCA_Weight[index , 0:self.K_factors]) / self.trading_client.current_data[sym].price
                    if hedge_position > 1 :
                        self.order(sym, -1*hedge_position)
                self.invested = True

        elif abs(zscore) < .5 and self.invested:
            #self.sell_spread(sid1,sid2) # exit our position
            for sym in sid2 :
                amount = self.portfolio.positions[sym].amount
                self.order(sym, -1*amount)
            amount  = self.portfolio.positions[sid1].amount
            self.order(sid1, -1*amount)
            self.invested = False # indicating that we do not hold any shares of stocks
            #self.hit_before = False

            
            # dynamic hedge
            
    def sell_spread(self,sid1,sid2):
        """
        decrease exposure, regardless of position long/short.
        buy for a short position, sell for a long.
        """
        sad1_amount = self.portfolio.positions[sid1].amount
        self.order(sid1, -1 * sad1_amount)
        sad2_amount = self.portfolio.positions[sid2].amount
        self.order(sid2, -1 * sad2_amount)




if __name__ == '__main__':
    ###########################################################################################################
    start = datetime(2006, 1, 1, 0, 0, 0, 0, pytz.utc)
    end = datetime(2008, 1, 1, 0, 0, 0, 0, pytz.utc)
    Largest_Cap_Stocks = ['AAPL','XOM','GOOGL','MSFT','BRK.A','JNJ','GE','WFC','WMT','CVX','PG','VZ','IBM','T','PFE','ORCL','KO','MRK']
    Largest_Cap_Stocks.append('JPM')
    file_path = "D:\Baruch_course\Algo_trading\Python_Project\Pair_Trading\Data"
    #data = load_from_yahoo(stocks=Largest_Cap_Stocks, indexes={},start=start, end=end,adjusted=True)
    #data.to_csv('[data]18_SP500.csv', encoding='utf-8')
    data = pd.DataFrame.from_csv(file_path+"\[data]18_SP500.csv", parse_dates=True, encoding='utf-8')
    data.index = data.index.tz_localize(pytz.utc) 
    ## heard to save in the csv file and reget it. because the time zone and the timestampe is different
    
    
    #data.to_csv("PEP_KO_2007-2008.csv",index = True ,cols=('PEP','KO'))
    # the cols = () is order sensitive. If we save them as PEP and KO in cols them they will change the order
    # If want to load another dataset then denote these lines.
    ############################################################################################################
    #read_data = pd.DataFrame.from_csv('PEP_KO_2007-2008.csv',infer_datetime_format = True)
    #read_data.index = pd.DatetimeIndex(read_data["Date"])
    R_data = getReturn(data)[:-1]
    PCA_Weight, R_PCA_Protfolio = PCA(R_data[Largest_Cap_Stocks],Largest_Cap_Stocks)
    pairtrade = Pairtrade(Largest_Cap_Stocks, PCA_Weight, R_PCA_Protfolio)
    results = pairtrade.run(data)
    #pd.DataFrame.join(data, results.pnl, on = 'index')
    
    result = pd.concat([results.zscores,results.orders,results.positions,results.pnl],axis = 1)
    #more_result = pd.concat([pd.DataFrame(pairtrade.beta,columns = ['beta']),
    #                         pd.DataFrame(pairtrade.before_sid1_position,columns=['Pre_sid1']),
    #                         pd.DataFrame(pairtrade.before_sid2_position, columns = ['Pre_sid2']),
    #                         pd.DataFrame(pairtrade.sscore,columns = ['sscores']),
    #                         pd.DataFrame(pairtrade.current_sid1_price,columns = ['sid1_price']),
    #                         pd.DataFrame(pairtrade.current_sid2_price,columns = ['sid2_price'])],
    #                         axis = 1)
    
    result.to_csv("zscores_orders_position_2.csv")
    #more_result.to_csv("beta_pre_position_2.csv")


    #ax1 = plt.subplot(211)
    #data[['JPM', 'XLF']].plot(ax=ax1)
    #plt.ylabel('price')
    #plt.setp(ax1.get_xticklabels(), visible=False)

    ax2 = plt.subplot(212)
    results.zscores.plot(ax=ax2, color='r')
    plt.ylabel('zscored spread')

    plt.gcf().set_size_inches(18, 8)
    
    br = trading.environment.benchmark_returns
    bm_returns = br[(br.index >= start) & (br.index <= end)]
    results['benchmark_returns'] = (1 + bm_returns).cumprod().values
    results['algorithm_returns'] = (1 + results.returns).cumprod()
    fig = plt.figure()
    ax1 = fig.add_subplot(211, ylabel='cumulative returns')
    results[['algorithm_returns', 'benchmark_returns']].plot(ax=ax1,sharex=True)
    plt.show()