import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from statsmodels.tsa.stattools import adfuller
from statsmodels.formula.api import ols
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
import seaborn as sns


df = pd.read_csv('/Users/user/Documents/중규모 기상학/발표/pohang_temp_asos.csv',
                 parse_dates = ['year'], index_col = 'year')

df.plot(figsize = (12, 4))
plt.title('Annual average temperature in Pohang', fontsize = 12)


# 2. with drift & no trend
def adf_test_c(timeseries):
    print("Results of ADF Test (c):")
    dftest = adfuller(timeseries, autolag = "AIC")
    dfoutput = pd.Series(dftest[0:4], index = ["Test Statistic", "p-value", "# of lags", "# of Observations"])
    for key, value in dftest[4].items():
        dfoutput["Critical Value ({})".format(key)] = value
    print(dfoutput)

# 3. with drift & trend
def adf_test_ct(timeseries):
    print("Results of ADF Test (ct):")
    dftest = adfuller(timeseries, regression = "ctt", autolag = "AIC")
    dfoutput = pd.Series(dftest[0:4], index = ["Test Statistic", "p-value", "# of lags", "# of Observations"])
    for key, value in dftest[4].items():
        dfoutput["Critical Value ({})".format(key)] = value
    print(dfoutput)

adf_test_c(df)
adf_test_ct(df)


df2 = pd.read_csv('/Users/user/Documents/중규모 기상학/발표/pohang_temp_res.csv')
fit = ols('temp ~ num', data = df2).fit()
print(fit.summary())

res = df2['temp'] - fit.predict(df2)

fig = plt.figure(figsize = (12, 4))
plt.plot(df2['num'], res)
plt.title('Residuals')

plot_acf(np.array(res))
plot_pacf(np.array(res), method = 'ywm')


fig = plt.figure(figsize = (12, 4))
plt.plot(df2['num'], df2['temp'])
sns.regplot(x = 'num', y = 'temp', data = df2)
plt.title('Annual average temperature in Pohang', fontsize = 12)


plt.show()
