from sklearn.neural_network import MLPRegressor
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score, mean_squared_error
import pandas as pd
from scipy.stats import pearsonr

data = pd.read_csv("Clean Reef 2020 1H.csv")
data1 = pd.read_csv("Clean Reef 2020 2H.csv")
data2 = pd.read_csv("Clean Reef 2021 1H.csv")
data3 = pd.read_csv("Clean Reef 2021 2H.csv")
data4 = pd.read_csv("Clean Reef 2022 1H.csv")
data5 = pd.read_csv("Clean Reef 2022 2H.csv")

reef_data = [data,data1,data2,data3,data4,data5]
reef_data = pd.concat(reef_data)

x = reef_data[['tss', 'DIP', 'DIN', 'PH', 'Turbidity','temp','Oxygen','current_speed','TN','TP']]

test_var = "MA_N_gr"
y = reef_data[[test_var]]

print("STANDARD DEVIATION OF GROWTH: ",reef_data[[test_var]].std())
print("GROWTH RATE MEAN: ",reef_data[[test_var]].mean())
x_train, x_test, y_train, y_test = train_test_split(x,y,random_state=1,test_size=0.2)

x_trainsc = StandardScaler().fit_transform(x_train)
x_testsc = StandardScaler().fit_transform(x_test)

reg = MLPRegressor(hidden_layer_sizes=(64,64,64),activation="relu" ,random_state=1, max_iter=2000).fit(x_trainsc, y_train)
rf = RandomForestRegressor()
rf_fit = rf.fit(x_trainsc, y_train)
dt = DecisionTreeRegressor()
dt_fit = dt.fit(x_trainsc,y_train)
#lr = LogisticRegression().fit(x_trainsc,y_train)
linear = LinearRegression()
linear_fit = linear.fit(x_trainsc, y_train)


y_pred=reg.predict(x_testsc)
pred2 = rf_fit.predict(x_testsc)
pred3 = dt_fit.predict(x_testsc)
#pred4 = lr.predict(x_testsc)
pred5 = linear_fit.predict(x_testsc)

print("R Squared Score for MLP: ", r2_score(y_pred, y_test))
print(mean_squared_error(y_test, y_pred, squared=False))
#print(reg.coefs_[0])
#print('Pearson Correlation for MLP: ', pearsonr(y_pred, y_test)[0])
print("R Squared Score for Random Forest: ", r2_score(pred2, y_test))
print(rf.feature_importances_)
print(mean_squared_error(y_test, pred2, squared=False))
#print('Pearson Correlation for Random Forest: ', pearsonr(pred2, y_test)[0])
print("R Squared Score for Decision Tree: ", r2_score(pred3, y_test))
print(dt.feature_importances_)
print(mean_squared_error(y_test, pred3, squared=False))
#print('Pearson Correlation for Decision Tree: ', pearsonr(pred3, y_test)[0])
#print("R Squared Score for Logistic Regression: ", r2_score(pred4, y_test))
print("R Squared Score for Linear Regression: ", r2_score(pred5, y_test))
print(linear.coef_[0])
print(mean_squared_error(y_test, pred5, squared=False))
#print('Pearson Correlation for Linear Regression: ', pearsonr(pred5, y_test)[0])