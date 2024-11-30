import joblib
from sklearn.preprocessing import StandardScaler
import pandas as pd 

def macroalgae_result(tss, DIP, DIN, PH, Turbidity, temp, Oxygen, current_speed, TN, TP):

    inputs = [[tss, DIP, DIN, PH, Turbidity, temp, Oxygen, current_speed, TN, TP]]
    df = pd.DataFrame(inputs)
    df.columns = ['tss', 'DIP', 'DIN', 'PH', 'Turbidity','temp','Oxygen','current_speed','TN','TP']
    #print(df)
    model = joblib.load('RandomForestModel.pkl')
    data_ref = joblib.load('data_ref.pkl')
    #print(data_ref)
    scaler = StandardScaler().fit(data_ref)
    scaled_input = scaler.transform(inputs)
    #print(scaled_input)
    results = model.predict(scaled_input)
    
    return results[0]

#print(macroalgae_result(0.000357,  0.091478,  0.064695 , 4.023255 ,  1.23298889 , 18.031408,  10000.291 ,0.011175 , 0.89485  ,0.030444))