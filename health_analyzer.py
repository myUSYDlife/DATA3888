import pandas as pd 

df = pd.read_csv('chealth.csv')

print(df["ALGAE_COVER"].corr(df["HARD_COVER"]))
