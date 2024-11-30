text = """w,salt,FineSed,tss,NO3,DIP,DIN,PH,TN,TP,wave_dir,weekly_Coral_IN_up,erdepflux_total,Turbidity,Oxygen,temp,NH4"""
blah = []
f= text.split(",")
for i in range(1,len(f)):
    blah.append(f[i])
print(blah)