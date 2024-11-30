#!/usr/bin/env python
# coding: utf-8

# In[ ]:


import matplotlib.pyplot as plt

# R-squared values
r2_scores = [-0.309, 0.626, 0.565, -14.050]

# Model labels
model_labels = ['MLP', 'Random Forest', 'Decision Tree', 'Linear Regression']

plt.figure(figsize=(8, 6)) 

# Plotting the bar plot
plt.bar(model_labels, r2_scores)
plt.ylim([-15, 1])  # Adjust the y-axis limits as needed
plt.xlabel('Model')
plt.ylabel('R-squared Score')
plt.title('R-squared Scores for Regression Models')
plt.axhline(0, color='black', linestyle='-')
plt.show()


import matplotlib.pyplot as plt

# R-squared values
r2_scores = [-0.309, 0.626, 0.565]

plt.figure(figsize=(8, 6)) 

# Model labels
model_labels = ['MLP', 'Random Forest', 'Decision Tree']

# Plotting the bar plot
plt.bar(model_labels, r2_scores)
plt.ylim([-0.5, 1])  # Adjust the y-axis limits as needed
plt.xlabel('Model')
plt.ylabel('R-squared Score')
plt.title('R-squared Scores for Regression Models sans Linear Regression')
plt.axhline(0, color='black', linestyle='-')
plt.show()

