# Predictive Maintenance Analysis

## Project Overview
This project showcases the application of data science methodologies to conduct a predictive maintenance analysis. It involves an initial explorative data analysis phase to gather statistics and informative charts, followed by the deployment of four supervised machine learning algorithms aimed at identifying potential machine failures.

## Dataset
The study utilizes the "AI4I 2020 Predictive Maintenance" dataset, a time series of measurements from a working machine, sourced from the UCI Machine Learning Repository. It comprises 14 attributes over 10,000 instances.

## Failure Types
Machine failures are categorized into five distinct types:
1. TWF (Tool Wear Failure)
2. HDF (Heat Dissipation Failure)
3. PWF (Power Failure)
4. OSF (Overstrain Failure)
5. RNF (Random Failure)

A binary classification approach was adopted, labeling each instance with a machine failure as '1' and otherwise as '0'.

## Exploratory Phase
The analysis revealed a negative linear correlation between "Rotational speed" and "Torque," and a positive one between "Air temperature" and "Process temperature." Further graphical analysis suggested a significant occurrence of machine failures at peak air temperature levels.

## Machine Learning Models
Four supervised learning algorithms were employed, evaluated based on their F1 score due to class imbalance:
1. Decision Tree
2. K-Nearest Neighbors (KNN)
3. Logistic Regression
4. Kernel Machine (with a polynomial kernel function)

The Decision Tree model demonstrated the highest performance, followed by the Kernel Machine, Logistic Regression, and KNN.

## Conclusion
This predictive maintenance project leverages advanced data analytics and machine learning techniques to predict machinery failures, contributing valuable insights for preventive maintenance strategies.
