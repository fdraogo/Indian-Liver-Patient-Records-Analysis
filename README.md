# Indian-Liver-Patient-Records-Analysis

## HarvardX PH125.9x Capstone Final
   According to the study description available here: https://www.kaggle.com/datasets/uciml/indian-liver-patient-records?select=indian_liver_patient.csv, there has been a continuous rise in the prevalence for liver disease in a local community in India. A list of potential suspects has been established including excessive consumption of alcohol, inhale of harmful gases, intake of contaminated food, pickles and drugs. For decision makers to elaborate effective policies, they need to have data-driven evidence that the overmentioned suspects are in fact contributing factors. A dataset consisting of 583 hospital patients was gathered. The dataset contains some demographic information (gender and age) along with some biological test results (level of bilirubin, alkaline phosphotase, alamine aminotransferase, aspartate aminotransferase, proteins, albumin, and the ratio of albumin to globulin) and a diagnosis of liver disease. For the later, the outcome is a binary variable taking the value of 1 if the patient has a liver disease and 0 otherwise.
   
   As a data scientist, I was given the opportunity to analyze the data and help the authorities handle the issue. My role is to come up with a model that is not only capable of identifying the driving factors, but more importantly, capable of predicting with high accuracy individuals within the community with higher probabilities of being diagnosed positive to the disease. That will allow decision makers to identify their high-risk individuals, to develop and deploy appropriate strategies and policies. Because of the nature of the response variable (binary variable), I relied on logit model which is more powerful in handling such type of response variable.
   
   All analyses were performed in R studio. Machine learning was used for the statistical analyses. Dataset was divided into training set (75% or the data) and testing set (25%). Several models were estimated, and the AIC was used as a criterion for best model selection. Prediction was performed using the best model and both the confusion matrix and the Receiver Operator Characteristic (ROC) curve were used to test the performance of the prediction.
   
   This project has three components: (1) a data file, (2) and Rmd file describing the data cleaning and analysis processes, and (3) a final report.
