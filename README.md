# NLMLapp
A Shiny app in R which allows researchers to build person-specific and pooled prediction models with elastic net regularized regression. Based on the functions from the NLML repository.

The app is still in beta, so if you run into any problems, be sure to mention them in the issues tab, or send me an email at nicolas.leenaerts@kuleuven.be.

## Getting started

To get the app working, you only need to run the following three lines of code!

```
devtools::install_github('nicolasleenaerts/NLMLapp')
library(NLMLapp)
NLMLapp()
```

## Materials

Need a tutorial on how to use the app? Then watch this [YouTube video](https://youtu.be/9tB9n4Njwz0)!

### Uploading data

You need to upload all the files you want to analyze on the 'Data' page. Importantly, the files need to be .xlsx files. They also need to be complete, meaning that they can't contain any missing data. If you have missing data that you want to deal with. Go to the [NLML repository](https://github.com/nicolasleenaerts/NLML/tree/main/Elastic%20Net/Multiple%20Imputation). 

![NLML_Data.png](https://github.com/nicolasleenaerts/NLML/blob/main/Images/NLML_Data.png?raw=true)

### Preprocessing

On the 'Preprocessing' page, you need to specificy whether you want to build a pooled prediction model or whether you want to make seperate person-specific ones. Also, you need to specificy how you want the model to be built and evaluated (e.g., cross-validation or a train-test split).

Looking for information on the options? Consult the wiki of the NLML github repository! [NLML WIKI](https://github.com/mikojeske/NLML/wiki/)

![NLML_Preprocessing.png](https://github.com/nicolasleenaerts/NLML/blob/main/Images/NLML_Preprocessing.png?raw=true)

### Analyses

After you have specified which model you want to build and how you want to train and test it, press 'Start' on the 'Model Training and Testing' page to run the analyses.

![NLML_Analyses.png](https://github.com/nicolasleenaerts/NLML/blob/main/Images/NLML_Analyses.png?raw=true)

### Results

You can look at the results and download them on the 'Results' page.

![NLML_Results.png](https://github.com/nicolasleenaerts/NLML/blob/main/Images/NLML_Results.png?raw=true)

### Plots

On the 'Plots' page you can make a plot showing a certain percentage of best predictors and how predictive they are. You can make a parametric plot, where the raw estimates of the elastic net regularized regression models are averaged across participants, or you can make a nonparametric plot, where the estimates are first converted into ranks (e.g., highest positive estimate becomes 1, the second highest 2, highest negative becomes -1, etc.) and then averaged across participants. 

![NLML_Plots.png](https://github.com/nicolasleenaerts/NLML/blob/main/Images/NLML_Plots.png?raw=true)

