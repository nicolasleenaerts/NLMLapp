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

On the 'Data' page, you need to upload all the files you want to analyze. Importantly, the files need to be .xlsx files. They also need to be complete, meaning that they can't contain any missing data. If you have missing data that you want to deal with. Go to the [NLML repository]([Elastic Net/Multiple Imputation](https://github.com/nicolasleenaerts/NLML/tree/main/Elastic%20Net/Multiple%20Imputation)). 



Looking for information on the scripts? Consult the wiki of the NLML github repository!

[NLML WIKI](https://github.com/mikojeske/NLML/wiki/)
