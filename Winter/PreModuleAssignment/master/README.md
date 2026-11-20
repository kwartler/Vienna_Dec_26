# Master Pre-Module Assessment

Write a machine learning R script from scratch to predict customer churn. Load the data with:

```r
assignmentData <- read.csv('https://raw.githubusercontent.com/kwartler/teaching-datasets/main/WA_Fn-UseC_-Telco-Customer-Churn.csv')
```

Your script should cover each step:

1. **Sample:** perform appropriate sampling of the data.
2. **Explore:** build visualizations and summary statistics on the training data and identify any issues in the data.
3. **Modify:** process the data to account for missing/NA values, factors and outliers.
4. **Model:** perform **3** modeling techniques and tune appropriate parameters and/or change included variables.
5. **Assess:** compare the 3 models to identify the best one. Choose an *appropriate* metric like RMSE, MAPE, LogLoss or Tweedie Deviance based on your understanding of the problem.

Then create 2 presentation slides:

- **Slide 1, side by side.** Left: describe your machine learning workflow, including the steps you took to clean the data. Right: a scatterplot of the Y variable with the most correlated x variable, using the g docs theme from `ggthemes`.
- **Slide 2, three sections side by side.** In each: name the model, give a one to two sentence lay person's description of how it learns, list the parameters or variables you chose to optimize it, and list its KPI. Mark which model is best.

Turn in your R script and slides (pptx or PDF) on Moodle named `SURNAME_PRE`, with your name in the script header and on the slides.
