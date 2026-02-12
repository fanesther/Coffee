
#assignment 3
#Task 1 Use the dataset on the “Coffee & Blood Pressure” tab of the datasheet to complete the tasks
#installation
install.packages("ggplot2")

#library
library(ggplot2)
#load data
cb <- read.csv("C:\\Users\\15878\\OneDrive\\Desktop\\data420\\coffee_bloodPressure.csv")

#create model for linear regression
model <- lm(Blood.Pressure..Systolic.Pressure. ~ Cups.of.Coffee, data = cb)
summary(model)

#correlation between coffee consumption and blood pressure
correlation <-  cor(cb$Cups.of.Coffee, cb$Blood.Pressure..Systolic.Pressure.)
print(paste("Correlation: ", correlation))

#plot the regression model 
ggplot(cb, aes(x = Cups.of.Coffee, y = Blood.Pressure..Systolic.Pressure.)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Coffee Consumption vs Blood Pressure",
       x = "Cups of Coffee",
       y = "Blood Pressure (Systolic)"
  )

#conclusion: The model above does indicate a positive relationship between coffee consumption and blood pressure, but the correlation is weak
#Reduce the coffee intake could potentially help to lower the blood pressure.

#Task 2.
#installation
install.packages("plotly")
#import library
library(plotly)
library(dplyr)
#load data
bsc <- read.csv("C:\\Users\\15878\\OneDrive\\Desktop\\data420\\dentalService.csv")

#data cleansing
#Remove rows with missing values in relevant columns
bsc_clean <- bsc %>% na.omit(Dentist....Visits., Price, Income..K.)

# Check the dimensions to ensure consistent sizes
dim(bsc_clean)

#create a model to display lineari progression
model1 <- lm(Price ~Income..K., data = bsc_clean)
model2<- predict(model1, bsc_clean)
summary(model1)

intercept <- coef(model1)[1]
slope <- coef(model1)[2]

# Create the scatter plot
plot_dentist_price <- plot_ly(
  data = bsc_clean, 
  x = ~Income..K., 
  y = ~Price, 
  type = "scatter", 
  mode = "markers", 
  marker = list(size = 10),
  color = ~Income..K.,
  text = ~paste("Dentists(#visits):", Income..K., "<br>Price: $", Price),
  hoverinfo = "text"
) %>%
  add_trace(
    x = ~Income..K., 
    y = ~model1,
    type = 'scatter',
    mode='lines',
    line = list(color = 'blue'), 
    name = "Regression Line"
  ) %>%
  layout(
    title = "Dentist Visits vs Price",
    xaxis = list(title = "Dentist Visits (#)"),
    yaxis = list(title = "Price ($)"),
    annotations = list(
      x = max(bsc_clean$Income..K.), 
      y = min(bsc_clean$Price),
      text = paste("y =", round(intercept, 2), "+", round(slope, 2), "* x"),
      showarrow = FALSE
    )
  )

# Display the plot
plot_dentist_price

