# Question 2 - Using VARS to model the data

# libraries
# install.packages("vars")
# install.packages("greybox")
library(vars)
library(greybox)

# Data load
df = read.csv('/Users/danielroca/Desktop/Forecasting/Projectdata.csv')

# Define the train-test split, this must be changed to have multiple
# values that can be looped to have multiple models/coefficients
train_sample = 703
test_sample = 1068


# ----------------------------------------------------------------------------
# Question 2.1 - One var model for each store
# ----------------------------------------------------------------------------
# Extracting the columns of the CA_1 store and renaming them
df_CA_1 = df[c('Hobbies_CA_1','Household_1_CA_1','Household_2_CA_1'
               ,'Foods_1_CA_1','Foods_2_CA_1','Foods_3_CA_1' )]
colnames(df_CA_1) = c('Hobbies','Household_1','Household_2'
                       ,'Foods_1','Foods_2','Foods_3')

df_CA_2 = df[c('Hobbies_CA_2','Household_1_CA_2','Household_2_CA_2'
               ,'Foods_1_CA_2','Foods_2_CA_2','Foods_3_CA_2' )]
colnames(df_CA_2) = c('Hobbies','Household_1','Household_2'
                      ,'Foods_1','Foods_2','Foods_3')

df_CA_3 = df[c('Hobbies_CA_3','Household_1_CA_3','Household_2_CA_3'
               ,'Foods_1_CA_3','Foods_2_CA_3','Foods_3_CA_3' )]
colnames(df_CA_3) = c('Hobbies','Household_1','Household_2'
                      ,'Foods_1','Foods_2','Foods_3')

# Create train and test samples based on the previous variables
df_CA_1_train = head(df_CA_1, train_sample)
df_CA_1_test  = head(df_CA_1, test_sample)
df_CA_1_test  = tail(df_CA_1_test, test_sample - train_sample)

df_CA_2_train = head(df_CA_2, train_sample)
df_CA_2_test  = head(df_CA_2, test_sample)
df_CA_2_test  = tail(df_CA_2_test, test_sample - train_sample)

df_CA_3_train = head(df_CA_2, train_sample)
df_CA_3_test  = head(df_CA_3, test_sample)
df_CA_3_test  = tail(df_CA_3_test, test_sample - train_sample)



# Create VAR model we set p=1 because this was stated in the exercise h=1
# the VAR is created with the training data
var_CA_1_train = VAR(df_CA_1_train, p=1)
var_CA_2_train = VAR(df_CA_2_train, p=1)
var_CA_3_train = VAR(df_CA_3_train, p=1)


# the predict function doesn't allow for newdata with vars coming from this
# package, the coefficients of each variable are extracted to predict the 
# values and assess the error.
# predictions_CA_1 = predict(var_CA_1_train, newdata = ----, n.ahead = 1)

# Extract the coefficients
dcoef1 = coef(var_CA_1_train)
dcoef2 = coef(var_CA_2_train)
dcoef3 = coef(var_CA_3_train)

dcoef1
#How to get the standard error
std_err = dcoef1$Hobbies[1,2] + dcoef1$Hobbies[2,2] + dcoef1$Hobbies[3,2] + dcoef1$Hobbies[4,2] + dcoef1$Hobbies[5,2] + dcoef1$Hobbies[6,2] + dcoef1$Hobbies[7,2]
std_err = std_err * 1.96
std_err


# iterate the number of test samples to predict using the var coefficients
length_sample = test_sample - train_sample

for (i in 1:length_sample) {
  if (i==1) {
    df_CA_1_test[i, 7] = dcoef1$Hobbies[1]*df_CA_1_train[train_sample,1] + dcoef1$Hobbies[2]*df_CA_1_train[train_sample,2] + dcoef1$Hobbies[3]*df_CA_1_train[train_sample,3] + dcoef1$Hobbies[4]*df_CA_1_train[train_sample,4] + dcoef1$Hobbies[5]*df_CA_1_train[train_sample,5] + dcoef1$Hobbies[6]*df_CA_1_train[train_sample,6] + dcoef1$Hobbies[7]
    df_CA_1_test[i, 8] = dcoef1$Household_1[1]*df_CA_1_train[train_sample,1] + dcoef1$Household_1[2]*df_CA_1_train[train_sample,2] + dcoef1$Household_1[3]*df_CA_1_train[train_sample,3] + dcoef1$Household_1[4]*df_CA_1_train[train_sample,4] + dcoef1$Household_1[5]*df_CA_1_train[train_sample,5] + dcoef1$Household_1[6]*df_CA_1_train[train_sample,6] + dcoef1$Household_1[7]
    df_CA_1_test[i, 9] = dcoef1$Household_2[1]*df_CA_1_train[train_sample,1] + dcoef1$Household_2[2]*df_CA_1_train[train_sample,2] + dcoef1$Household_2[3]*df_CA_1_train[train_sample,3] + dcoef1$Household_2[4]*df_CA_1_train[train_sample,4] + dcoef1$Household_2[5]*df_CA_1_train[train_sample,5] + dcoef1$Household_2[6]*df_CA_1_train[train_sample,6] + dcoef1$Household_2[7]
    df_CA_1_test[i, 10] = dcoef1$Foods_1[1]*df_CA_1_train[train_sample,1] + dcoef1$Foods_1[2]*df_CA_1_train[train_sample,2] + dcoef1$Foods_1[3]*df_CA_1_train[train_sample,3] + dcoef1$Foods_1[4]*df_CA_1_train[train_sample,4] + dcoef1$Foods_1[5]*df_CA_1_train[train_sample,5] + dcoef1$Foods_1[6]*df_CA_1_train[train_sample,6] + dcoef1$Foods_1[7]
    df_CA_1_test[i, 11] = dcoef1$Foods_2[1]*df_CA_1_train[train_sample,1] + dcoef1$Foods_2[2]*df_CA_1_train[train_sample,2] + dcoef1$Foods_2[3]*df_CA_1_train[train_sample,3] + dcoef1$Foods_2[4]*df_CA_1_train[train_sample,4] + dcoef1$Foods_2[5]*df_CA_1_train[train_sample,5] + dcoef1$Foods_2[6]*df_CA_1_train[train_sample,6] + dcoef1$Foods_2[7]
    df_CA_1_test[i, 12] = dcoef1$Foods_3[1]*df_CA_1_train[train_sample,1] + dcoef1$Foods_3[2]*df_CA_1_train[train_sample,2] + dcoef1$Foods_3[3]*df_CA_1_train[train_sample,3] + dcoef1$Foods_3[4]*df_CA_1_train[train_sample,4] + dcoef1$Foods_3[5]*df_CA_1_train[train_sample,5] + dcoef1$Foods_3[6]*df_CA_1_train[train_sample,6] + dcoef1$Foods_3[7]

    df_CA_2_test[i, 7] = dcoef2$Hobbies[1]*df_CA_2_train[train_sample,1] + dcoef2$Hobbies[2]*df_CA_2_train[train_sample,2] + dcoef2$Hobbies[3]*df_CA_2_train[train_sample,3] + dcoef2$Hobbies[4]*df_CA_2_train[train_sample,4] + dcoef2$Hobbies[5]*df_CA_2_train[train_sample,5] + dcoef2$Hobbies[6]*df_CA_2_train[train_sample,6] + dcoef2$Hobbies[7]
    df_CA_2_test[i, 8] = dcoef2$Household_1[1]*df_CA_2_train[train_sample,1] + dcoef2$Household_1[2]*df_CA_2_train[train_sample,2] + dcoef2$Household_1[3]*df_CA_2_train[train_sample,3] + dcoef2$Household_1[4]*df_CA_2_train[train_sample,4] + dcoef2$Household_1[5]*df_CA_2_train[train_sample,5] + dcoef2$Household_1[6]*df_CA_2_train[train_sample,6] + dcoef2$Household_1[7]
    df_CA_2_test[i, 9] = dcoef2$Household_2[1]*df_CA_2_train[train_sample,1] + dcoef2$Household_2[2]*df_CA_2_train[train_sample,2] + dcoef2$Household_2[3]*df_CA_2_train[train_sample,3] + dcoef2$Household_2[4]*df_CA_2_train[train_sample,4] + dcoef2$Household_2[5]*df_CA_2_train[train_sample,5] + dcoef2$Household_2[6]*df_CA_2_train[train_sample,6] + dcoef2$Household_2[7]
    df_CA_2_test[i, 10] = dcoef2$Foods_1[1]*df_CA_2_train[train_sample,1] + dcoef2$Foods_1[2]*df_CA_2_train[train_sample,2] + dcoef2$Foods_1[3]*df_CA_2_train[train_sample,3] + dcoef2$Foods_1[4]*df_CA_2_train[train_sample,4] + dcoef2$Foods_1[5]*df_CA_2_train[train_sample,5] + dcoef2$Foods_1[6]*df_CA_2_train[train_sample,6] + dcoef2$Foods_1[7]
    df_CA_2_test[i, 11] = dcoef2$Foods_2[1]*df_CA_2_train[train_sample,1] + dcoef2$Foods_2[2]*df_CA_2_train[train_sample,2] + dcoef2$Foods_2[3]*df_CA_2_train[train_sample,3] + dcoef2$Foods_2[4]*df_CA_2_train[train_sample,4] + dcoef2$Foods_2[5]*df_CA_2_train[train_sample,5] + dcoef2$Foods_2[6]*df_CA_2_train[train_sample,6] + dcoef2$Foods_2[7]
    df_CA_2_test[i, 12] = dcoef2$Foods_3[1]*df_CA_2_train[train_sample,1] + dcoef2$Foods_3[2]*df_CA_2_train[train_sample,2] + dcoef2$Foods_3[3]*df_CA_2_train[train_sample,3] + dcoef2$Foods_3[4]*df_CA_2_train[train_sample,4] + dcoef2$Foods_3[5]*df_CA_2_train[train_sample,5] + dcoef2$Foods_3[6]*df_CA_2_train[train_sample,6] + dcoef2$Foods_3[7]

    df_CA_3_test[i, 7] = dcoef3$Hobbies[1]*df_CA_3_train[train_sample,1] + dcoef3$Hobbies[2]*df_CA_3_train[train_sample,2] + dcoef3$Hobbies[3]*df_CA_3_train[train_sample,3] + dcoef3$Hobbies[4]*df_CA_3_train[train_sample,4] + dcoef3$Hobbies[5]*df_CA_3_train[train_sample,5] + dcoef3$Hobbies[6]*df_CA_3_train[train_sample,6] + dcoef3$Hobbies[7]
    df_CA_3_test[i, 8] = dcoef3$Household_1[1]*df_CA_3_train[train_sample,1] + dcoef3$Household_1[2]*df_CA_3_train[train_sample,2] + dcoef3$Household_1[3]*df_CA_3_train[train_sample,3] + dcoef3$Household_1[4]*df_CA_3_train[train_sample,4] + dcoef3$Household_1[5]*df_CA_3_train[train_sample,5] + dcoef3$Household_1[6]*df_CA_3_train[train_sample,6] + dcoef3$Household_1[7]
    df_CA_3_test[i, 9] = dcoef3$Household_2[1]*df_CA_3_train[train_sample,1] + dcoef3$Household_2[2]*df_CA_3_train[train_sample,2] + dcoef3$Household_2[3]*df_CA_3_train[train_sample,3] + dcoef3$Household_2[4]*df_CA_3_train[train_sample,4] + dcoef3$Household_2[5]*df_CA_3_train[train_sample,5] + dcoef3$Household_2[6]*df_CA_3_train[train_sample,6] + dcoef3$Household_2[7]
    df_CA_3_test[i, 10] = dcoef3$Foods_1[1]*df_CA_3_train[train_sample,1] + dcoef3$Foods_1[2]*df_CA_3_train[train_sample,2] + dcoef3$Foods_1[3]*df_CA_3_train[train_sample,3] + dcoef3$Foods_1[4]*df_CA_3_train[train_sample,4] + dcoef3$Foods_1[5]*df_CA_3_train[train_sample,5] + dcoef3$Foods_1[6]*df_CA_3_train[train_sample,6] + dcoef3$Foods_1[7]
    df_CA_3_test[i, 11] = dcoef3$Foods_2[1]*df_CA_3_train[train_sample,1] + dcoef3$Foods_2[2]*df_CA_3_train[train_sample,2] + dcoef3$Foods_2[3]*df_CA_3_train[train_sample,3] + dcoef3$Foods_2[4]*df_CA_3_train[train_sample,4] + dcoef3$Foods_2[5]*df_CA_3_train[train_sample,5] + dcoef3$Foods_2[6]*df_CA_3_train[train_sample,6] + dcoef3$Foods_2[7]
    df_CA_3_test[i, 12] = dcoef3$Foods_3[1]*df_CA_3_train[train_sample,1] + dcoef3$Foods_3[2]*df_CA_3_train[train_sample,2] + dcoef3$Foods_3[3]*df_CA_3_train[train_sample,3] + dcoef3$Foods_3[4]*df_CA_3_train[train_sample,4] + dcoef3$Foods_3[5]*df_CA_3_train[train_sample,5] + dcoef3$Foods_3[6]*df_CA_3_train[train_sample,6] + dcoef3$Foods_3[7]
    
    }
  else {
    df_CA_1_test[i, 7] = dcoef1$Hobbies[1]*df_CA_1_test[i-1,1] + dcoef1$Hobbies[2]*df_CA_1_test[i-1,2] + dcoef1$Hobbies[3]*df_CA_1_test[i-1,3] + dcoef1$Hobbies[4]*df_CA_1_test[i-1,4] + dcoef1$Hobbies[5]*df_CA_1_test[i-1,5] + dcoef1$Hobbies[6]*df_CA_1_test[i-1,6] + dcoef1$Hobbies[7]
    df_CA_1_test[i, 8] = dcoef1$Household_1[1]*df_CA_1_test[i-1,1] + dcoef1$Household_1[2]*df_CA_1_test[i-1,2] + dcoef1$Household_1[3]*df_CA_1_test[i-1,3] + dcoef1$Household_1[4]*df_CA_1_test[i-1,4] + dcoef1$Household_1[5]*df_CA_1_test[i-1,5] + dcoef1$Household_1[6]*df_CA_1_test[i-1,6] + dcoef1$Household_1[7]
    df_CA_1_test[i, 9] = dcoef1$Household_2[1]*df_CA_1_test[i-1,1] + dcoef1$Household_2[2]*df_CA_1_test[i-1,2] + dcoef1$Household_2[3]*df_CA_1_test[i-1,3] + dcoef1$Household_2[4]*df_CA_1_test[i-1,4] + dcoef1$Household_2[5]*df_CA_1_test[i-1,5] + dcoef1$Household_2[6]*df_CA_1_test[i-1,6] + dcoef1$Household_2[7]
    df_CA_1_test[i, 10] = dcoef1$Foods_1[1]*df_CA_1_test[i-1,1] + dcoef1$Foods_1[2]*df_CA_1_test[i-1,2] + dcoef1$Foods_1[3]*df_CA_1_test[i-1,3] + dcoef1$Foods_1[4]*df_CA_1_test[i-1,4] + dcoef1$Foods_1[5]*df_CA_1_test[i-1,5] + dcoef1$Foods_1[6]*df_CA_1_test[i-1,6] + dcoef1$Foods_1[7]
    df_CA_1_test[i, 11] = dcoef1$Foods_2[1]*df_CA_1_test[i-1,1] + dcoef1$Foods_2[2]*df_CA_1_test[i-1,2] + dcoef1$Foods_2[3]*df_CA_1_test[i-1,3] + dcoef1$Foods_2[4]*df_CA_1_test[i-1,4] + dcoef1$Foods_2[5]*df_CA_1_test[i-1,5] + dcoef1$Foods_2[6]*df_CA_1_test[i-1,6] + dcoef1$Foods_2[7]
    df_CA_1_test[i, 12] = dcoef1$Foods_3[1]*df_CA_1_test[i-1,1] + dcoef1$Foods_3[2]*df_CA_1_test[i-1,2] + dcoef1$Foods_3[3]*df_CA_1_test[i-1,3] + dcoef1$Foods_3[4]*df_CA_1_test[i-1,4] + dcoef1$Foods_3[5]*df_CA_1_test[i-1,5] + dcoef1$Foods_3[6]*df_CA_1_test[i-1,6] + dcoef1$Foods_3[7]
    
    df_CA_2_test[i, 7] = dcoef2$Hobbies[1]*df_CA_2_test[i-1,1] + dcoef2$Hobbies[2]*df_CA_2_test[i-1,2] + dcoef2$Hobbies[3]*df_CA_2_test[i-1,3] + dcoef2$Hobbies[4]*df_CA_2_test[i-1,4] + dcoef2$Hobbies[5]*df_CA_2_test[i-1,5] + dcoef2$Hobbies[6]*df_CA_2_test[i-1,6] + dcoef2$Hobbies[7]
    df_CA_2_test[i, 8] = dcoef2$Household_1[1]*df_CA_2_test[i-1,1] + dcoef2$Household_1[2]*df_CA_2_test[i-1,2] + dcoef2$Household_1[3]*df_CA_2_test[i-1,3] + dcoef2$Household_1[4]*df_CA_2_test[i-1,4] + dcoef2$Household_1[5]*df_CA_2_test[i-1,5] + dcoef2$Household_1[6]*df_CA_2_test[i-1,6] + dcoef2$Household_1[7]
    df_CA_2_test[i, 9] = dcoef2$Household_2[1]*df_CA_2_test[i-1,1] + dcoef2$Household_2[2]*df_CA_2_test[i-1,2] + dcoef2$Household_2[3]*df_CA_2_test[i-1,3] + dcoef2$Household_2[4]*df_CA_2_test[i-1,4] + dcoef2$Household_2[5]*df_CA_2_test[i-1,5] + dcoef2$Household_2[6]*df_CA_2_test[i-1,6] + dcoef2$Household_2[7]
    df_CA_2_test[i, 10] = dcoef2$Foods_1[1]*df_CA_2_test[i-1,1] + dcoef2$Foods_1[2]*df_CA_2_test[i-1,2] + dcoef2$Foods_1[3]*df_CA_2_test[i-1,3] + dcoef2$Foods_1[4]*df_CA_2_test[i-1,4] + dcoef2$Foods_1[5]*df_CA_2_test[i-1,5] + dcoef2$Foods_1[6]*df_CA_2_test[i-1,6] + dcoef2$Foods_1[7]
    df_CA_2_test[i, 11] = dcoef2$Foods_2[1]*df_CA_2_test[i-1,1] + dcoef2$Foods_2[2]*df_CA_2_test[i-1,2] + dcoef2$Foods_2[3]*df_CA_2_test[i-1,3] + dcoef2$Foods_2[4]*df_CA_2_test[i-1,4] + dcoef2$Foods_2[5]*df_CA_2_test[i-1,5] + dcoef2$Foods_2[6]*df_CA_2_test[i-1,6] + dcoef2$Foods_2[7]
    df_CA_2_test[i, 12] = dcoef2$Foods_3[1]*df_CA_2_test[i-1,1] + dcoef2$Foods_3[2]*df_CA_2_test[i-1,2] + dcoef2$Foods_3[3]*df_CA_2_test[i-1,3] + dcoef2$Foods_3[4]*df_CA_2_test[i-1,4] + dcoef2$Foods_3[5]*df_CA_2_test[i-1,5] + dcoef2$Foods_3[6]*df_CA_2_test[i-1,6] + dcoef2$Foods_3[7]
    
    df_CA_3_test[i, 7] = dcoef3$Hobbies[1]*df_CA_3_test[i-1,1] + dcoef3$Hobbies[2]*df_CA_3_test[i-1,2] + dcoef3$Hobbies[3]*df_CA_3_test[i-1,3] + dcoef3$Hobbies[4]*df_CA_3_test[i-1,4] + dcoef3$Hobbies[5]*df_CA_3_test[i-1,5] + dcoef3$Hobbies[6]*df_CA_3_test[i-1,6] + dcoef3$Hobbies[7]
    df_CA_3_test[i, 8] = dcoef3$Household_1[1]*df_CA_3_test[i-1,1] + dcoef3$Household_1[2]*df_CA_3_test[i-1,2] + dcoef3$Household_1[3]*df_CA_3_test[i-1,3] + dcoef3$Household_1[4]*df_CA_3_test[i-1,4] + dcoef3$Household_1[5]*df_CA_3_test[i-1,5] + dcoef3$Household_1[6]*df_CA_3_test[i-1,6] + dcoef3$Household_1[7]
    df_CA_3_test[i, 9] = dcoef3$Household_2[1]*df_CA_3_test[i-1,1] + dcoef3$Household_2[2]*df_CA_3_test[i-1,2] + dcoef3$Household_2[3]*df_CA_3_test[i-1,3] + dcoef3$Household_2[4]*df_CA_3_test[i-1,4] + dcoef3$Household_2[5]*df_CA_3_test[i-1,5] + dcoef3$Household_2[6]*df_CA_3_test[i-1,6] + dcoef3$Household_2[7]
    df_CA_3_test[i, 10] = dcoef3$Foods_1[1]*df_CA_3_test[i-1,1] + dcoef3$Foods_1[2]*df_CA_3_test[i-1,2] + dcoef3$Foods_1[3]*df_CA_3_test[i-1,3] + dcoef3$Foods_1[4]*df_CA_3_test[i-1,4] + dcoef3$Foods_1[5]*df_CA_3_test[i-1,5] + dcoef3$Foods_1[6]*df_CA_3_test[i-1,6] + dcoef3$Foods_1[7]
    df_CA_3_test[i, 11] = dcoef3$Foods_2[1]*df_CA_3_test[i-1,1] + dcoef3$Foods_2[2]*df_CA_3_test[i-1,2] + dcoef3$Foods_2[3]*df_CA_3_test[i-1,3] + dcoef3$Foods_2[4]*df_CA_3_test[i-1,4] + dcoef3$Foods_2[5]*df_CA_3_test[i-1,5] + dcoef3$Foods_2[6]*df_CA_3_test[i-1,6] + dcoef3$Foods_2[7]
    df_CA_3_test[i, 12] = dcoef3$Foods_3[1]*df_CA_3_test[i-1,1] + dcoef3$Foods_3[2]*df_CA_3_test[i-1,2] + dcoef3$Foods_3[3]*df_CA_3_test[i-1,3] + dcoef3$Foods_3[4]*df_CA_3_test[i-1,4] + dcoef3$Foods_3[5]*df_CA_3_test[i-1,5] + dcoef3$Foods_3[6]*df_CA_3_test[i-1,6] + dcoef3$Foods_3[7]
    
    
  }
}

colnames(df_CA_1_test) = c('Hobbies','Household_1','Household_2'
                           ,'Foods_1','Foods_2','Foods_3',
                           'Hobbies_pred','Household_1_pred','Household_2_pred'
                           ,'Foods_1_pred','Foods_2_pred','Foods_3_pred')

colnames(df_CA_2_test) = c('Hobbies','Household_1','Household_2'
                           ,'Foods_1','Foods_2','Foods_3',
                           'Hobbies_pred','Household_1_pred','Household_2_pred'
                           ,'Foods_1_pred','Foods_2_pred','Foods_3_pred')

colnames(df_CA_3_test) = c('Hobbies','Household_1','Household_2'
                           ,'Foods_1','Foods_2','Foods_3',
                           'Hobbies_pred','Household_1_pred','Household_2_pred'
                           ,'Foods_1_pred','Foods_2_pred','Foods_3_pred')

forecasts_CA_1_21 = df_CA_1_test
forecasts_CA_2_21 = df_CA_2_test
forecasts_CA_3_21 = df_CA_3_test

forecasts_CA_1_21$Hobbies_High = forecasts_CA_1_21$Hobbies_pred + 1.96 * (dcoef1$Hobbies[1,2] + dcoef1$Hobbies[2,2] + dcoef1$Hobbies[3,2] + dcoef1$Hobbies[4,2] + dcoef1$Hobbies[5,2] + dcoef1$Hobbies[6,2] + dcoef1$Hobbies[7,2])
forecasts_CA_1_21$Hobbies_Low = forecasts_CA_1_21$Hobbies_pred - 1.96 * (dcoef1$Hobbies[1,2] + dcoef1$Hobbies[2,2] + dcoef1$Hobbies[3,2] + dcoef1$Hobbies[4,2] + dcoef1$Hobbies[5,2] + dcoef1$Hobbies[6,2] + dcoef1$Hobbies[7,2])

# remove variables that will not be used anymore
rm(dcoef1, dcoef2, dcoef3)
rm(df_CA_1,df_CA_1_test,df_CA_1_train)
rm(df_CA_2,df_CA_2_test,df_CA_2_train)
rm(df_CA_3,df_CA_3_test,df_CA_3_train)
rm(var_CA_1_train,var_CA_2_train,var_CA_3_train)

# Provisional
length_x = test_sample-train_sample
x = 1:length_x

# Plot hobbies real vs predicted forecasts for each store

plot(x, forecasts_CA_1_21$Hobbies, type = "l", frame = FALSE, pch = 19, col = "blue", xlab = "time", ylab = "Hobbies Store 1")
polygon(c(x,rev(x)),c(forecasts_CA_1_21$Hobbies_Low,rev(forecasts_CA_1_21$Hobbies_High)),col="thistle",border=NA)
lines(x, forecasts_CA_1_21$Hobbies, type = "l",  pch = 19, col = "blue")
lines(x, forecasts_CA_1_21$Hobbies_pred, pch = 18, col = "red", type = "l")
legend("bottomright", legend=c("Real", "Predicted", "C.I. 95%"),col=c("blue", "red","thistle"), lty = 1:1, cex=0.8)
title("Hobbies Stores 1 - VAR Store 1")

# Create RMSSE Data frames
df_rmsse_CA_1 = data.frame()
df_rmsse_CA_2 = data.frame()
df_rmsse_CA_3 = data.frame()

# Save the RMSSE Results in a data frame
df_rmsse_CA_1[1, 1] = RMSSE(forecasts_CA_1_21$Hobbies, forecasts_CA_1_21$Hobbies_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_1[2, 1] = RMSSE(forecasts_CA_1_21$Household_1, forecasts_CA_1_21$Household_1_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_1[3, 1] = RMSSE(forecasts_CA_1_21$Household_2, forecasts_CA_1_21$Household_2_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_1[4, 1] = RMSSE(forecasts_CA_1_21$Foods_1, forecasts_CA_1_21$Foods_1_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_1[5, 1] = RMSSE(forecasts_CA_1_21$Foods_2, forecasts_CA_1_21$Foods_2_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_1[6, 1] = RMSSE(forecasts_CA_1_21$Foods_3, forecasts_CA_1_21$Foods_3_pred, scale = 1, na.rm = TRUE)

df_rmsse_CA_2[1, 1] = RMSSE(forecasts_CA_2_21$Hobbies, forecasts_CA_2_21$Hobbies_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_2[2, 1] = RMSSE(forecasts_CA_2_21$Household_1, forecasts_CA_2_21$Household_1_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_2[3, 1] = RMSSE(forecasts_CA_2_21$Household_2, forecasts_CA_2_21$Household_2_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_2[4, 1] = RMSSE(forecasts_CA_2_21$Foods_1, forecasts_CA_2_21$Foods_1_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_2[5, 1] = RMSSE(forecasts_CA_2_21$Foods_2, forecasts_CA_2_21$Foods_2_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_2[6, 1] = RMSSE(forecasts_CA_2_21$Foods_3, forecasts_CA_2_21$Foods_3_pred, scale = 1, na.rm = TRUE)

df_rmsse_CA_3[1, 1] = RMSSE(forecasts_CA_3_21$Hobbies, forecasts_CA_3_21$Hobbies_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_3[2, 1] = RMSSE(forecasts_CA_3_21$Household_1, forecasts_CA_3_21$Household_1_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_3[3, 1] = RMSSE(forecasts_CA_3_21$Household_2, forecasts_CA_3_21$Household_2_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_3[4, 1] = RMSSE(forecasts_CA_3_21$Foods_1, forecasts_CA_3_21$Foods_1_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_3[5, 1] = RMSSE(forecasts_CA_3_21$Foods_2, forecasts_CA_3_21$Foods_2_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_3[6, 1] = RMSSE(forecasts_CA_3_21$Foods_3, forecasts_CA_3_21$Foods_3_pred, scale = 1, na.rm = TRUE)

# ----------------------------------------------------------------------------
# Question 2.2 - One var model for each type of product
# ----------------------------------------------------------------------------

# Extracting the columns of the CA_1 store and renaming them
df_hobbies = df[c('Hobbies_CA_1','Hobbies_CA_2','Hobbies_CA_3')]
df_household1 = df[c('Household_1_CA_1','Household_1_CA_2','Household_1_CA_3')]
df_household2 = df[c('Household_2_CA_1','Household_2_CA_2','Household_2_CA_3')]
df_foods1 = df[c('Foods_1_CA_1','Foods_1_CA_2','Foods_1_CA_3')]
df_foods2 = df[c('Foods_2_CA_1','Foods_2_CA_2','Foods_2_CA_3')]
df_foods3 = df[c('Foods_3_CA_1','Foods_3_CA_2','Foods_3_CA_3')]

# Create train and test samples based on the previous variables
df_hobbies_train = head(df_hobbies, train_sample)
df_hobbies_test  = tail(df_hobbies, test_sample)

df_household1_train = head(df_household1, train_sample)
df_household1_test  = tail(df_household1, test_sample)

df_household2_train = head(df_household2, train_sample)
df_household2_test  = tail(df_household2, test_sample)

df_foods1_train = head(df_foods1, train_sample)
df_foods1_test  = tail(df_foods1, test_sample)

df_foods2_train = head(df_foods2, train_sample)
df_foods2_test  = tail(df_foods2, test_sample)

df_foods3_train = head(df_foods3, train_sample)
df_foods3_test  = tail(df_foods3, test_sample)

# Create VAR model we set p=1 because this was stated in the exercise h=1
# the VAR is created with the training data
var_hobbies_train = VAR(df_hobbies_train, p=1)
var_household1_train = VAR(df_household1_train, p=1)
var_household2_train = VAR(df_household2_train, p=1)
var_foods1_train = VAR(df_foods1_train, p=1)
var_foods2_train = VAR(df_foods2_train, p=1)
var_foods3_train = VAR(df_foods3_train, p=1)

# Extract the coefficients
dcoef_hobbies = coef(var_hobbies_train)
dcoef_household1 = coef(var_household1_train)
dcoef_household2 = coef(var_household2_train)
dcoef_foods1 = coef(var_foods1_train)
dcoef_foods2 = coef(var_foods2_train)
dcoef_foods3 = coef(var_foods3_train)

dcoef_hobbies

# iterate the number of test samples to predict using the var coefficients
for (i in 1:test_sample) {
  if (i==1) {
    df_hobbies_test[i, 4] = dcoef_hobbies$Hobbies_CA_1[1]*df_hobbies_train[train_sample,1] + dcoef_hobbies$Hobbies_CA_1[2]*df_hobbies_train[train_sample,2] + dcoef_hobbies$Hobbies_CA_1[3]*df_hobbies_train[train_sample,3]+ dcoef_hobbies$Hobbies_CA_1[4]
    df_hobbies_test[i, 5] = dcoef_hobbies$Hobbies_CA_2[1]*df_hobbies_train[train_sample,1] + dcoef_hobbies$Hobbies_CA_2[2]*df_hobbies_train[train_sample,2] + dcoef_hobbies$Hobbies_CA_2[3]*df_hobbies_train[train_sample,3]+ dcoef_hobbies$Hobbies_CA_2[4]
    df_hobbies_test[i, 6] = dcoef_hobbies$Hobbies_CA_3[1]*df_hobbies_train[train_sample,1] + dcoef_hobbies$Hobbies_CA_3[2]*df_hobbies_train[train_sample,2] + dcoef_hobbies$Hobbies_CA_3[3]*df_hobbies_train[train_sample,3]+ dcoef_hobbies$Hobbies_CA_3[4]

    df_household1_test[i, 4] = dcoef_household1$Household_1_CA_1[1]*df_household1_train[train_sample,1] + dcoef_household1$Household_1_CA_1[2]*df_household1_train[train_sample,2] + dcoef_household1$Household_1_CA_1[3]*df_household1_train[train_sample,3]+ dcoef_household1$Household_1_CA_1[4]
    df_household1_test[i, 5] = dcoef_household1$Household_1_CA_2[1]*df_household1_train[train_sample,1] + dcoef_household1$Household_1_CA_2[2]*df_household1_train[train_sample,2] + dcoef_household1$Household_1_CA_2[3]*df_household1_train[train_sample,3]+ dcoef_household1$Household_1_CA_2[4]
    df_household1_test[i, 6] = dcoef_household1$Household_1_CA_3[1]*df_household1_train[train_sample,1] + dcoef_household1$Household_1_CA_3[2]*df_household1_train[train_sample,2] + dcoef_household1$Household_1_CA_3[3]*df_household1_train[train_sample,3]+ dcoef_household1$Household_1_CA_3[4]
    
    df_household2_test[i, 4] = dcoef_household2$Household_2_CA_1[1]*df_household2_train[train_sample,1] + dcoef_household2$Household_2_CA_1[2]*df_household2_train[train_sample,2] + dcoef_household2$Household_2_CA_1[3]*df_household2_train[train_sample,3]+ dcoef_household2$Household_2_CA_1[4]
    df_household2_test[i, 5] = dcoef_household2$Household_2_CA_2[1]*df_household2_train[train_sample,1] + dcoef_household2$Household_2_CA_2[2]*df_household2_train[train_sample,2] + dcoef_household2$Household_2_CA_2[3]*df_household2_train[train_sample,3]+ dcoef_household2$Household_2_CA_2[4]
    df_household2_test[i, 6] = dcoef_household2$Household_2_CA_3[1]*df_household2_train[train_sample,1] + dcoef_household2$Household_2_CA_3[2]*df_household2_train[train_sample,2] + dcoef_household2$Household_2_CA_3[3]*df_household2_train[train_sample,3]+ dcoef_household2$Household_2_CA_3[4]
    
    df_foods1_test[i, 4] = dcoef_foods1$Foods_1_CA_1[1]*df_foods1_train[train_sample,1] + dcoef_foods1$Foods_1_CA_1[2]*df_foods1_train[train_sample,2] + dcoef_foods1$Foods_1_CA_1[3]*df_foods1_train[train_sample,3]+ dcoef_foods1$Foods_1_CA_1[4]
    df_foods1_test[i, 5] = dcoef_foods1$Foods_1_CA_2[1]*df_foods1_train[train_sample,1] + dcoef_foods1$Foods_1_CA_2[2]*df_foods1_train[train_sample,2] + dcoef_foods1$Foods_1_CA_2[3]*df_foods1_train[train_sample,3]+ dcoef_foods1$Foods_1_CA_2[4]
    df_foods1_test[i, 6] = dcoef_foods1$Foods_1_CA_3[1]*df_foods1_train[train_sample,1] + dcoef_foods1$Foods_1_CA_3[2]*df_foods1_train[train_sample,2] + dcoef_foods1$Foods_1_CA_3[3]*df_foods1_train[train_sample,3]+ dcoef_foods1$Foods_1_CA_3[4]
    
    df_foods2_test[i, 4] = dcoef_foods2$Foods_2_CA_1[1]*df_foods2_train[train_sample,1] + dcoef_foods2$Foods_2_CA_1[2]*df_foods2_train[train_sample,2] + dcoef_foods2$Foods_2_CA_1[3]*df_foods2_train[train_sample,3]+ dcoef_foods2$Foods_2_CA_1[4]
    df_foods2_test[i, 5] = dcoef_foods2$Foods_2_CA_2[1]*df_foods2_train[train_sample,1] + dcoef_foods2$Foods_2_CA_2[2]*df_foods2_train[train_sample,2] + dcoef_foods2$Foods_2_CA_2[3]*df_foods2_train[train_sample,3]+ dcoef_foods2$Foods_2_CA_2[4]
    df_foods2_test[i, 6] = dcoef_foods2$Foods_2_CA_3[1]*df_foods2_train[train_sample,1] + dcoef_foods2$Foods_2_CA_3[2]*df_foods2_train[train_sample,2] + dcoef_foods2$Foods_2_CA_3[3]*df_foods2_train[train_sample,3]+ dcoef_foods2$Foods_2_CA_3[4]
    
    df_foods3_test[i, 4] = dcoef_foods3$Foods_3_CA_1[1]*df_foods3_train[train_sample,1] + dcoef_foods3$Foods_3_CA_1[2]*df_foods3_train[train_sample,2] + dcoef_foods3$Foods_3_CA_1[3]*df_foods3_train[train_sample,3]+ dcoef_foods3$Foods_3_CA_1[4]
    df_foods3_test[i, 5] = dcoef_foods3$Foods_3_CA_2[1]*df_foods3_train[train_sample,1] + dcoef_foods3$Foods_3_CA_2[2]*df_foods3_train[train_sample,2] + dcoef_foods3$Foods_3_CA_2[3]*df_foods3_train[train_sample,3]+ dcoef_foods3$Foods_3_CA_2[4]
    df_foods3_test[i, 6] = dcoef_foods3$Foods_3_CA_3[1]*df_foods3_train[train_sample,1] + dcoef_foods3$Foods_3_CA_3[2]*df_foods3_train[train_sample,2] + dcoef_foods3$Foods_3_CA_3[3]*df_foods3_train[train_sample,3]+ dcoef_foods3$Foods_3_CA_3[4]
    
    
  }
  else {

    df_hobbies_test[i, 4] = dcoef_hobbies$Hobbies_CA_1[1]*df_hobbies_test[i-1,1] + dcoef_hobbies$Hobbies_CA_1[2]*df_hobbies_test[i-1,2] + dcoef_hobbies$Hobbies_CA_1[3]*df_hobbies_test[i-1,3]+ dcoef_hobbies$Hobbies_CA_1[4]
    df_hobbies_test[i, 5] = dcoef_hobbies$Hobbies_CA_2[1]*df_hobbies_test[i-1,1] + dcoef_hobbies$Hobbies_CA_2[2]*df_hobbies_test[i-1,2] + dcoef_hobbies$Hobbies_CA_2[3]*df_hobbies_test[i-1,3]+ dcoef_hobbies$Hobbies_CA_2[4]
    df_hobbies_test[i, 6] = dcoef_hobbies$Hobbies_CA_3[1]*df_hobbies_test[i-1,1] + dcoef_hobbies$Hobbies_CA_3[2]*df_hobbies_test[i-1,2] + dcoef_hobbies$Hobbies_CA_3[3]*df_hobbies_test[i-1,3]+ dcoef_hobbies$Hobbies_CA_3[4]
    
    df_household1_test[i, 4] = dcoef_household1$Household_1_CA_1[1]*df_household1_test[i-1,1] + dcoef_household1$Household_1_CA_1[2]*df_household1_test[i-1,2] + dcoef_household1$Household_1_CA_1[3]*df_household1_test[i-1,3]+ dcoef_household1$Household_1_CA_1[4]
    df_household1_test[i, 5] = dcoef_household1$Household_1_CA_2[1]*df_household1_test[i-1,1] + dcoef_household1$Household_1_CA_2[2]*df_household1_test[i-1,2] + dcoef_household1$Household_1_CA_2[3]*df_household1_test[i-1,3]+ dcoef_household1$Household_1_CA_2[4]
    df_household1_test[i, 6] = dcoef_household1$Household_1_CA_3[1]*df_household1_test[i-1,1] + dcoef_household1$Household_1_CA_3[2]*df_household1_test[i-1,2] + dcoef_household1$Household_1_CA_3[3]*df_household1_test[i-1,3]+ dcoef_household1$Household_1_CA_3[4]
    
    df_household2_test[i, 4] = dcoef_household2$Household_2_CA_1[1]*df_household2_test[i-1,1] + dcoef_household2$Household_2_CA_1[2]*df_household2_test[i-1,2] + dcoef_household2$Household_2_CA_1[3]*df_household2_test[i-1,3]+ dcoef_household2$Household_2_CA_1[4]
    df_household2_test[i, 5] = dcoef_household2$Household_2_CA_2[1]*df_household2_test[i-1,1] + dcoef_household2$Household_2_CA_2[2]*df_household2_test[i-1,2] + dcoef_household2$Household_2_CA_2[3]*df_household2_test[i-1,3]+ dcoef_household2$Household_2_CA_2[4]
    df_household2_test[i, 6] = dcoef_household2$Household_2_CA_3[1]*df_household2_test[i-1,1] + dcoef_household2$Household_2_CA_3[2]*df_household2_test[i-1,2] + dcoef_household2$Household_2_CA_3[3]*df_household2_test[i-1,3]+ dcoef_household2$Household_2_CA_3[4]
    
    df_foods1_test[i, 4] = dcoef_foods1$Foods_1_CA_1[1]*df_foods1_test[i-1,1] + dcoef_foods1$Foods_1_CA_1[2]*df_foods1_test[i-1,2] + dcoef_foods1$Foods_1_CA_1[3]*df_foods1_test[i-1,3]+ dcoef_foods1$Foods_1_CA_1[4]
    df_foods1_test[i, 5] = dcoef_foods1$Foods_1_CA_2[1]*df_foods1_test[i-1,1] + dcoef_foods1$Foods_1_CA_2[2]*df_foods1_test[i-1,2] + dcoef_foods1$Foods_1_CA_2[3]*df_foods1_test[i-1,3]+ dcoef_foods1$Foods_1_CA_2[4]
    df_foods1_test[i, 6] = dcoef_foods1$Foods_1_CA_3[1]*df_foods1_test[i-1,1] + dcoef_foods1$Foods_1_CA_3[2]*df_foods1_test[i-1,2] + dcoef_foods1$Foods_1_CA_3[3]*df_foods1_test[i-1,3]+ dcoef_foods1$Foods_1_CA_3[4]    
    
    df_foods2_test[i, 4] = dcoef_foods2$Foods_2_CA_1[1]*df_foods2_test[i-1,1] + dcoef_foods2$Foods_2_CA_1[2]*df_foods2_test[i-1,2] + dcoef_foods2$Foods_2_CA_1[3]*df_foods2_test[i-1,3]+ dcoef_foods2$Foods_2_CA_1[4]
    df_foods2_test[i, 5] = dcoef_foods2$Foods_2_CA_2[1]*df_foods2_test[i-1,1] + dcoef_foods2$Foods_2_CA_2[2]*df_foods2_test[i-1,2] + dcoef_foods2$Foods_2_CA_2[3]*df_foods2_test[i-1,3]+ dcoef_foods2$Foods_2_CA_2[4]
    df_foods2_test[i, 6] = dcoef_foods2$Foods_2_CA_3[1]*df_foods2_test[i-1,1] + dcoef_foods2$Foods_2_CA_3[2]*df_foods2_test[i-1,2] + dcoef_foods2$Foods_2_CA_3[3]*df_foods2_test[i-1,3]+ dcoef_foods2$Foods_2_CA_3[4]    
    
    df_foods3_test[i, 4] = dcoef_foods3$Foods_3_CA_1[1]*df_foods3_test[i-1,1] + dcoef_foods3$Foods_3_CA_1[2]*df_foods3_test[i-1,2] + dcoef_foods3$Foods_3_CA_1[3]*df_foods3_test[i-1,3]+ dcoef_foods3$Foods_3_CA_1[4]
    df_foods3_test[i, 5] = dcoef_foods3$Foods_3_CA_2[1]*df_foods3_test[i-1,1] + dcoef_foods3$Foods_3_CA_2[2]*df_foods3_test[i-1,2] + dcoef_foods3$Foods_3_CA_2[3]*df_foods3_test[i-1,3]+ dcoef_foods3$Foods_3_CA_2[4]
    df_foods3_test[i, 6] = dcoef_foods3$Foods_3_CA_3[1]*df_foods3_test[i-1,1] + dcoef_foods3$Foods_3_CA_3[2]*df_foods3_test[i-1,2] + dcoef_foods3$Foods_3_CA_3[3]*df_foods3_test[i-1,3]+ dcoef_foods3$Foods_3_CA_3[4]    
    
  }
}

colnames(df_hobbies_test) = c('Hobbies_CA_1','Hobbies_CA_2','Hobbies_CA_3'
                           ,'Hobbies_CA_1_pred','Hobbies_CA_2_pred','Hobbies_CA_3_pred')

colnames(df_household1_test) = c('Household_1_CA_1','Household_1_CA_2','Household_1_CA_3'
                              ,'Household_1_CA_1_pred','Household_1_CA_2_pred','Household_1_CA_3_pred')

colnames(df_household2_test) = c('Household_2_CA_1','Household_2_CA_2','Household_2_CA_3'
                                 ,'Household_2_CA_1_pred','Household_2_CA_2_pred','Household_2_CA_3_pred')

colnames(df_foods1_test) = c('Foods_1_CA_1','Foods_1_CA_2','Foods_1_CA_3'
                                 ,'Foods_1_CA_1_pred','Foods_1_CA_2_pred','Foods_1_CA_3_pred')

colnames(df_foods2_test) = c('Foods_2_CA_1','Foods_2_CA_2','Foods_2_CA_3'
                             ,'Foods_2_CA_1_pred','Foods_2_CA_2_pred','Foods_2_CA_3_pred')

colnames(df_foods3_test) = c('Foods_3_CA_1','Foods_3_CA_2','Foods_3_CA_3'
                             ,'Foods_3_CA_1_pred','Foods_3_CA_2_pred','Foods_3_CA_3_pred')


forecasts_hobbies_22 = df_hobbies_test
forecasts_household1_22 = df_household1_test
forecasts_household2_22 = df_household2_test
forecasts_foods1_22 = df_foods1_test
forecasts_foods2_22 = df_foods2_test
forecasts_foods3_22 = df_foods3_test

# remove variables that will not be used anymore
rm(dcoef_hobbies, dcoef_household1, dcoef_household2, dcoef_foods1, dcoef_foods2, dcoef_foods3)
rm(df_hobbies, df_hobbies_train, df_hobbies_test)
rm(df_household1, df_household1_train, df_household1_test)
rm(df_household2, df_household2_train, df_household2_test)
rm(df_foods1, df_foods1_train, df_foods1_test)
rm(df_foods2, df_foods2_train, df_foods2_test)
rm(df_foods3, df_foods3_train, df_foods3_test)
rm(var_hobbies_train, var_household1_train, var_household2_train, var_foods1_train, var_foods2_train, var_foods3_train)

# Save the RMSSE Results in the previous 2.1 dataframe
df_rmsse_CA_1[1, 2] = RMSSE(forecasts_hobbies_22$Hobbies_CA_1, forecasts_hobbies_22$Hobbies_CA_1_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_1[2, 2] = RMSSE(forecasts_household1_22$Household_1_CA_1, forecasts_household1_22$Household_1_CA_1_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_1[3, 2] = RMSSE(forecasts_household2_22$Household_2_CA_1, forecasts_household2_22$Household_2_CA_1_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_1[4, 2] = RMSSE(forecasts_foods1_22$Foods_1_CA_1, forecasts_foods1_22$Foods_1_CA_1_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_1[5, 2] = RMSSE(forecasts_foods2_22$Foods_2_CA_1, forecasts_foods2_22$Foods_2_CA_1_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_1[6, 2] = RMSSE(forecasts_foods3_22$Foods_3_CA_1, forecasts_foods3_22$Foods_3_CA_1_pred, scale = 1, na.rm = TRUE)

df_rmsse_CA_2[1, 2] = RMSSE(forecasts_hobbies_22$Hobbies_CA_2, forecasts_hobbies_22$Hobbies_CA_2_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_2[2, 2] = RMSSE(forecasts_household1_22$Household_1_CA_2, forecasts_household1_22$Household_1_CA_2_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_2[3, 2] = RMSSE(forecasts_household2_22$Household_2_CA_2, forecasts_household2_22$Household_2_CA_2_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_2[4, 2] = RMSSE(forecasts_foods1_22$Foods_1_CA_2, forecasts_foods1_22$Foods_1_CA_2_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_2[5, 2] = RMSSE(forecasts_foods2_22$Foods_2_CA_2, forecasts_foods2_22$Foods_2_CA_2_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_2[6, 2] = RMSSE(forecasts_foods3_22$Foods_3_CA_2, forecasts_foods3_22$Foods_3_CA_2_pred, scale = 1, na.rm = TRUE)

df_rmsse_CA_3[1, 2] = RMSSE(forecasts_hobbies_22$Hobbies_CA_3, forecasts_hobbies_22$Hobbies_CA_3_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_3[2, 2] = RMSSE(forecasts_household1_22$Household_1_CA_3, forecasts_household1_22$Household_1_CA_3_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_3[3, 2] = RMSSE(forecasts_household2_22$Household_2_CA_3, forecasts_household2_22$Household_2_CA_3_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_3[4, 2] = RMSSE(forecasts_foods1_22$Foods_1_CA_3, forecasts_foods1_22$Foods_1_CA_3_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_3[5, 2] = RMSSE(forecasts_foods2_22$Foods_2_CA_3, forecasts_foods2_22$Foods_2_CA_3_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_3[6, 2] = RMSSE(forecasts_foods3_22$Foods_3_CA_3, forecasts_foods3_22$Foods_3_CA_3_pred, scale = 1, na.rm = TRUE)


# ----------------------------------------------------------------------------
# Question 2.3 - One big VAR Model
# ----------------------------------------------------------------------------

# Create train and test samples based on the previous variables

df_all = df
df_all$date = NULL

df_all_train = head(df_all, train_sample)
df_all_test  = head(df_all, test_sample)
df_all_test  = tail(df_all_test, test_sample - train_sample)

# Create VAR model we set p=1 because this was stated in the exercise h=1
# the VAR is created with the training data
var_all_train = VAR(df_all_train, p=1)

# Extract the coefficients
dcoef_all = coef(var_all_train)

dcoef_all$Hobbies_CA_1

# iterate the number of test samples to predict using the var coefficients

length_sample = test_sample - train_sample

for (i in 1:length_sample) {
  if (i==1) {
    df_all_test[i, 19] = dcoef_all$Hobbies_CA_1[1]*df_all_train[train_sample,1] + dcoef_all$Hobbies_CA_1[2]*df_all_train[train_sample,2] + dcoef_all$Hobbies_CA_1[3]*df_all_train[train_sample,3] + dcoef_all$Hobbies_CA_1[4]*df_all_train[train_sample,4] + dcoef_all$Hobbies_CA_1[5]*df_all_train[train_sample,5] + dcoef_all$Hobbies_CA_1[6]*df_all_train[train_sample,6] + dcoef_all$Hobbies_CA_1[7]*df_all_train[train_sample,7] + dcoef_all$Hobbies_CA_1[8]*df_all_train[train_sample,8] + dcoef_all$Hobbies_CA_1[9]*df_all_train[train_sample,9] + dcoef_all$Hobbies_CA_1[10]*df_all_train[train_sample,10] + dcoef_all$Hobbies_CA_1[11]*df_all_train[train_sample,11] + dcoef_all$Hobbies_CA_1[12]*df_all_train[train_sample,12] + dcoef_all$Hobbies_CA_1[13]*df_all_train[train_sample,13] + dcoef_all$Hobbies_CA_1[14]*df_all_train[train_sample,14] + dcoef_all$Hobbies_CA_1[15]*df_all_train[train_sample,15] + dcoef_all$Hobbies_CA_1[16]*df_all_train[train_sample,16] + dcoef_all$Hobbies_CA_1[17]*df_all_train[train_sample,17] + dcoef_all$Hobbies_CA_1[18]*df_all_train[train_sample,18] + dcoef_all$Hobbies_CA_1[19]
    df_all_test[i, 20] = dcoef_all$Household_1_CA_1[1]*df_all_train[train_sample,1] + dcoef_all$Household_1_CA_1[2]*df_all_train[train_sample,2] + dcoef_all$Household_1_CA_1[3]*df_all_train[train_sample,3] + dcoef_all$Household_1_CA_1[4]*df_all_train[train_sample,4] + dcoef_all$Household_1_CA_1[5]*df_all_train[train_sample,5] + dcoef_all$Household_1_CA_1[6]*df_all_train[train_sample,6] + dcoef_all$Household_1_CA_1[7]*df_all_train[train_sample,7] + dcoef_all$Household_1_CA_1[8]*df_all_train[train_sample,8] + dcoef_all$Household_1_CA_1[9]*df_all_train[train_sample,9] + dcoef_all$Household_1_CA_1[10]*df_all_train[train_sample,10] + dcoef_all$Household_1_CA_1[11]*df_all_train[train_sample,11] + dcoef_all$Household_1_CA_1[12]*df_all_train[train_sample,12] + dcoef_all$Household_1_CA_1[13]*df_all_train[train_sample,13] + dcoef_all$Household_1_CA_1[14]*df_all_train[train_sample,14] + dcoef_all$Household_1_CA_1[15]*df_all_train[train_sample,15] + dcoef_all$Household_1_CA_1[16]*df_all_train[train_sample,16] + dcoef_all$Household_1_CA_1[17]*df_all_train[train_sample,17] + dcoef_all$Household_1_CA_1[18]*df_all_train[train_sample,18] + dcoef_all$Household_1_CA_1[19]
    df_all_test[i, 21] = dcoef_all$Household_2_CA_1[1]*df_all_train[train_sample,1] + dcoef_all$Household_2_CA_1[2]*df_all_train[train_sample,2] + dcoef_all$Household_2_CA_1[3]*df_all_train[train_sample,3] + dcoef_all$Household_2_CA_1[4]*df_all_train[train_sample,4] + dcoef_all$Household_2_CA_1[5]*df_all_train[train_sample,5] + dcoef_all$Household_2_CA_1[6]*df_all_train[train_sample,6] + dcoef_all$Household_2_CA_1[7]*df_all_train[train_sample,7] + dcoef_all$Household_2_CA_1[8]*df_all_train[train_sample,8] + dcoef_all$Household_2_CA_1[9]*df_all_train[train_sample,9] + dcoef_all$Household_2_CA_1[10]*df_all_train[train_sample,10] + dcoef_all$Household_2_CA_1[11]*df_all_train[train_sample,11] + dcoef_all$Household_2_CA_1[12]*df_all_train[train_sample,12] + dcoef_all$Household_2_CA_1[13]*df_all_train[train_sample,13] + dcoef_all$Household_2_CA_1[14]*df_all_train[train_sample,14] + dcoef_all$Household_2_CA_1[15]*df_all_train[train_sample,15] + dcoef_all$Household_2_CA_1[16]*df_all_train[train_sample,16] + dcoef_all$Household_2_CA_1[17]*df_all_train[train_sample,17] + dcoef_all$Household_2_CA_1[18]*df_all_train[train_sample,18] + dcoef_all$Household_2_CA_1[19]
    df_all_test[i, 22] = dcoef_all$Foods_1_CA_1[1]*df_all_train[train_sample,1] + dcoef_all$Foods_1_CA_1[2]*df_all_train[train_sample,2] + dcoef_all$Foods_1_CA_1[3]*df_all_train[train_sample,3] + dcoef_all$Foods_1_CA_1[4]*df_all_train[train_sample,4] + dcoef_all$Foods_1_CA_1[5]*df_all_train[train_sample,5] + dcoef_all$Foods_1_CA_1[6]*df_all_train[train_sample,6] + dcoef_all$Foods_1_CA_1[7]*df_all_train[train_sample,7] + dcoef_all$Foods_1_CA_1[8]*df_all_train[train_sample,8] + dcoef_all$Foods_1_CA_1[9]*df_all_train[train_sample,9] + dcoef_all$Foods_1_CA_1[10]*df_all_train[train_sample,10] + dcoef_all$Foods_1_CA_1[11]*df_all_train[train_sample,11] + dcoef_all$Foods_1_CA_1[12]*df_all_train[train_sample,12] + dcoef_all$Foods_1_CA_1[13]*df_all_train[train_sample,13] + dcoef_all$Foods_1_CA_1[14]*df_all_train[train_sample,14] + dcoef_all$Foods_1_CA_1[15]*df_all_train[train_sample,15] + dcoef_all$Foods_1_CA_1[16]*df_all_train[train_sample,16] + dcoef_all$Foods_1_CA_1[17]*df_all_train[train_sample,17] + dcoef_all$Foods_1_CA_1[18]*df_all_train[train_sample,18] + dcoef_all$Foods_1_CA_1[19]
    df_all_test[i, 23] = dcoef_all$Foods_2_CA_1[1]*df_all_train[train_sample,1] + dcoef_all$Foods_2_CA_1[2]*df_all_train[train_sample,2] + dcoef_all$Foods_2_CA_1[3]*df_all_train[train_sample,3] + dcoef_all$Foods_2_CA_1[4]*df_all_train[train_sample,4] + dcoef_all$Foods_2_CA_1[5]*df_all_train[train_sample,5] + dcoef_all$Foods_2_CA_1[6]*df_all_train[train_sample,6] + dcoef_all$Foods_2_CA_1[7]*df_all_train[train_sample,7] + dcoef_all$Foods_2_CA_1[8]*df_all_train[train_sample,8] + dcoef_all$Foods_2_CA_1[9]*df_all_train[train_sample,9] + dcoef_all$Foods_2_CA_1[10]*df_all_train[train_sample,10] + dcoef_all$Foods_2_CA_1[11]*df_all_train[train_sample,11] + dcoef_all$Foods_2_CA_1[12]*df_all_train[train_sample,12] + dcoef_all$Foods_2_CA_1[13]*df_all_train[train_sample,13] + dcoef_all$Foods_2_CA_1[14]*df_all_train[train_sample,14] + dcoef_all$Foods_2_CA_1[15]*df_all_train[train_sample,15] + dcoef_all$Foods_2_CA_1[16]*df_all_train[train_sample,16] + dcoef_all$Foods_2_CA_1[17]*df_all_train[train_sample,17] + dcoef_all$Foods_2_CA_1[18]*df_all_train[train_sample,18] + dcoef_all$Foods_2_CA_1[19]
    df_all_test[i, 24] = dcoef_all$Foods_3_CA_1[1]*df_all_train[train_sample,1] + dcoef_all$Foods_3_CA_1[2]*df_all_train[train_sample,2] + dcoef_all$Foods_3_CA_1[3]*df_all_train[train_sample,3] + dcoef_all$Foods_3_CA_1[4]*df_all_train[train_sample,4] + dcoef_all$Foods_3_CA_1[5]*df_all_train[train_sample,5] + dcoef_all$Foods_3_CA_1[6]*df_all_train[train_sample,6] + dcoef_all$Foods_3_CA_1[7]*df_all_train[train_sample,7] + dcoef_all$Foods_3_CA_1[8]*df_all_train[train_sample,8] + dcoef_all$Foods_3_CA_1[9]*df_all_train[train_sample,9] + dcoef_all$Foods_3_CA_1[10]*df_all_train[train_sample,10] + dcoef_all$Foods_3_CA_1[11]*df_all_train[train_sample,11] + dcoef_all$Foods_3_CA_1[12]*df_all_train[train_sample,12] + dcoef_all$Foods_3_CA_1[13]*df_all_train[train_sample,13] + dcoef_all$Foods_3_CA_1[14]*df_all_train[train_sample,14] + dcoef_all$Foods_3_CA_1[15]*df_all_train[train_sample,15] + dcoef_all$Foods_3_CA_1[16]*df_all_train[train_sample,16] + dcoef_all$Foods_3_CA_1[17]*df_all_train[train_sample,17] + dcoef_all$Foods_3_CA_1[18]*df_all_train[train_sample,18] + dcoef_all$Foods_3_CA_1[19]
    df_all_test[i, 25] = dcoef_all$Hobbies_CA_2[1]*df_all_train[train_sample,1] + dcoef_all$Hobbies_CA_2[2]*df_all_train[train_sample,2] + dcoef_all$Hobbies_CA_2[3]*df_all_train[train_sample,3] + dcoef_all$Hobbies_CA_2[4]*df_all_train[train_sample,4] + dcoef_all$Hobbies_CA_2[5]*df_all_train[train_sample,5] + dcoef_all$Hobbies_CA_2[6]*df_all_train[train_sample,6] + dcoef_all$Hobbies_CA_2[7]*df_all_train[train_sample,7] + dcoef_all$Hobbies_CA_2[8]*df_all_train[train_sample,8] + dcoef_all$Hobbies_CA_2[9]*df_all_train[train_sample,9] + dcoef_all$Hobbies_CA_2[10]*df_all_train[train_sample,10] + dcoef_all$Hobbies_CA_2[11]*df_all_train[train_sample,11] + dcoef_all$Hobbies_CA_2[12]*df_all_train[train_sample,12] + dcoef_all$Hobbies_CA_2[13]*df_all_train[train_sample,13] + dcoef_all$Hobbies_CA_2[14]*df_all_train[train_sample,14] + dcoef_all$Hobbies_CA_2[15]*df_all_train[train_sample,15] + dcoef_all$Hobbies_CA_2[16]*df_all_train[train_sample,16] + dcoef_all$Hobbies_CA_2[17]*df_all_train[train_sample,17] + dcoef_all$Hobbies_CA_2[18]*df_all_train[train_sample,18] + dcoef_all$Hobbies_CA_2[19]
    df_all_test[i, 26] = dcoef_all$Household_1_CA_2[1]*df_all_train[train_sample,1] + dcoef_all$Household_1_CA_2[2]*df_all_train[train_sample,2] + dcoef_all$Household_1_CA_2[3]*df_all_train[train_sample,3] + dcoef_all$Household_1_CA_2[4]*df_all_train[train_sample,4] + dcoef_all$Household_1_CA_2[5]*df_all_train[train_sample,5] + dcoef_all$Household_1_CA_2[6]*df_all_train[train_sample,6] + dcoef_all$Household_1_CA_2[7]*df_all_train[train_sample,7] + dcoef_all$Household_1_CA_2[8]*df_all_train[train_sample,8] + dcoef_all$Household_1_CA_2[9]*df_all_train[train_sample,9] + dcoef_all$Household_1_CA_2[10]*df_all_train[train_sample,10] + dcoef_all$Household_1_CA_2[11]*df_all_train[train_sample,11] + dcoef_all$Household_1_CA_2[12]*df_all_train[train_sample,12] + dcoef_all$Household_1_CA_2[13]*df_all_train[train_sample,13] + dcoef_all$Household_1_CA_2[14]*df_all_train[train_sample,14] + dcoef_all$Household_1_CA_2[15]*df_all_train[train_sample,15] + dcoef_all$Household_1_CA_2[16]*df_all_train[train_sample,16] + dcoef_all$Household_1_CA_2[17]*df_all_train[train_sample,17] + dcoef_all$Household_1_CA_2[18]*df_all_train[train_sample,18] + dcoef_all$Household_1_CA_2[19]
    df_all_test[i, 27] = dcoef_all$Household_2_CA_2[1]*df_all_train[train_sample,1] + dcoef_all$Household_2_CA_2[2]*df_all_train[train_sample,2] + dcoef_all$Household_2_CA_2[3]*df_all_train[train_sample,3] + dcoef_all$Household_2_CA_2[4]*df_all_train[train_sample,4] + dcoef_all$Household_2_CA_2[5]*df_all_train[train_sample,5] + dcoef_all$Household_2_CA_2[6]*df_all_train[train_sample,6] + dcoef_all$Household_2_CA_2[7]*df_all_train[train_sample,7] + dcoef_all$Household_2_CA_2[8]*df_all_train[train_sample,8] + dcoef_all$Household_2_CA_2[9]*df_all_train[train_sample,9] + dcoef_all$Household_2_CA_2[10]*df_all_train[train_sample,10] + dcoef_all$Household_2_CA_2[11]*df_all_train[train_sample,11] + dcoef_all$Household_2_CA_2[12]*df_all_train[train_sample,12] + dcoef_all$Household_2_CA_2[13]*df_all_train[train_sample,13] + dcoef_all$Household_2_CA_2[14]*df_all_train[train_sample,14] + dcoef_all$Household_2_CA_2[15]*df_all_train[train_sample,15] + dcoef_all$Household_2_CA_2[16]*df_all_train[train_sample,16] + dcoef_all$Household_2_CA_2[17]*df_all_train[train_sample,17] + dcoef_all$Household_2_CA_2[18]*df_all_train[train_sample,18] + dcoef_all$Household_2_CA_2[19]
    df_all_test[i, 28] = dcoef_all$Foods_1_CA_2[1]*df_all_train[train_sample,1] + dcoef_all$Foods_1_CA_2[2]*df_all_train[train_sample,2] + dcoef_all$Foods_1_CA_2[3]*df_all_train[train_sample,3] + dcoef_all$Foods_1_CA_2[4]*df_all_train[train_sample,4] + dcoef_all$Foods_1_CA_2[5]*df_all_train[train_sample,5] + dcoef_all$Foods_1_CA_2[6]*df_all_train[train_sample,6] + dcoef_all$Foods_1_CA_2[7]*df_all_train[train_sample,7] + dcoef_all$Foods_1_CA_2[8]*df_all_train[train_sample,8] + dcoef_all$Foods_1_CA_2[9]*df_all_train[train_sample,9] + dcoef_all$Foods_1_CA_2[10]*df_all_train[train_sample,10] + dcoef_all$Foods_1_CA_2[11]*df_all_train[train_sample,11] + dcoef_all$Foods_1_CA_2[12]*df_all_train[train_sample,12] + dcoef_all$Foods_1_CA_2[13]*df_all_train[train_sample,13] + dcoef_all$Foods_1_CA_2[14]*df_all_train[train_sample,14] + dcoef_all$Foods_1_CA_2[15]*df_all_train[train_sample,15] + dcoef_all$Foods_1_CA_2[16]*df_all_train[train_sample,16] + dcoef_all$Foods_1_CA_2[17]*df_all_train[train_sample,17] + dcoef_all$Foods_1_CA_2[18]*df_all_train[train_sample,18] + dcoef_all$Foods_1_CA_2[19]
    df_all_test[i, 29] = dcoef_all$Foods_2_CA_2[1]*df_all_train[train_sample,1] + dcoef_all$Foods_2_CA_2[2]*df_all_train[train_sample,2] + dcoef_all$Foods_2_CA_2[3]*df_all_train[train_sample,3] + dcoef_all$Foods_2_CA_2[4]*df_all_train[train_sample,4] + dcoef_all$Foods_2_CA_2[5]*df_all_train[train_sample,5] + dcoef_all$Foods_2_CA_2[6]*df_all_train[train_sample,6] + dcoef_all$Foods_2_CA_2[7]*df_all_train[train_sample,7] + dcoef_all$Foods_2_CA_2[8]*df_all_train[train_sample,8] + dcoef_all$Foods_2_CA_2[9]*df_all_train[train_sample,9] + dcoef_all$Foods_2_CA_2[10]*df_all_train[train_sample,10] + dcoef_all$Foods_2_CA_2[11]*df_all_train[train_sample,11] + dcoef_all$Foods_2_CA_2[12]*df_all_train[train_sample,12] + dcoef_all$Foods_2_CA_2[13]*df_all_train[train_sample,13] + dcoef_all$Foods_2_CA_2[14]*df_all_train[train_sample,14] + dcoef_all$Foods_2_CA_2[15]*df_all_train[train_sample,15] + dcoef_all$Foods_2_CA_2[16]*df_all_train[train_sample,16] + dcoef_all$Foods_2_CA_2[17]*df_all_train[train_sample,17] + dcoef_all$Foods_2_CA_2[18]*df_all_train[train_sample,18] + dcoef_all$Foods_2_CA_2[19]
    df_all_test[i, 30] = dcoef_all$Foods_3_CA_2[1]*df_all_train[train_sample,1] + dcoef_all$Foods_3_CA_2[2]*df_all_train[train_sample,2] + dcoef_all$Foods_3_CA_2[3]*df_all_train[train_sample,3] + dcoef_all$Foods_3_CA_2[4]*df_all_train[train_sample,4] + dcoef_all$Foods_3_CA_2[5]*df_all_train[train_sample,5] + dcoef_all$Foods_3_CA_2[6]*df_all_train[train_sample,6] + dcoef_all$Foods_3_CA_2[7]*df_all_train[train_sample,7] + dcoef_all$Foods_3_CA_2[8]*df_all_train[train_sample,8] + dcoef_all$Foods_3_CA_2[9]*df_all_train[train_sample,9] + dcoef_all$Foods_3_CA_2[10]*df_all_train[train_sample,10] + dcoef_all$Foods_3_CA_2[11]*df_all_train[train_sample,11] + dcoef_all$Foods_3_CA_2[12]*df_all_train[train_sample,12] + dcoef_all$Foods_3_CA_2[13]*df_all_train[train_sample,13] + dcoef_all$Foods_3_CA_2[14]*df_all_train[train_sample,14] + dcoef_all$Foods_3_CA_2[15]*df_all_train[train_sample,15] + dcoef_all$Foods_3_CA_2[16]*df_all_train[train_sample,16] + dcoef_all$Foods_3_CA_2[17]*df_all_train[train_sample,17] + dcoef_all$Foods_3_CA_2[18]*df_all_train[train_sample,18] + dcoef_all$Foods_3_CA_2[19]
    df_all_test[i, 31] = dcoef_all$Hobbies_CA_3[1]*df_all_train[train_sample,1] + dcoef_all$Hobbies_CA_3[2]*df_all_train[train_sample,2] + dcoef_all$Hobbies_CA_3[3]*df_all_train[train_sample,3] + dcoef_all$Hobbies_CA_3[4]*df_all_train[train_sample,4] + dcoef_all$Hobbies_CA_3[5]*df_all_train[train_sample,5] + dcoef_all$Hobbies_CA_3[6]*df_all_train[train_sample,6] + dcoef_all$Hobbies_CA_3[7]*df_all_train[train_sample,7] + dcoef_all$Hobbies_CA_3[8]*df_all_train[train_sample,8] + dcoef_all$Hobbies_CA_3[9]*df_all_train[train_sample,9] + dcoef_all$Hobbies_CA_3[10]*df_all_train[train_sample,10] + dcoef_all$Hobbies_CA_3[11]*df_all_train[train_sample,11] + dcoef_all$Hobbies_CA_3[12]*df_all_train[train_sample,12] + dcoef_all$Hobbies_CA_3[13]*df_all_train[train_sample,13] + dcoef_all$Hobbies_CA_3[14]*df_all_train[train_sample,14] + dcoef_all$Hobbies_CA_3[15]*df_all_train[train_sample,15] + dcoef_all$Hobbies_CA_3[16]*df_all_train[train_sample,16] + dcoef_all$Hobbies_CA_3[17]*df_all_train[train_sample,17] + dcoef_all$Hobbies_CA_3[18]*df_all_train[train_sample,18] + dcoef_all$Hobbies_CA_3[19]
    df_all_test[i, 32] = dcoef_all$Household_1_CA_3[1]*df_all_train[train_sample,1] + dcoef_all$Household_1_CA_3[2]*df_all_train[train_sample,2] + dcoef_all$Household_1_CA_3[3]*df_all_train[train_sample,3] + dcoef_all$Household_1_CA_3[4]*df_all_train[train_sample,4] + dcoef_all$Household_1_CA_3[5]*df_all_train[train_sample,5] + dcoef_all$Household_1_CA_3[6]*df_all_train[train_sample,6] + dcoef_all$Household_1_CA_3[7]*df_all_train[train_sample,7] + dcoef_all$Household_1_CA_3[8]*df_all_train[train_sample,8] + dcoef_all$Household_1_CA_3[9]*df_all_train[train_sample,9] + dcoef_all$Household_1_CA_3[10]*df_all_train[train_sample,10] + dcoef_all$Household_1_CA_3[11]*df_all_train[train_sample,11] + dcoef_all$Household_1_CA_3[12]*df_all_train[train_sample,12] + dcoef_all$Household_1_CA_3[13]*df_all_train[train_sample,13] + dcoef_all$Household_1_CA_3[14]*df_all_train[train_sample,14] + dcoef_all$Household_1_CA_3[15]*df_all_train[train_sample,15] + dcoef_all$Household_1_CA_3[16]*df_all_train[train_sample,16] + dcoef_all$Household_1_CA_3[17]*df_all_train[train_sample,17] + dcoef_all$Household_1_CA_3[18]*df_all_train[train_sample,18] + dcoef_all$Household_1_CA_3[19]
    df_all_test[i, 33] = dcoef_all$Household_2_CA_3[1]*df_all_train[train_sample,1] + dcoef_all$Household_2_CA_3[2]*df_all_train[train_sample,2] + dcoef_all$Household_2_CA_3[3]*df_all_train[train_sample,3] + dcoef_all$Household_2_CA_3[4]*df_all_train[train_sample,4] + dcoef_all$Household_2_CA_3[5]*df_all_train[train_sample,5] + dcoef_all$Household_2_CA_3[6]*df_all_train[train_sample,6] + dcoef_all$Household_2_CA_3[7]*df_all_train[train_sample,7] + dcoef_all$Household_2_CA_3[8]*df_all_train[train_sample,8] + dcoef_all$Household_2_CA_3[9]*df_all_train[train_sample,9] + dcoef_all$Household_2_CA_3[10]*df_all_train[train_sample,10] + dcoef_all$Household_2_CA_3[11]*df_all_train[train_sample,11] + dcoef_all$Household_2_CA_3[12]*df_all_train[train_sample,12] + dcoef_all$Household_2_CA_3[13]*df_all_train[train_sample,13] + dcoef_all$Household_2_CA_3[14]*df_all_train[train_sample,14] + dcoef_all$Household_2_CA_3[15]*df_all_train[train_sample,15] + dcoef_all$Household_2_CA_3[16]*df_all_train[train_sample,16] + dcoef_all$Household_2_CA_3[17]*df_all_train[train_sample,17] + dcoef_all$Household_2_CA_3[18]*df_all_train[train_sample,18] + dcoef_all$Household_2_CA_3[19]
    df_all_test[i, 34] = dcoef_all$Foods_1_CA_3[1]*df_all_train[train_sample,1] + dcoef_all$Foods_1_CA_3[2]*df_all_train[train_sample,2] + dcoef_all$Foods_1_CA_3[3]*df_all_train[train_sample,3] + dcoef_all$Foods_1_CA_3[4]*df_all_train[train_sample,4] + dcoef_all$Foods_1_CA_3[5]*df_all_train[train_sample,5] + dcoef_all$Foods_1_CA_3[6]*df_all_train[train_sample,6] + dcoef_all$Foods_1_CA_3[7]*df_all_train[train_sample,7] + dcoef_all$Foods_1_CA_3[8]*df_all_train[train_sample,8] + dcoef_all$Foods_1_CA_3[9]*df_all_train[train_sample,9] + dcoef_all$Foods_1_CA_3[10]*df_all_train[train_sample,10] + dcoef_all$Foods_1_CA_3[11]*df_all_train[train_sample,11] + dcoef_all$Foods_1_CA_3[12]*df_all_train[train_sample,12] + dcoef_all$Foods_1_CA_3[13]*df_all_train[train_sample,13] + dcoef_all$Foods_1_CA_3[14]*df_all_train[train_sample,14] + dcoef_all$Foods_1_CA_3[15]*df_all_train[train_sample,15] + dcoef_all$Foods_1_CA_3[16]*df_all_train[train_sample,16] + dcoef_all$Foods_1_CA_3[17]*df_all_train[train_sample,17] + dcoef_all$Foods_1_CA_3[18]*df_all_train[train_sample,18] + dcoef_all$Foods_1_CA_3[19]
    df_all_test[i, 35] = dcoef_all$Foods_2_CA_3[1]*df_all_train[train_sample,1] + dcoef_all$Foods_2_CA_3[2]*df_all_train[train_sample,2] + dcoef_all$Foods_2_CA_3[3]*df_all_train[train_sample,3] + dcoef_all$Foods_2_CA_3[4]*df_all_train[train_sample,4] + dcoef_all$Foods_2_CA_3[5]*df_all_train[train_sample,5] + dcoef_all$Foods_2_CA_3[6]*df_all_train[train_sample,6] + dcoef_all$Foods_2_CA_3[7]*df_all_train[train_sample,7] + dcoef_all$Foods_2_CA_3[8]*df_all_train[train_sample,8] + dcoef_all$Foods_2_CA_3[9]*df_all_train[train_sample,9] + dcoef_all$Foods_2_CA_3[10]*df_all_train[train_sample,10] + dcoef_all$Foods_2_CA_3[11]*df_all_train[train_sample,11] + dcoef_all$Foods_2_CA_3[12]*df_all_train[train_sample,12] + dcoef_all$Foods_2_CA_3[13]*df_all_train[train_sample,13] + dcoef_all$Foods_2_CA_3[14]*df_all_train[train_sample,14] + dcoef_all$Foods_2_CA_3[15]*df_all_train[train_sample,15] + dcoef_all$Foods_2_CA_3[16]*df_all_train[train_sample,16] + dcoef_all$Foods_2_CA_3[17]*df_all_train[train_sample,17] + dcoef_all$Foods_2_CA_3[18]*df_all_train[train_sample,18] + dcoef_all$Foods_2_CA_3[19]
    df_all_test[i, 36] = dcoef_all$Foods_3_CA_3[1]*df_all_train[train_sample,1] + dcoef_all$Foods_3_CA_3[2]*df_all_train[train_sample,2] + dcoef_all$Foods_3_CA_3[3]*df_all_train[train_sample,3] + dcoef_all$Foods_3_CA_3[4]*df_all_train[train_sample,4] + dcoef_all$Foods_3_CA_3[5]*df_all_train[train_sample,5] + dcoef_all$Foods_3_CA_3[6]*df_all_train[train_sample,6] + dcoef_all$Foods_3_CA_3[7]*df_all_train[train_sample,7] + dcoef_all$Foods_3_CA_3[8]*df_all_train[train_sample,8] + dcoef_all$Foods_3_CA_3[9]*df_all_train[train_sample,9] + dcoef_all$Foods_3_CA_3[10]*df_all_train[train_sample,10] + dcoef_all$Foods_3_CA_3[11]*df_all_train[train_sample,11] + dcoef_all$Foods_3_CA_3[12]*df_all_train[train_sample,12] + dcoef_all$Foods_3_CA_3[13]*df_all_train[train_sample,13] + dcoef_all$Foods_3_CA_3[14]*df_all_train[train_sample,14] + dcoef_all$Foods_3_CA_3[15]*df_all_train[train_sample,15] + dcoef_all$Foods_3_CA_3[16]*df_all_train[train_sample,16] + dcoef_all$Foods_3_CA_3[17]*df_all_train[train_sample,17] + dcoef_all$Foods_3_CA_3[18]*df_all_train[train_sample,18] + dcoef_all$Foods_3_CA_3[19]
    
  }
  else {
    df_all_test[i, 19] = dcoef_all$Hobbies_CA_1[1]*df_all_test[i-1,1] + dcoef_all$Hobbies_CA_1[2]*df_all_test[i-1,2] + dcoef_all$Hobbies_CA_1[3]*df_all_test[i-1,3] + dcoef_all$Hobbies_CA_1[4]*df_all_test[i-1,4] + dcoef_all$Hobbies_CA_1[5]*df_all_test[i-1,5] + dcoef_all$Hobbies_CA_1[6]*df_all_test[i-1,6] + dcoef_all$Hobbies_CA_1[7]*df_all_test[i-1,7] + dcoef_all$Hobbies_CA_1[8]*df_all_test[i-1,8] + dcoef_all$Hobbies_CA_1[9]*df_all_test[i-1,9] + dcoef_all$Hobbies_CA_1[10]*df_all_test[i-1,10] + dcoef_all$Hobbies_CA_1[11]*df_all_test[i-1,11] + dcoef_all$Hobbies_CA_1[12]*df_all_test[i-1,12] + dcoef_all$Hobbies_CA_1[13]*df_all_test[i-1,13] + dcoef_all$Hobbies_CA_1[14]*df_all_test[i-1,14] + dcoef_all$Hobbies_CA_1[15]*df_all_test[i-1,15] + dcoef_all$Hobbies_CA_1[16]*df_all_test[i-1,16] + dcoef_all$Hobbies_CA_1[17]*df_all_test[i-1,17] + dcoef_all$Hobbies_CA_1[18]*df_all_test[i-1,18] + dcoef_all$Hobbies_CA_1[19]
    df_all_test[i, 20] = dcoef_all$Household_1_CA_1[1]*df_all_test[i-1,1] + dcoef_all$Household_1_CA_1[2]*df_all_test[i-1,2] + dcoef_all$Household_1_CA_1[3]*df_all_test[i-1,3] + dcoef_all$Household_1_CA_1[4]*df_all_test[i-1,4] + dcoef_all$Household_1_CA_1[5]*df_all_test[i-1,5] + dcoef_all$Household_1_CA_1[6]*df_all_test[i-1,6] + dcoef_all$Household_1_CA_1[7]*df_all_test[i-1,7] + dcoef_all$Household_1_CA_1[8]*df_all_test[i-1,8] + dcoef_all$Household_1_CA_1[9]*df_all_test[i-1,9] + dcoef_all$Household_1_CA_1[10]*df_all_test[i-1,10] + dcoef_all$Household_1_CA_1[11]*df_all_test[i-1,11] + dcoef_all$Household_1_CA_1[12]*df_all_test[i-1,12] + dcoef_all$Household_1_CA_1[13]*df_all_test[i-1,13] + dcoef_all$Household_1_CA_1[14]*df_all_test[i-1,14] + dcoef_all$Household_1_CA_1[15]*df_all_test[i-1,15] + dcoef_all$Household_1_CA_1[16]*df_all_test[i-1,16] + dcoef_all$Household_1_CA_1[17]*df_all_test[i-1,17] + dcoef_all$Household_1_CA_1[18]*df_all_test[i-1,18] + dcoef_all$Household_1_CA_1[19]
    df_all_test[i, 21] = dcoef_all$Household_2_CA_1[1]*df_all_test[i-1,1] + dcoef_all$Household_2_CA_1[2]*df_all_test[i-1,2] + dcoef_all$Household_2_CA_1[3]*df_all_test[i-1,3] + dcoef_all$Household_2_CA_1[4]*df_all_test[i-1,4] + dcoef_all$Household_2_CA_1[5]*df_all_test[i-1,5] + dcoef_all$Household_2_CA_1[6]*df_all_test[i-1,6] + dcoef_all$Household_2_CA_1[7]*df_all_test[i-1,7] + dcoef_all$Household_2_CA_1[8]*df_all_test[i-1,8] + dcoef_all$Household_2_CA_1[9]*df_all_test[i-1,9] + dcoef_all$Household_2_CA_1[10]*df_all_test[i-1,10] + dcoef_all$Household_2_CA_1[11]*df_all_test[i-1,11] + dcoef_all$Household_2_CA_1[12]*df_all_test[i-1,12] + dcoef_all$Household_2_CA_1[13]*df_all_test[i-1,13] + dcoef_all$Household_2_CA_1[14]*df_all_test[i-1,14] + dcoef_all$Household_2_CA_1[15]*df_all_test[i-1,15] + dcoef_all$Household_2_CA_1[16]*df_all_test[i-1,16] + dcoef_all$Household_2_CA_1[17]*df_all_test[i-1,17] + dcoef_all$Household_2_CA_1[18]*df_all_test[i-1,18] + dcoef_all$Household_2_CA_1[19]
    df_all_test[i, 22] = dcoef_all$Foods_1_CA_1[1]*df_all_test[i-1,1] + dcoef_all$Foods_1_CA_1[2]*df_all_test[i-1,2] + dcoef_all$Foods_1_CA_1[3]*df_all_test[i-1,3] + dcoef_all$Foods_1_CA_1[4]*df_all_test[i-1,4] + dcoef_all$Foods_1_CA_1[5]*df_all_test[i-1,5] + dcoef_all$Foods_1_CA_1[6]*df_all_test[i-1,6] + dcoef_all$Foods_1_CA_1[7]*df_all_test[i-1,7] + dcoef_all$Foods_1_CA_1[8]*df_all_test[i-1,8] + dcoef_all$Foods_1_CA_1[9]*df_all_test[i-1,9] + dcoef_all$Foods_1_CA_1[10]*df_all_test[i-1,10] + dcoef_all$Foods_1_CA_1[11]*df_all_test[i-1,11] + dcoef_all$Foods_1_CA_1[12]*df_all_test[i-1,12] + dcoef_all$Foods_1_CA_1[13]*df_all_test[i-1,13] + dcoef_all$Foods_1_CA_1[14]*df_all_test[i-1,14] + dcoef_all$Foods_1_CA_1[15]*df_all_test[i-1,15] + dcoef_all$Foods_1_CA_1[16]*df_all_test[i-1,16] + dcoef_all$Foods_1_CA_1[17]*df_all_test[i-1,17] + dcoef_all$Foods_1_CA_1[18]*df_all_test[i-1,18] + dcoef_all$Foods_1_CA_1[19]
    df_all_test[i, 23] = dcoef_all$Foods_2_CA_1[1]*df_all_test[i-1,1] + dcoef_all$Foods_2_CA_1[2]*df_all_test[i-1,2] + dcoef_all$Foods_2_CA_1[3]*df_all_test[i-1,3] + dcoef_all$Foods_2_CA_1[4]*df_all_test[i-1,4] + dcoef_all$Foods_2_CA_1[5]*df_all_test[i-1,5] + dcoef_all$Foods_2_CA_1[6]*df_all_test[i-1,6] + dcoef_all$Foods_2_CA_1[7]*df_all_test[i-1,7] + dcoef_all$Foods_2_CA_1[8]*df_all_test[i-1,8] + dcoef_all$Foods_2_CA_1[9]*df_all_test[i-1,9] + dcoef_all$Foods_2_CA_1[10]*df_all_test[i-1,10] + dcoef_all$Foods_2_CA_1[11]*df_all_test[i-1,11] + dcoef_all$Foods_2_CA_1[12]*df_all_test[i-1,12] + dcoef_all$Foods_2_CA_1[13]*df_all_test[i-1,13] + dcoef_all$Foods_2_CA_1[14]*df_all_test[i-1,14] + dcoef_all$Foods_2_CA_1[15]*df_all_test[i-1,15] + dcoef_all$Foods_2_CA_1[16]*df_all_test[i-1,16] + dcoef_all$Foods_2_CA_1[17]*df_all_test[i-1,17] + dcoef_all$Foods_2_CA_1[18]*df_all_test[i-1,18] + dcoef_all$Foods_2_CA_1[19]
    df_all_test[i, 24] = dcoef_all$Foods_3_CA_1[1]*df_all_test[i-1,1] + dcoef_all$Foods_3_CA_1[2]*df_all_test[i-1,2] + dcoef_all$Foods_3_CA_1[3]*df_all_test[i-1,3] + dcoef_all$Foods_3_CA_1[4]*df_all_test[i-1,4] + dcoef_all$Foods_3_CA_1[5]*df_all_test[i-1,5] + dcoef_all$Foods_3_CA_1[6]*df_all_test[i-1,6] + dcoef_all$Foods_3_CA_1[7]*df_all_test[i-1,7] + dcoef_all$Foods_3_CA_1[8]*df_all_test[i-1,8] + dcoef_all$Foods_3_CA_1[9]*df_all_test[i-1,9] + dcoef_all$Foods_3_CA_1[10]*df_all_test[i-1,10] + dcoef_all$Foods_3_CA_1[11]*df_all_test[i-1,11] + dcoef_all$Foods_3_CA_1[12]*df_all_test[i-1,12] + dcoef_all$Foods_3_CA_1[13]*df_all_test[i-1,13] + dcoef_all$Foods_3_CA_1[14]*df_all_test[i-1,14] + dcoef_all$Foods_3_CA_1[15]*df_all_test[i-1,15] + dcoef_all$Foods_3_CA_1[16]*df_all_test[i-1,16] + dcoef_all$Foods_3_CA_1[17]*df_all_test[i-1,17] + dcoef_all$Foods_3_CA_1[18]*df_all_test[i-1,18] + dcoef_all$Foods_3_CA_1[19]
    df_all_test[i, 25] = dcoef_all$Hobbies_CA_2[1]*df_all_test[i-1,1] + dcoef_all$Hobbies_CA_2[2]*df_all_test[i-1,2] + dcoef_all$Hobbies_CA_2[3]*df_all_test[i-1,3] + dcoef_all$Hobbies_CA_2[4]*df_all_test[i-1,4] + dcoef_all$Hobbies_CA_2[5]*df_all_test[i-1,5] + dcoef_all$Hobbies_CA_2[6]*df_all_test[i-1,6] + dcoef_all$Hobbies_CA_2[7]*df_all_test[i-1,7] + dcoef_all$Hobbies_CA_2[8]*df_all_test[i-1,8] + dcoef_all$Hobbies_CA_2[9]*df_all_test[i-1,9] + dcoef_all$Hobbies_CA_2[10]*df_all_test[i-1,10] + dcoef_all$Hobbies_CA_2[11]*df_all_test[i-1,11] + dcoef_all$Hobbies_CA_2[12]*df_all_test[i-1,12] + dcoef_all$Hobbies_CA_2[13]*df_all_test[i-1,13] + dcoef_all$Hobbies_CA_2[14]*df_all_test[i-1,14] + dcoef_all$Hobbies_CA_2[15]*df_all_test[i-1,15] + dcoef_all$Hobbies_CA_2[16]*df_all_test[i-1,16] + dcoef_all$Hobbies_CA_2[17]*df_all_test[i-1,17] + dcoef_all$Hobbies_CA_2[18]*df_all_test[i-1,18] + dcoef_all$Hobbies_CA_2[19]
    df_all_test[i, 26] = dcoef_all$Household_1_CA_2[1]*df_all_test[i-1,1] + dcoef_all$Household_1_CA_2[2]*df_all_test[i-1,2] + dcoef_all$Household_1_CA_2[3]*df_all_test[i-1,3] + dcoef_all$Household_1_CA_2[4]*df_all_test[i-1,4] + dcoef_all$Household_1_CA_2[5]*df_all_test[i-1,5] + dcoef_all$Household_1_CA_2[6]*df_all_test[i-1,6] + dcoef_all$Household_1_CA_2[7]*df_all_test[i-1,7] + dcoef_all$Household_1_CA_2[8]*df_all_test[i-1,8] + dcoef_all$Household_1_CA_2[9]*df_all_test[i-1,9] + dcoef_all$Household_1_CA_2[10]*df_all_test[i-1,10] + dcoef_all$Household_1_CA_2[11]*df_all_test[i-1,11] + dcoef_all$Household_1_CA_2[12]*df_all_test[i-1,12] + dcoef_all$Household_1_CA_2[13]*df_all_test[i-1,13] + dcoef_all$Household_1_CA_2[14]*df_all_test[i-1,14] + dcoef_all$Household_1_CA_2[15]*df_all_test[i-1,15] + dcoef_all$Household_1_CA_2[16]*df_all_test[i-1,16] + dcoef_all$Household_1_CA_2[17]*df_all_test[i-1,17] + dcoef_all$Household_1_CA_2[18]*df_all_test[i-1,18] + dcoef_all$Household_1_CA_2[19]
    df_all_test[i, 27] = dcoef_all$Household_2_CA_2[1]*df_all_test[i-1,1] + dcoef_all$Household_2_CA_2[2]*df_all_test[i-1,2] + dcoef_all$Household_2_CA_2[3]*df_all_test[i-1,3] + dcoef_all$Household_2_CA_2[4]*df_all_test[i-1,4] + dcoef_all$Household_2_CA_2[5]*df_all_test[i-1,5] + dcoef_all$Household_2_CA_2[6]*df_all_test[i-1,6] + dcoef_all$Household_2_CA_2[7]*df_all_test[i-1,7] + dcoef_all$Household_2_CA_2[8]*df_all_test[i-1,8] + dcoef_all$Household_2_CA_2[9]*df_all_test[i-1,9] + dcoef_all$Household_2_CA_2[10]*df_all_test[i-1,10] + dcoef_all$Household_2_CA_2[11]*df_all_test[i-1,11] + dcoef_all$Household_2_CA_2[12]*df_all_test[i-1,12] + dcoef_all$Household_2_CA_2[13]*df_all_test[i-1,13] + dcoef_all$Household_2_CA_2[14]*df_all_test[i-1,14] + dcoef_all$Household_2_CA_2[15]*df_all_test[i-1,15] + dcoef_all$Household_2_CA_2[16]*df_all_test[i-1,16] + dcoef_all$Household_2_CA_2[17]*df_all_test[i-1,17] + dcoef_all$Household_2_CA_2[18]*df_all_test[i-1,18] + dcoef_all$Household_2_CA_2[19]
    df_all_test[i, 28] = dcoef_all$Foods_1_CA_2[1]*df_all_test[i-1,1] + dcoef_all$Foods_1_CA_2[2]*df_all_test[i-1,2] + dcoef_all$Foods_1_CA_2[3]*df_all_test[i-1,3] + dcoef_all$Foods_1_CA_2[4]*df_all_test[i-1,4] + dcoef_all$Foods_1_CA_2[5]*df_all_test[i-1,5] + dcoef_all$Foods_1_CA_2[6]*df_all_test[i-1,6] + dcoef_all$Foods_1_CA_2[7]*df_all_test[i-1,7] + dcoef_all$Foods_1_CA_2[8]*df_all_test[i-1,8] + dcoef_all$Foods_1_CA_2[9]*df_all_test[i-1,9] + dcoef_all$Foods_1_CA_2[10]*df_all_test[i-1,10] + dcoef_all$Foods_1_CA_2[11]*df_all_test[i-1,11] + dcoef_all$Foods_1_CA_2[12]*df_all_test[i-1,12] + dcoef_all$Foods_1_CA_2[13]*df_all_test[i-1,13] + dcoef_all$Foods_1_CA_2[14]*df_all_test[i-1,14] + dcoef_all$Foods_1_CA_2[15]*df_all_test[i-1,15] + dcoef_all$Foods_1_CA_2[16]*df_all_test[i-1,16] + dcoef_all$Foods_1_CA_2[17]*df_all_test[i-1,17] + dcoef_all$Foods_1_CA_2[18]*df_all_test[i-1,18] + dcoef_all$Foods_1_CA_2[19]
    df_all_test[i, 29] = dcoef_all$Foods_2_CA_2[1]*df_all_test[i-1,1] + dcoef_all$Foods_2_CA_2[2]*df_all_test[i-1,2] + dcoef_all$Foods_2_CA_2[3]*df_all_test[i-1,3] + dcoef_all$Foods_2_CA_2[4]*df_all_test[i-1,4] + dcoef_all$Foods_2_CA_2[5]*df_all_test[i-1,5] + dcoef_all$Foods_2_CA_2[6]*df_all_test[i-1,6] + dcoef_all$Foods_2_CA_2[7]*df_all_test[i-1,7] + dcoef_all$Foods_2_CA_2[8]*df_all_test[i-1,8] + dcoef_all$Foods_2_CA_2[9]*df_all_test[i-1,9] + dcoef_all$Foods_2_CA_2[10]*df_all_test[i-1,10] + dcoef_all$Foods_2_CA_2[11]*df_all_test[i-1,11] + dcoef_all$Foods_2_CA_2[12]*df_all_test[i-1,12] + dcoef_all$Foods_2_CA_2[13]*df_all_test[i-1,13] + dcoef_all$Foods_2_CA_2[14]*df_all_test[i-1,14] + dcoef_all$Foods_2_CA_2[15]*df_all_test[i-1,15] + dcoef_all$Foods_2_CA_2[16]*df_all_test[i-1,16] + dcoef_all$Foods_2_CA_2[17]*df_all_test[i-1,17] + dcoef_all$Foods_2_CA_2[18]*df_all_test[i-1,18] + dcoef_all$Foods_2_CA_2[19]
    df_all_test[i, 30] = dcoef_all$Foods_3_CA_2[1]*df_all_test[i-1,1] + dcoef_all$Foods_3_CA_2[2]*df_all_test[i-1,2] + dcoef_all$Foods_3_CA_2[3]*df_all_test[i-1,3] + dcoef_all$Foods_3_CA_2[4]*df_all_test[i-1,4] + dcoef_all$Foods_3_CA_2[5]*df_all_test[i-1,5] + dcoef_all$Foods_3_CA_2[6]*df_all_test[i-1,6] + dcoef_all$Foods_3_CA_2[7]*df_all_test[i-1,7] + dcoef_all$Foods_3_CA_2[8]*df_all_test[i-1,8] + dcoef_all$Foods_3_CA_2[9]*df_all_test[i-1,9] + dcoef_all$Foods_3_CA_2[10]*df_all_test[i-1,10] + dcoef_all$Foods_3_CA_2[11]*df_all_test[i-1,11] + dcoef_all$Foods_3_CA_2[12]*df_all_test[i-1,12] + dcoef_all$Foods_3_CA_2[13]*df_all_test[i-1,13] + dcoef_all$Foods_3_CA_2[14]*df_all_test[i-1,14] + dcoef_all$Foods_3_CA_2[15]*df_all_test[i-1,15] + dcoef_all$Foods_3_CA_2[16]*df_all_test[i-1,16] + dcoef_all$Foods_3_CA_2[17]*df_all_test[i-1,17] + dcoef_all$Foods_3_CA_2[18]*df_all_test[i-1,18] + dcoef_all$Foods_3_CA_2[19]
    df_all_test[i, 31] = dcoef_all$Hobbies_CA_3[1]*df_all_test[i-1,1] + dcoef_all$Hobbies_CA_3[2]*df_all_test[i-1,2] + dcoef_all$Hobbies_CA_3[3]*df_all_test[i-1,3] + dcoef_all$Hobbies_CA_3[4]*df_all_test[i-1,4] + dcoef_all$Hobbies_CA_3[5]*df_all_test[i-1,5] + dcoef_all$Hobbies_CA_3[6]*df_all_test[i-1,6] + dcoef_all$Hobbies_CA_3[7]*df_all_test[i-1,7] + dcoef_all$Hobbies_CA_3[8]*df_all_test[i-1,8] + dcoef_all$Hobbies_CA_3[9]*df_all_test[i-1,9] + dcoef_all$Hobbies_CA_3[10]*df_all_test[i-1,10] + dcoef_all$Hobbies_CA_3[11]*df_all_test[i-1,11] + dcoef_all$Hobbies_CA_3[12]*df_all_test[i-1,12] + dcoef_all$Hobbies_CA_3[13]*df_all_test[i-1,13] + dcoef_all$Hobbies_CA_3[14]*df_all_test[i-1,14] + dcoef_all$Hobbies_CA_3[15]*df_all_test[i-1,15] + dcoef_all$Hobbies_CA_3[16]*df_all_test[i-1,16] + dcoef_all$Hobbies_CA_3[17]*df_all_test[i-1,17] + dcoef_all$Hobbies_CA_3[18]*df_all_test[i-1,18] + dcoef_all$Hobbies_CA_3[19]
    df_all_test[i, 32] = dcoef_all$Household_1_CA_3[1]*df_all_test[i-1,1] + dcoef_all$Household_1_CA_3[2]*df_all_test[i-1,2] + dcoef_all$Household_1_CA_3[3]*df_all_test[i-1,3] + dcoef_all$Household_1_CA_3[4]*df_all_test[i-1,4] + dcoef_all$Household_1_CA_3[5]*df_all_test[i-1,5] + dcoef_all$Household_1_CA_3[6]*df_all_test[i-1,6] + dcoef_all$Household_1_CA_3[7]*df_all_test[i-1,7] + dcoef_all$Household_1_CA_3[8]*df_all_test[i-1,8] + dcoef_all$Household_1_CA_3[9]*df_all_test[i-1,9] + dcoef_all$Household_1_CA_3[10]*df_all_test[i-1,10] + dcoef_all$Household_1_CA_3[11]*df_all_test[i-1,11] + dcoef_all$Household_1_CA_3[12]*df_all_test[i-1,12] + dcoef_all$Household_1_CA_3[13]*df_all_test[i-1,13] + dcoef_all$Household_1_CA_3[14]*df_all_test[i-1,14] + dcoef_all$Household_1_CA_3[15]*df_all_test[i-1,15] + dcoef_all$Household_1_CA_3[16]*df_all_test[i-1,16] + dcoef_all$Household_1_CA_3[17]*df_all_test[i-1,17] + dcoef_all$Household_1_CA_3[18]*df_all_test[i-1,18] + dcoef_all$Household_1_CA_3[19]
    df_all_test[i, 33] = dcoef_all$Household_2_CA_3[1]*df_all_test[i-1,1] + dcoef_all$Household_2_CA_3[2]*df_all_test[i-1,2] + dcoef_all$Household_2_CA_3[3]*df_all_test[i-1,3] + dcoef_all$Household_2_CA_3[4]*df_all_test[i-1,4] + dcoef_all$Household_2_CA_3[5]*df_all_test[i-1,5] + dcoef_all$Household_2_CA_3[6]*df_all_test[i-1,6] + dcoef_all$Household_2_CA_3[7]*df_all_test[i-1,7] + dcoef_all$Household_2_CA_3[8]*df_all_test[i-1,8] + dcoef_all$Household_2_CA_3[9]*df_all_test[i-1,9] + dcoef_all$Household_2_CA_3[10]*df_all_test[i-1,10] + dcoef_all$Household_2_CA_3[11]*df_all_test[i-1,11] + dcoef_all$Household_2_CA_3[12]*df_all_test[i-1,12] + dcoef_all$Household_2_CA_3[13]*df_all_test[i-1,13] + dcoef_all$Household_2_CA_3[14]*df_all_test[i-1,14] + dcoef_all$Household_2_CA_3[15]*df_all_test[i-1,15] + dcoef_all$Household_2_CA_3[16]*df_all_test[i-1,16] + dcoef_all$Household_2_CA_3[17]*df_all_test[i-1,17] + dcoef_all$Household_2_CA_3[18]*df_all_test[i-1,18] + dcoef_all$Household_2_CA_3[19]
    df_all_test[i, 34] = dcoef_all$Foods_1_CA_3[1]*df_all_test[i-1,1] + dcoef_all$Foods_1_CA_3[2]*df_all_test[i-1,2] + dcoef_all$Foods_1_CA_3[3]*df_all_test[i-1,3] + dcoef_all$Foods_1_CA_3[4]*df_all_test[i-1,4] + dcoef_all$Foods_1_CA_3[5]*df_all_test[i-1,5] + dcoef_all$Foods_1_CA_3[6]*df_all_test[i-1,6] + dcoef_all$Foods_1_CA_3[7]*df_all_test[i-1,7] + dcoef_all$Foods_1_CA_3[8]*df_all_test[i-1,8] + dcoef_all$Foods_1_CA_3[9]*df_all_test[i-1,9] + dcoef_all$Foods_1_CA_3[10]*df_all_test[i-1,10] + dcoef_all$Foods_1_CA_3[11]*df_all_test[i-1,11] + dcoef_all$Foods_1_CA_3[12]*df_all_test[i-1,12] + dcoef_all$Foods_1_CA_3[13]*df_all_test[i-1,13] + dcoef_all$Foods_1_CA_3[14]*df_all_test[i-1,14] + dcoef_all$Foods_1_CA_3[15]*df_all_test[i-1,15] + dcoef_all$Foods_1_CA_3[16]*df_all_test[i-1,16] + dcoef_all$Foods_1_CA_3[17]*df_all_test[i-1,17] + dcoef_all$Foods_1_CA_3[18]*df_all_test[i-1,18] + dcoef_all$Foods_1_CA_3[19]
    df_all_test[i, 35] = dcoef_all$Foods_2_CA_3[1]*df_all_test[i-1,1] + dcoef_all$Foods_2_CA_3[2]*df_all_test[i-1,2] + dcoef_all$Foods_2_CA_3[3]*df_all_test[i-1,3] + dcoef_all$Foods_2_CA_3[4]*df_all_test[i-1,4] + dcoef_all$Foods_2_CA_3[5]*df_all_test[i-1,5] + dcoef_all$Foods_2_CA_3[6]*df_all_test[i-1,6] + dcoef_all$Foods_2_CA_3[7]*df_all_test[i-1,7] + dcoef_all$Foods_2_CA_3[8]*df_all_test[i-1,8] + dcoef_all$Foods_2_CA_3[9]*df_all_test[i-1,9] + dcoef_all$Foods_2_CA_3[10]*df_all_test[i-1,10] + dcoef_all$Foods_2_CA_3[11]*df_all_test[i-1,11] + dcoef_all$Foods_2_CA_3[12]*df_all_test[i-1,12] + dcoef_all$Foods_2_CA_3[13]*df_all_test[i-1,13] + dcoef_all$Foods_2_CA_3[14]*df_all_test[i-1,14] + dcoef_all$Foods_2_CA_3[15]*df_all_test[i-1,15] + dcoef_all$Foods_2_CA_3[16]*df_all_test[i-1,16] + dcoef_all$Foods_2_CA_3[17]*df_all_test[i-1,17] + dcoef_all$Foods_2_CA_3[18]*df_all_test[i-1,18] + dcoef_all$Foods_2_CA_3[19]
    df_all_test[i, 36] = dcoef_all$Foods_3_CA_3[1]*df_all_test[i-1,1] + dcoef_all$Foods_3_CA_3[2]*df_all_test[i-1,2] + dcoef_all$Foods_3_CA_3[3]*df_all_test[i-1,3] + dcoef_all$Foods_3_CA_3[4]*df_all_test[i-1,4] + dcoef_all$Foods_3_CA_3[5]*df_all_test[i-1,5] + dcoef_all$Foods_3_CA_3[6]*df_all_test[i-1,6] + dcoef_all$Foods_3_CA_3[7]*df_all_test[i-1,7] + dcoef_all$Foods_3_CA_3[8]*df_all_test[i-1,8] + dcoef_all$Foods_3_CA_3[9]*df_all_test[i-1,9] + dcoef_all$Foods_3_CA_3[10]*df_all_test[i-1,10] + dcoef_all$Foods_3_CA_3[11]*df_all_test[i-1,11] + dcoef_all$Foods_3_CA_3[12]*df_all_test[i-1,12] + dcoef_all$Foods_3_CA_3[13]*df_all_test[i-1,13] + dcoef_all$Foods_3_CA_3[14]*df_all_test[i-1,14] + dcoef_all$Foods_3_CA_3[15]*df_all_test[i-1,15] + dcoef_all$Foods_3_CA_3[16]*df_all_test[i-1,16] + dcoef_all$Foods_3_CA_3[17]*df_all_test[i-1,17] + dcoef_all$Foods_3_CA_3[18]*df_all_test[i-1,18] + dcoef_all$Foods_3_CA_3[19]
        
  }
}

colnames(df_all_test) = c('Hobbies_CA_1','Household_1_CA_1','Household_2_CA_1','Foods_1_CA_1','Foods_2_CA_1','Foods_3_CA_1'
                          ,'Hobbies_CA_2','Household_1_CA_2','Household_2_CA_2','Foods_1_CA_2','Foods_2_CA_2','Foods_3_CA_2'
                          ,'Hobbies_CA_3','Household_1_CA_3','Household_2_CA_3','Foods_1_CA_3','Foods_2_CA_3','Foods_3_CA_3'
                          ,'Hobbies_CA_1_pred','Household_1_CA_1_pred','Household_2_CA_1_pred','Foods_1_CA_1_pred','Foods_2_CA_1_pred','Foods_3_CA_1_pred'
                          ,'Hobbies_CA_2_pred','Household_1_CA_2_pred','Household_2_CA_2_pred','Foods_1_CA_2_pred','Foods_2_CA_2_pred','Foods_3_CA_2_pred'
                          ,'Hobbies_CA_3_pred','Household_1_CA_3_pred','Household_2_CA_3_pred','Foods_1_CA_3_pred','Foods_2_CA_3_pred','Foods_3_CA_3_pred'
                          )


forecasts_all_23 = df_all_test

length_x = test_sample-train_sample
x = 1:length_x


# Plot hobbies real vs predicted forecasts for each store

forecasts_all_23$Hobbies_CA_1_High = forecasts_all_23$Hobbies_CA_1_pred + 1.96 * (dcoef_all$Hobbies_CA_1[1,2] + dcoef_all$Hobbies_CA_1[2,2] + dcoef_all$Hobbies_CA_1[3,2] + dcoef_all$Hobbies_CA_1[4,2] + dcoef_all$Hobbies_CA_1[5,2] + dcoef_all$Hobbies_CA_1[6,2] + dcoef_all$Hobbies_CA_1[7,2] + dcoef_all$Hobbies_CA_1[8,2] + dcoef_all$Hobbies_CA_1[9,2] + dcoef_all$Hobbies_CA_1[10,2] + dcoef_all$Hobbies_CA_1[11,2] + dcoef_all$Hobbies_CA_1[12,2] + dcoef_all$Hobbies_CA_1[13,2] + dcoef_all$Hobbies_CA_1[14,2] + dcoef_all$Hobbies_CA_1[14,2] + dcoef_all$Hobbies_CA_1[16,2] + dcoef_all$Hobbies_CA_1[17,2] + dcoef_all$Hobbies_CA_1[18,2] + dcoef_all$Hobbies_CA_1[19,2])

forecasts_all_23$Hobbies_CA_1_High = forecasts_all_23$Hobbies_CA_1_pred - 1.96 * (dcoef_all$Hobbies_CA_1[1,2] + dcoef_all$Hobbies_CA_1[2,2] + dcoef_all$Hobbies_CA_1[3,2] + dcoef_all$Hobbies_CA_1[4,2] + dcoef_all$Hobbies_CA_1[5,2] + dcoef_all$Hobbies_CA_1[6,2] + dcoef_all$Hobbies_CA_1[7,2] + dcoef_all$Hobbies_CA_1[8,2] + dcoef_all$Hobbies_CA_1[9,2] + dcoef_all$Hobbies_CA_1[10,2] + dcoef_all$Hobbies_CA_1[11,2] + dcoef_all$Hobbies_CA_1[12,2] + dcoef_all$Hobbies_CA_1[13,2] + dcoef_all$Hobbies_CA_1[14,2] + dcoef_all$Hobbies_CA_1[14,2] + dcoef_all$Hobbies_CA_1[16,2] + dcoef_all$Hobbies_CA_1[17,2] + dcoef_all$Hobbies_CA_1[18,2] + dcoef_all$Hobbies_CA_1[19,2])

forecasts_all_23$Hobbies_CA_2_High = forecasts_all_23$Hobbies_CA_2_pred + 1.96 * (dcoef_all$Hobbies_CA_2[1,2] + dcoef_all$Hobbies_CA_2[2,2] + dcoef_all$Hobbies_CA_2[3,2] + dcoef_all$Hobbies_CA_2[4,2] + dcoef_all$Hobbies_CA_2[5,2] + dcoef_all$Hobbies_CA_2[6,2] + dcoef_all$Hobbies_CA_2[7,2] + dcoef_all$Hobbies_CA_2[8,2] + dcoef_all$Hobbies_CA_2[9,2] + dcoef_all$Hobbies_CA_2[10,2] + dcoef_all$Hobbies_CA_2[11,2] + dcoef_all$Hobbies_CA_2[12,2] + dcoef_all$Hobbies_CA_2[13,2] + dcoef_all$Hobbies_CA_2[14,2] + dcoef_all$Hobbies_CA_2[14,2] + dcoef_all$Hobbies_CA_2[16,2] + dcoef_all$Hobbies_CA_2[17,2] + dcoef_all$Hobbies_CA_2[18,2] + dcoef_all$Hobbies_CA_2[19,2])

forecasts_all_23$Hobbies_CA_2_High = forecasts_all_23$Hobbies_CA_2_pred - 1.96 * (dcoef_all$Hobbies_CA_2[1,2] + dcoef_all$Hobbies_CA_2[2,2] + dcoef_all$Hobbies_CA_2[3,2] + dcoef_all$Hobbies_CA_2[4,2] + dcoef_all$Hobbies_CA_2[5,2] + dcoef_all$Hobbies_CA_2[6,2] + dcoef_all$Hobbies_CA_2[7,2] + dcoef_all$Hobbies_CA_2[8,2] + dcoef_all$Hobbies_CA_2[9,2] + dcoef_all$Hobbies_CA_2[10,2] + dcoef_all$Hobbies_CA_2[11,2] + dcoef_all$Hobbies_CA_2[12,2] + dcoef_all$Hobbies_CA_2[13,2] + dcoef_all$Hobbies_CA_2[14,2] + dcoef_all$Hobbies_CA_2[14,2] + dcoef_all$Hobbies_CA_2[16,2] + dcoef_all$Hobbies_CA_2[17,2] + dcoef_all$Hobbies_CA_2[18,2] + dcoef_all$Hobbies_CA_2[19,2])


forecasts_all_23$Hobbies_CA_3_High = forecasts_all_23$Hobbies_CA_3_pred + 1.96 * (dcoef_all$Hobbies_CA_3[1,2] + dcoef_all$Hobbies_CA_3[2,2] + dcoef_all$Hobbies_CA_3[3,2] + dcoef_all$Hobbies_CA_3[4,2] + dcoef_all$Hobbies_CA_3[5,2] + dcoef_all$Hobbies_CA_3[6,2] + dcoef_all$Hobbies_CA_3[7,2] + dcoef_all$Hobbies_CA_3[8,2] + dcoef_all$Hobbies_CA_3[9,2] + dcoef_all$Hobbies_CA_3[10,2] + dcoef_all$Hobbies_CA_3[11,2] + dcoef_all$Hobbies_CA_3[12,2] + dcoef_all$Hobbies_CA_3[13,2] + dcoef_all$Hobbies_CA_3[14,2] + dcoef_all$Hobbies_CA_3[14,2] + dcoef_all$Hobbies_CA_3[16,2] + dcoef_all$Hobbies_CA_3[17,2] + dcoef_all$Hobbies_CA_3[18,2] + dcoef_all$Hobbies_CA_3[19,2])

forecasts_all_23$Hobbies_CA_3_High = forecasts_all_23$Hobbies_CA_3_pred - 1.96 * (dcoef_all$Hobbies_CA_3[1,2] + dcoef_all$Hobbies_CA_3[2,2] + dcoef_all$Hobbies_CA_3[3,2] + dcoef_all$Hobbies_CA_3[4,2] + dcoef_all$Hobbies_CA_3[5,2] + dcoef_all$Hobbies_CA_3[6,2] + dcoef_all$Hobbies_CA_3[7,2] + dcoef_all$Hobbies_CA_3[8,2] + dcoef_all$Hobbies_CA_3[9,2] + dcoef_all$Hobbies_CA_3[10,2] + dcoef_all$Hobbies_CA_3[11,2] + dcoef_all$Hobbies_CA_3[12,2] + dcoef_all$Hobbies_CA_3[13,2] + dcoef_all$Hobbies_CA_3[14,2] + dcoef_all$Hobbies_CA_3[14,2] + dcoef_all$Hobbies_CA_3[16,2] + dcoef_all$Hobbies_CA_3[17,2] + dcoef_all$Hobbies_CA_3[18,2] + dcoef_all$Hobbies_CA_3[19,2])

print(length_x)
print(dim(forecasts_all_23))

plot(x, forecasts_all_23$Hobbies_CA_1, type = "l", frame = FALSE, pch = 19, col = "blue", xlab = "time", ylab = "Hobbies Store 1")
polygon(c(x,rev(x)),c(forecasts_all_23$Hobbies_CA_1_Low,rev(forecasts_all_23$Hobbies_CA_1_High)),col="thistle",border=NA)
lines(x, forecasts_all_23$Hobbies_CA_1, type = "l",  pch = 19, col = "blue")
lines(x, forecasts_all_23$Hobbies_CA_1_pred, pch = 18, col = "red", type = "l")
legend("bottomright", legend=c("Real", "Predicted", "C.I. 95%"),col=c("blue", "red","thistle"), lty = 1:1, cex=0.8)
title("Hobbies Store 1 - VAR All")















rm(dcoef_all)
rm(df_all, df_all_train, df_all_test)
rm(var_all_train)

# Save the RMSSE Results in the previous 2.1 dataframe
df_rmsse_CA_1[1, 3] = RMSSE(forecasts_all_23$Hobbies_CA_1, forecasts_all_23$Hobbies_CA_1_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_1[2, 3] = RMSSE(forecasts_all_23$Household_1_CA_1, forecasts_all_23$Household_1_CA_1_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_1[3, 3] = RMSSE(forecasts_all_23$Household_2_CA_1, forecasts_all_23$Household_2_CA_1_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_1[4, 3] = RMSSE(forecasts_all_23$Foods_1_CA_1, forecasts_all_23$Foods_1_CA_1_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_1[5, 3] = RMSSE(forecasts_all_23$Foods_2_CA_1, forecasts_all_23$Foods_2_CA_1_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_1[6, 3] = RMSSE(forecasts_all_23$Foods_3_CA_1, forecasts_all_23$Foods_3_CA_1_pred, scale = 1, na.rm = TRUE)

df_rmsse_CA_2[1, 3] = RMSSE(forecasts_all_23$Hobbies_CA_2, forecasts_all_23$Hobbies_CA_2_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_2[2, 3] = RMSSE(forecasts_all_23$Household_1_CA_2, forecasts_all_23$Household_1_CA_2_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_2[3, 3] = RMSSE(forecasts_all_23$Household_2_CA_2, forecasts_all_23$Household_2_CA_2_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_2[4, 3] = RMSSE(forecasts_all_23$Foods_1_CA_2, forecasts_all_23$Foods_1_CA_2_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_2[5, 3] = RMSSE(forecasts_all_23$Foods_2_CA_2, forecasts_all_23$Foods_2_CA_2_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_2[6, 3] = RMSSE(forecasts_all_23$Foods_3_CA_2, forecasts_all_23$Foods_3_CA_2_pred, scale = 1, na.rm = TRUE)

df_rmsse_CA_3[1, 3] = RMSSE(forecasts_all_23$Hobbies_CA_3, forecasts_all_23$Hobbies_CA_3_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_3[2, 3] = RMSSE(forecasts_all_23$Household_1_CA_3, forecasts_all_23$Household_1_CA_3_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_3[3, 3] = RMSSE(forecasts_all_23$Household_2_CA_3, forecasts_all_23$Household_2_CA_3_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_3[4, 3] = RMSSE(forecasts_all_23$Foods_1_CA_3, forecasts_all_23$Foods_1_CA_3_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_3[5, 3] = RMSSE(forecasts_all_23$Foods_2_CA_3, forecasts_all_23$Foods_2_CA_3_pred, scale = 1, na.rm = TRUE)
df_rmsse_CA_3[6, 3] = RMSSE(forecasts_all_23$Foods_3_CA_3, forecasts_all_23$Foods_3_CA_3_pred, scale = 1, na.rm = TRUE)

rownames(df_rmsse_CA_1) =  c("Hobbies","Household1","Household2","Foods1","Foods2","Foods3")
rownames(df_rmsse_CA_2) =  c("Hobbies","Household1","Household2","Foods1","Foods2","Foods3")
rownames(df_rmsse_CA_3) =  c("Hobbies","Household1","Household2","Foods1","Foods2","Foods3")

colnames(df_rmsse_CA_1) = c("a","b","c")
colnames(df_rmsse_CA_2) = c("a","b","c")
colnames(df_rmsse_CA_3) = c("a","b","c")

df_rmsse_CA_1 = transform(df_rmsse_CA_1, d= ifelse( a <= b, ifelse(a <= c, "A", "C"),ifelse(b <= c, "B", "C")))
df_rmsse_CA_2 = transform(df_rmsse_CA_2, d= ifelse( a <= b, ifelse(a <= c, "A", "C"),ifelse(b <= c, "B", "C")))
df_rmsse_CA_3 = transform(df_rmsse_CA_3, d= ifelse( a <= b, ifelse(a <= c, "A", "C"),ifelse(b <= c, "B", "C")))

colnames(df_rmsse_CA_1) = c("var21","var22","var23","best_var")
colnames(df_rmsse_CA_2) = c("var21","var22","var23","best_var")
colnames(df_rmsse_CA_3) = c("var21","var22","var23","best_var")

# plot the VAR time-series and estimates at 1-step 
x = 1:test_sample

# Plot hobbies real vs predicted forecasts for each store

par(mfrow=c(1,1))

if(df_rmsse_CA_1[1,4] == "C"){
  plot(x, forecasts_all_23$Hobbies_CA_1, type = "l", frame = FALSE, pch = 19, col = "blue", xlab = "time", ylab = "Hobbies Store 1")
  lines(x, forecasts_all_23$Hobbies_CA_1_pred, pch = 18, col = "red", type = "l")
  legend("bottomright", legend=c("Real", "Predicted"),col=c("blue", "red"), lty = 1:1, cex=0.8)
  title("Hobbies Stores 1 -> Best VAR = All VAR")
}else if (df_rmsse_CA_1[1,4]=="B"){
  plot(x, forecasts_hobbies_22$Hobbies_CA_1, type = "l", frame = FALSE, pch = 19, col = "blue", xlab = "time", ylab = "Hobbies Store 1")
  lines(x, forecasts_hobbies_22$Hobbies_CA_1_pred, pch = 18, col = "red", type = "l")
  title("Hobbies Stores 1 -> Best VAR = Product VAR")
}else {
  plot(x, forecasts_CA_1_21$Hobbies, type = "l", frame = FALSE, pch = 19, col = "blue", xlab = "time", ylab = "Hobbies Store 1")
  lines(x, forecasts_CA_1_21$Hobbies_pred, pch = 18, col = "red", type = "l")
  title("Hobbies Stores 1 -> Best VAR = Store VAR")
}

if(df_rmsse_CA_2[1,4] == "C"){
  plot(x, forecasts_all_23$Hobbies_CA_2, type = "l", frame = FALSE, pch = 19, col = "blue", xlab = "time", ylab = "Hobbies Store 2")
  lines(x, forecasts_all_23$Hobbies_CA_2_pred, pch = 18, col = "red", type = "l")
  legend("bottomright", legend=c("Real", "Predicted"),col=c("blue", "red"), lty = 1:1, cex=0.8)
  title("Hobbies Stores 2 -> Best VAR = All VAR")
}else if (df_rmsse_CA_2[1,4]=="B"){
  plot(x, forecasts_hobbies_22$Hobbies_CA_2, type = "l", frame = FALSE, pch = 19, col = "blue", xlab = "time", ylab = "Hobbies Store 2")
  lines(x, forecasts_hobbies_22$Hobbies_CA_2_pred, pch = 18, col = "red", type = "l")
  title("Hobbies Stores 2 -> Best VAR = Product VAR")
}else {
  plot(x, forecasts_CA_2_21$Hobbies, type = "l", frame = FALSE, pch = 19, col = "blue", xlab = "time", ylab = "Hobbies Store 2")
  lines(x, forecasts_CA_2_21$Hobbies_pred, pch = 18, col = "red", type = "l")
  title("Hobbies Stores 2 -> Best VAR = Store VAR")
}

if(df_rmsse_CA_3[1,4] == "C"){
  plot(x, forecasts_all_23$Hobbies_CA_3, type = "l", frame = FALSE, pch = 19, col = "blue", xlab = "time", ylab = "Hobbies Store 3")
  lines(x, forecasts_all_23$Hobbies_CA_3_pred, pch = 18, col = "red", type = "l")
  legend("bottomright", legend=c("Real", "Predicted"),col=c("blue", "red"), lty = 1:1, cex=0.8)
  title("Hobbies Stores 2 -> Best VAR = All VAR")
}else if (df_rmsse_CA_3[1,4]=="B"){
  plot(x, forecasts_hobbies_22$Hobbies_CA_2, type = "l", frame = FALSE, pch = 19, col = "blue", xlab = "time", ylab = "Hobbies Store 3")
  lines(x, forecasts_hobbies_22$Hobbies_CA_2_pred, pch = 18, col = "red", type = "l")
  title("Hobbies Stores 2 -> Best VAR = Product VAR")
}else {
  plot(x, forecasts_CA_3_21$Hobbies, type = "l", frame = FALSE, pch = 19, col = "blue", xlab = "time", ylab = "Hobbies Store 3")
  lines(x, forecasts_CA_3_21$Hobbies_pred, pch = 18, col = "red", type = "l")
  title("Hobbies Stores 3 -> Best VAR = Store VAR")
}





