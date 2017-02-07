#HW3 Problem 2
# German Credit Score Prediction

##### Starter code for German credit scoring #####

# Refer to http://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data) for variable description. 
#Notice that “It is worse to class a customer as good when they are bad (5), than it is to class a customer as bad when they are good (1).” 
#Define your cost function accordingly!


colnames(german_credit) = c("chk_acct", "duration", "credit_his", "purpose", 
                            "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", 
                            "present_resid", "property", "age", "other_install", "housing", "n_credits", 
                            "job", "n_people", "telephone", "foreign", "response")

# orginal response coding 1= good, 2 = bad we need 0 = good, 1 = bad
german_credit$response = german_credit$response - 1

##### EDA ######

