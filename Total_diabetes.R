library(caret)

#creating dataset
link = "/home/sanket/Sanket/Praxis/R_study/DM1 Assignment/Custom_Diabetes_Dataset.csv"
dataset = read.csv(link, header = T )
dataset.clean = dataset

#Cleaning the dataset
dataset.clean[dataset.clean$blood.pressure == 0,3] = median(dataset.clean$blood.pressure)
dataset.clean[dataset.clean$triceps.skin.thickness == 0,4] = median(dataset.clean$triceps.skin.thickness)
dataset.clean[dataset.clean$plasma.glucose == 0,2] = median(dataset.clean$plasma.glucose)
dataset.clean[dataset.clean$bmi == 0,6] = median(dataset.clean$bmi)

#creating trainset and testset
trainset = dataset.clean[1:700,]
testset = dataset.clean[701:768,]

names(trainset)


#featrure selection based on p-value
P_val = c()
for (j in 1:8)
{
  a = summary(table(dataset.clean[,c(9,j)]))
  P_val =rbind(P_val,a$p.value)
}
print(P_val)

# modeling
Cost_matrix = c()
Accuracy= c()
Kappa_value = c()
models = c("C5.0","rpart", "J48")
for (i in models)
{
  cntrl=trainControl(method = "cv",number=5)
  model_C50_car=train(diabetes~ pregnancies + plasma.glucose + age,data=trainset,method = i ,trControl=cntrl,metric="Kappa")
  #model_C50_car=train(diabetes~ plasma.glucose + age + bmi + blood.pressure + diabetes.pedigree,data=trainset,method = i ,trControl=cntrl,metric="Kappa")
  pred_C50_car=predict(model_C50_car,newdata = testset)
  con = confusionMatrix(pred_C50_car,testset$diabetes)
  cost = con$table[2] * 150 +  con$table[3] * 50
  Cost_matrix = rbind(Cost_matrix, cost)
  Accuracy = rbind(Accuracy,con$overall[1])
  Kappa_value = rbind(Kappa_value,con$overall[2])
}
Compare_Matrix = data.frame(Models = c("C5.0","RPART", "J48"),Cost_matrix,Kappa_value,Accuracy)
print(Compare_Matrix)


