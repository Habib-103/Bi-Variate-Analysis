setwd('F:/AUAF FALL 2020/ITC255/R')
library(reshape)
library(wooldridge)

wool = data.frame(wooldridge::big9salary)
View(wool)

#extracting our needed data
d = data.frame(wool[12],wool[3], wool[8], wool[11], wool[5])

#rename our vars
d=rename(d, c(female="gender",salary ="salary", prof="fullTimePro",yearphd="yearPhdObtained",totpge="totalArticlePages" ))

#cleaning NA values
d_clean = na.omit(d)
#so now we analyze our clean data or implement our Tasks
summary(d_clean$salary)
sd(d_clean$salary)
sd(d_clean$yearhdObtained)
sd(d_clean$totalArticlePages)

aFre = table(d_clean$fullTimePro)
rFre = prop.table(aFre)
ecFre = cumsum(rFre)
tg = cbind(aFre, rFre, ecFre)
disTable = data.frame(tg)
show(disTable)

View(d_clean)

pie(rFre
    ,labels = c("no", "yes")
    ,col = c("red","blue")
    ,main = "Pie chart of Full Time Professor")
legend("toplef"
       ,legend = c("no","yes")
       ,col = c("red","blue")
       ,lty = 3)
box(which = "plot", lty = 1)



###TASKS:

##1. Consider two QL vars from your dataset and construct their two-way table.
#for this task I selected "gender" and "fullTimePro" as my Qual vars

t = table(d_clean$fullTimePro, d_clean$gender)
t

#both values of both variables are 0s and 1s;
#therefore we change the gender values into "f" & "m"

gen = c()
for(i in 1 :length(d_clean$gender)){
  
  if(d_clean$gender[i]==1){
    gen[i]="f"
  }else{
    gen[i]="m"
  }
}

d_clean = cbind(d_clean, gen)

#so now good to go 
t = table(d_clean$gen, d_clean$fullTimePro)
t
marginal_fullTimePro = colSums(t)
marginal_fullTimePro
t1 = rbind(t,marginal_fullTimePro)
t1
marginal_gender = rowSums(t1)
t2 = cbind(t1, marginal_gender)
View(t2)
write.csv(t2, file = "gender&fullTimePro_two-way-table.csv")

##2. Consider 1 QL nd 1 QN vars in your dataset and calculate their t-stat/F-stat.
#for this task we consider the "fullTimePro" Qual var and the "yearPhdObtained" Quant var

t.test(d_clean$yearPhdObtained~d_clean$fullTimePro)
#we should consider fullTimePro as a factor when it comes as to yearsthephd was obtained by 


##3. Consider 2 QN vars in your dataset: plot their scatter graph, calculate their core and build thei linear model.
#for this task we consider these Quant vars:  "totalArticlePages" & "salary"

#plotting their scatter graph
png(filename = "Scatter Graph of Article Pages & Salary.png")
plot(d_clean$totalArticlePages, d_clean$salary
     , main = "Scatter Graph of Article Pages & Salary"
     , xlab = "Total Article Pages"
     , ylab = "Salary Amount"
     , col = ("red")
     , pch = 1)
legend("bottomright", legend = "(articlePages, salary)", col = "red", pch=1)
dev.off()
cor(d_clean$totalArticlePages, d_clean$salary)


#hence its liner y = a+bx 
# x = totalArticlePages   y = salary

l_s_Salary = lm(d_clean$salary~d_clean$totalArticlePages)
summary(l_s_Salary)

# a = 67406  b = 91.014
#salary =  67406+91*totalArticlePages




