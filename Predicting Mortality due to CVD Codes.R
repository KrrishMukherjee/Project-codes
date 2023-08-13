#For bringing data
read.csv(file.choose())
d=read.csv(file.choose())
#For descriptive ;
data.frame(d)
Summary(d)

#For histogram we used; 
hist(d$age,prob=T,breaks=20,main="histogram of age")
	lines(density(d$age),col="red") 
	hist(d$serum_creatinine,prob=T,breaks=20,main="histogram of serum_creatinine")
	lines(d$density(serum_creatinine),col="red")
	hist(d$serum_creatinine,xlim=c(0.6,6.0),prob=T,breaks=20,main="histogram of serum_creatinine")
serum_sodium=d$serum_sodium
	hist(d$serum_creatinine,xlim=c(0.6,6.0),prob=T,breaks=20,main="histogram of serum_creatinine")
hist(d$serum_sodium,prob=T,breaks=20,main="histogram of serum_sodium")
	lines(d$density(serum_sodium),col="red")
	hist(d$serum_creatinine,prob=T,breaks=20,main="histogram of serum_creatinine")
	lines(density(d$serum_creatinine),col="red")
	hist(d$serum_creatinine,xlim=c(0.6,6.0),prob=T,breaks=20,main="histogram of serum_creatinine")
	serum_sodium=d$serum_sodium
	hist(d$serum_creatinine,xlim=c(0.6,6.0),prob=T,breaks=20,main="histogram of serum_creatinine")
	hist(d$serum_sodium,prob=T,breaks=20,main="histogram of serum_sodium")
	lines(density(d$serum_sodium)
	      
  #fitting linear regression using just one variable
age=d$age
death=d$DEATH_EVENT
 plot(death,age)
 plot(age,death)
 lm(death~age)
 
 #fitting logistic regression using just one independent variable for watching the difference with linear regression
glm(death~age,family=binomial)
 #fitting multivariate logistic regression
glm1=glm(DEATH_EVENT~.,data=d,family=binomial)
summary(glm1)

#likelihood ratio test
1-pchisq(107.855-86.142,11)

#Goodness of fit
et=predict(glm1,d,type="response")
diff=death-et
diff2=(diff)^2
diff2/(sqrt(et)*sqrt(1-et))
1-pchisq(35.05043,100-12)


#accuracy
accuracy=mean(et==death)


