y = trees[[3]] #Volume
x1 = trees[[1]] #Girth
x2 = trees[[2]] #Height
x3 = sqrt(x1) #sqrt(Girth)
x4 = sqrt(x2) #sqrt(Height)
x5 = x1*x1 #Girth^2
x6 = x2*x2 #Height^2

myData<- list("y"=y,"x1"=x1,"x2"=x2,"x3"=x3,"x4"=x4,"x5"=x5,"x6"=x6, "r"=(x1/2*pi))

pef<- lm(y~(r^2*x2), data = myData)
summary(pef)


model1 <- lm(y~x1+x2+x3+x4+x5+x6, data = myData)

model2 <- lm(y~x1+x2+x3+x4+x5, data = myData)
model3 <- lm(y~x1+x2+x3+x4+x6, data = myData)
model4 <- lm(y~x1+x2+x3+x5+x6, data = myData)
model5 <- lm(y~x1+x2+x4+x5+x6, data = myData)
model6 <- lm(y~x1+x3+x4+x5+x6, data = myData)
model7 <- lm(y~x2+x3+x4+x5+x6, data = myData)

model8 <- lm(y~x3+x4+x5+x6, data = myData)
model9 <- lm(y~x2+x4+x5+x6, data = myData)
model10 <- lm(y~x2+x3+x5+x6, data = myData)
model11 <- lm(y~x2+x3+x4+x6, data = myData)
model12 <- lm(y~x2+x3+x4+x5, data = myData)
model13 <- lm(y~x1+x4+x5+x6, data = myData)
model14 <- lm(y~x1+x3+x5+x6, data = myData)
model15 <- lm(y~x1+x3+x4+x6, data = myData)
model16 <- lm(y~x1+x3+x4+x5, data = myData)
model17 <- lm(y~x1+x2+x5+x6, data = myData)
model18 <- lm(y~x1+x2+x4+x6, data = myData)
model19 <- lm(y~x1+x2+x4+x5, data = myData)
model20 <- lm(y~x1+x2+x3+x6, data = myData)
model21 <- lm(y~x1+x2+x3+x5, data = myData)
model22 <- lm(y~x1+x2+x3+x4, data = myData)

model23 <- lm(y~x4+x5+x6, data = myData)
model24 <- lm(y~x3+x5+x6, data = myData)
model25 <- lm(y~x3+x4+x6, data = myData)
model26 <- lm(y~x3+x4+x5, data = myData)
model27 <- lm(y~x1+x5+x6, data = myData)
model28 <- lm(y~x1+x4+x6, data = myData)
model29 <- lm(y~x1+x4+x5, data = myData)
model30 <- lm(y~x1+x2+x6, data = myData)
model31 <- lm(y~x1+x2+x5, data = myData)
model32 <- lm(y~x1+x2+x3, data = myData)
model33 <- lm(y~x2+x5+x6, data = myData)
model34 <- lm(y~x2+x4+x6, data = myData)
model35 <- lm(y~x2+x4+x5, data = myData)
model36 <- lm(y~x2+x3+x6, data = myData)
model37 <- lm(y~x2+x3+x5, data = myData)
model38 <- lm(y~x1+x3+x5, data = myData)
model39 <- lm(y~x1+x3+x4, data = myData)
model40 <- lm(y~x1+x2+x4, data = myData)
model41 <- lm(y~x1+x2+x3, data = myData)
model42 <- lm(y~x2+x3+x4, data = myData)

model43 <- lm(y~x1+x2, data = myData)
model44 <- lm(y~x1+x3, data = myData)
model45 <- lm(y~x1+x4, data = myData)
model46 <- lm(y~x1+x5, data = myData)
model47 <- lm(y~x1+x6, data = myData)
model48 <- lm(y~x2+x3, data = myData)
model49 <- lm(y~x2+x4, data = myData)
model50 <- lm(y~x2+x5, data = myData)
model51 <- lm(y~x2+x6, data = myData)
model52 <- lm(y~x3+x4, data = myData)
model53 <- lm(y~x3+x5, data = myData)
model54 <- lm(y~x3+x6, data = myData)
model55 <- lm(y~x4+x5, data = myData)
model56 <- lm(y~x4+x6, data = myData)
model57 <- lm(y~x5+x6, data = myData)

model58 <- lm(y~x1, data = myData)
model59 <- lm(y~x2, data = myData)
model60 <- lm(y~x3, data = myData)
model61 <- lm(y~x4, data = myData)
model62 <- lm(y~x5, data = myData)
model63 <- lm(y~x6, data = myData)












