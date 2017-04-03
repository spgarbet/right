#########################
# Simple Mortality Model
# John A. Graves
#########################
#http://people.reed.edu/~jones/141/Life.html
#http://www.demog.berkeley.edu/213/PropHazI/welcome.pdf
#http://www.mortality.org/cgi-bin/hmd/country.php?cntr=USA&level=1
library(ggplot2); theme_set(theme_bw())
setwd("~/Dropbox/Projects/PREDICT/Simulation/simple_mortality_model/data/")
list.files()

# Read in Raw Lifetable Data
deaths.male = read.table("mltper_1x1.txt",skip=2,header=T,as.is=TRUE); head(deaths.male)
deaths.female = read.table("fltper_1x1.txt",skip=2,header=T,as.is=TRUE); head(deaths.female)
deaths.both = read.table("bltper_1x1.txt",skip=2,header=T,as.is=T); head(deaths.raw)

# Make Some Edits
deaths.male[which(deaths.male$Age=="110+"),]$Age = 110
deaths.female[which(deaths.female$Age=="110+"),]$Age = 110
deaths.both[which(deaths.both$Age=="110+"),]$Age = 110
deaths.male$Age = as.numeric(deaths.male$Age)
deaths.female$Age = as.numeric(deaths.female$Age)
deaths.both$Age = as.numeric(deaths.both$Age)

# Isolate each file to 2010 deaths only
deaths.2010 = subset(deaths.both,Year==2010); head(deaths.2010)
deaths.male.2010 = subset(deaths.male,Year==2010); head(deaths.male.2010)
deaths.female.2010 = subset(deaths.female,Year==2010); head(deaths.female.2010)

# Generate the CDF ## CDF == 1-Survival = 1 - lx/10)
deaths.2010$cdf = with(deaths.2010,1-lx/100000)
deaths.male.2010$cdf = with(deaths.male.2010,1-lx/100000)
deaths.female.2010$cdf = with(deaths.female.2010,1-lx/100000)


# Plotting a CDF Derived from the lx column of the life table
deaths.plot = rbind.data.frame(transform(deaths.2010,type="All Ages"),transform(deaths.male.2010,type="Males"),transform(deaths.female.2010,type="Females"))
dev.off()
pdf("death_cdf.pdf")
ggplot(data=deaths.plot,aes(x=Age,y=cdf))+geom_step(aes(colour=type))
dev.off()


simdeath.cdf = function(A=90,cdf=deaths.female.2010$cdf,Ages=1:110) {
  out = NA
  prDeath = diff(cdf)
  prDeath.c = prDeath[Ages>A]/sum(prDeath[Ages>A])
  prDeath.ltA = prDeath[Ages<=A]*0
  #cdf.c = c(diffinv(prDeath),1)
  cdf.c = c(prDeath.ltA,diffinv(prDeath.c))
  u = runif(1)
  #cat(u)
  out = cdf.c 
  return(out)
}

# Generate Male Mortality Data
CDF.Overall = matrix(nrow=length(1:110),ncol=112)
for (r in 1:110) {
  CDF.Overall[r,1] = r
  CDF.Overall[r,2:112] = simdeath.cdf(A=r,cdf=deaths.male.2010$cdf)
}
CDF.Overall = data.frame(CDF.Overall)
names(CDF.Overall) = c("Age",paste("aMortCEF",1:111,sep=""))
CDF.Male = CDF.Overall
write.csv(CDF.Overall,file="~/Dropbox/Projects/PREDICT/Simulation/simple_mortality_model/data/usmortality_males2010.csv")


# Generate Female Mortality Data
CDF.Overall = matrix(nrow=length(1:110),ncol=112)
for (r in 1:110) {
  CDF.Overall[r,1] = r
  CDF.Overall[r,2:112] = simdeath.cdf(A=r,cdf=deaths.female.2010$cdf)
}
CDF.Overall = data.frame(CDF.Overall)
names(CDF.Overall) = c("Age",paste("aMortCEF",1:111,sep=""))
CDF.Female = CDF.Overall
write.csv(CDF.Overall,file="~/Dropbox/Projects/PREDICT/Simulation/simple_mortality_model/data/usmortality_females2010.csv")

# Generate Female Mortality Data
CDF.Male$gender = "male"
CDF.Female$gender = "female"
write.csv(rbind.data.frame(CDF.Male,CDF.Female),file="~/Dropbox/Projects/PREDICT/Simulation/simple_mortality_model/data/usmortality_bothsexes2010.csv")


# Simulate A Death Time
simdeath = function(A=0,cdf=CDF,Ages=1:110) {
  out = NA
  while(is.na(out)) {
    prDeath = diff(cdf)
    prDeath.c = prDeath[Ages>A]/sum(prDeath[Ages>A])
    cdf.c = diffinv(prDeath.c)
    u = runif(1)
    (out = Ages[Ages>A][sum(cdf.c<u)]) # Equal Probability of Dying During the Year
  }
  return(out)
}
simdeath(A=0)

# Condtional Probability of Death After Age 10
prDeath = diff(CDF)
Age = 50
prDeath50 = prDeath[Ages>=Age]/sum(prDeath[Ages>=Age])
sum(Ages[Ages>=Age]*prDeath50) # 82.12964
check = c()
for (i in 1:100000) check[i] = simdeath(A=Age); mean(check,na.rm=TRUE)


# 
# Yearly Hazard of Death
#
rise = diff(log(LifeTable$lx))
run = diff(LifeTable$Age)
hazards = -rise/run
rise<- diff(log(us05$lx))
dotchart(hazards,labels=LifeTable$Age,main="Hazard Rate by Age Category")
