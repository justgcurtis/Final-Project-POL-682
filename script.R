#Summary Statistics

means<-c(mean(subset(allmajleses, majles==2)$reelect), mean(subset(allmajleses, majles==3)$reelect),
         mean(subset(allmajleses, majles==4)$reelect), mean(subset(allmajleses, majles==5)$reelect),
         mean(subset(allmajleses, majles==6)$reelect), mean(subset(allmajleses, majles==7)$reelect),
         mean(subset(allmajleses, majles==8)$reelect), mean(subset(allmajleses, majles==9)$reelect))
                                                          


#All the models

require(MASS)

all<-glm(reelect~bachelors+masters+doctorate+clergy, 
         data=allmajleses, family=binomial (link="logit"))

pred.all<-mvrnorm(1000, coef(all), vcov(all))

second<-glm(reelect~bachelors+masters+doctorate+clergy, 
            family=binomial (link="logit"), 
            data=allmajleses, subset=majles==2)

pred.second<-mvrnorm(1000, coef(second), vcov(second))

third<-glm(reelect~bachelors+masters+doctorate+clergy, 
           family=binomial (link="logit"), 
           data=allmajleses, subset=majles==3)

pred.third<-mvrnorm(1000, coef(third), vcov(third))

fourth<-glm(reelect~bachelors+masters+doctorate+clergy, 
            family=binomial (link="logit"), 
            data=allmajleses, subset=majles==4)

pred.fourth<-mvrnorm(1000, coef(fourth), vcov(fourth))

fifth<-glm(reelect~bachelors+masters+doctorate+clergy, 
           family=binomial (link="logit"), 
           data=allmajleses, subset=majles==5)

pred.fifth<-mvrnorm(1000, coef(fifth), vcov(fifth))

sixth<-glm(reelect~bachelors+masters+doctorate+clergy, 
           family=binomial (link="logit"), 
           data=allmajleses, subset=majles==6)

pred.sixth<-mvrnorm(1000, coef(sixth), vcov(sixth))

seventh<-glm(reelect~bachelors+masters+doctorate+clergy, 
             family=binomial (link="logit"), 
             data=allmajleses, subset=majles==7)

pred.seventh<-mvrnorm(1000, coef(seventh), vcov(seventh))

eighth<-glm(reelect~bachelors+masters+doctorate+clergy, 
            family=binomial (link="logit"), 
            data=allmajleses, subset=majles==8)

pred.eighth<-mvrnorm(1000, coef(eighth), vcov(eighth))

ninth<-glm(reelect~bachelors+masters+doctorate+clergy, 
           family=binomial (link="logit"), 
           data=allmajleses, subset=majles==9)

pred.ninth<-mvrnorm(1000, coef(ninth), vcov(ninth))

leap<-glm(leap~bachelors+masters+doctorate+clergy, 
          data=allmajleses, family=binomial (link="logit"))

pred.leap<-mvrnorm(1000, coef(leap), vcov(leap))

#No education

upper.bound.c.1=c(plogis(second$coef[1]+second$coef[2]*0+second$coef[3]*0+
                       second$coef[4]*0+quantile(pred.second[,5], c(0.95))), 
              plogis(third$coef[1]+third$coef[2]*0+third$coef[3]*0+
                       third$coef[4]*0+quantile(pred.third[,5], c(0.95))), 
              plogis(fourth$coef[1]+fourth$coef[2]*0+fourth$coef[3]*0+
                       fourth$coef[4]*0+quantile(pred.fourth[,5], c(0.95))), 
              plogis(fifth$coef[1]+fifth$coef[2]*0+fifth$coef[3]*0+
                       fifth$coef[4]*0+quantile(pred.fifth[,5], c(0.95))), 
              plogis(sixth$coef[1]+sixth$coef[2]*0+sixth$coef[3]*0+
                       sixth$coef[4]*0+quantile(pred.sixth[,5], c(0.95))), 
              plogis(seventh$coef[1]+seventh$coef[2]*0+seventh$coef[3]*0+
                       seventh$coef[4]*0+quantile(pred.seventh[,5], c(0.95))), 
              plogis(eighth$coef[1]+eighth$coef[2]*0+eighth$coef[3]*0+
                       eighth$coef[4]*0+quantile(pred.eighth[,5], c(0.95))), 
              plogis(ninth$coef[1]+ninth$coef[2]*0+ninth$coef[3]*0+
                       ninth$coef[4]*0+quantile(pred.ninth[,5], c(0.95))))

lower.bound.c.1=c(plogis(second$coef[1]+second$coef[2]*0+second$coef[3]*0+
                       second$coef[4]*0+quantile(pred.second[,5], c(0.05))), 
              plogis(third$coef[1]+third$coef[2]*0+third$coef[3]*0+
                       third$coef[4]*0+quantile(pred.third[,5], c(0.05))), 
              plogis(fourth$coef[1]+fourth$coef[2]*0+fourth$coef[3]*0+
                       fourth$coef[4]*0+quantile(pred.fourth[,5], c(0.05))), 
              plogis(fifth$coef[1]+fifth$coef[2]*0+fifth$coef[3]*0+
                       fifth$coef[4]*0+quantile(pred.fifth[,5], c(0.05))), 
              plogis(sixth$coef[1]+sixth$coef[2]*0+sixth$coef[3]*0+
                       sixth$coef[4]*0+quantile(pred.sixth[,5], c(0.05))), 
              plogis(seventh$coef[1]+seventh$coef[2]*0+seventh$coef[3]*0+
                       seventh$coef[4]*0+quantile(pred.seventh[,5], c(0.05))), 
              plogis(eighth$coef[1]+eighth$coef[2]*0+eighth$coef[3]*0+
                       eighth$coef[4]*0+quantile(pred.eighth[,5], c(0.05))), 
              plogis(ninth$coef[1]+ninth$coef[2]*0+ninth$coef[3]*0+
                       ninth$coef[4]*0+quantile(pred.ninth[,5], c(0.05))))

point.c.1=c(plogis(second$coef[1]+second$coef[2]*0+second$coef[3]*0+
                 second$coef[4]*0+second$coef[5]*1), 
        plogis(third$coef[1]+third$coef[2]*0+third$coef[3]*0+
                 third$coef[4]*0+third$coef[5]*1), 
        plogis(fourth$coef[1]+fourth$coef[2]*0+
                 fourth$coef[3]*0+fourth$coef[4]*0+
                 fourth$coef[5]*1), 
        plogis(fifth$coef[1]+fifth$coef[2]*0+fifth$coef[3]*0+
                 fifth$coef[4]*0+fifth$coef[5]*1), 
        plogis(sixth$coef[1]+sixth$coef[2]*0+sixth$coef[3]*0+
                 sixth$coef[4]*0+sixth$coef[5]*1), 
        plogis(seventh$coef[1]+seventh$coef[2]*0+seventh$coef[3]*0+
                 seventh$coef[4]*0+seventh$coef[5]*1), 
        plogis(eighth$coef[1]+eighth$coef[2]*0+eighth$coef[3]*0+
                 eighth$coef[4]*0+eighth$coef[5]*1), 
        plogis(ninth$coef[1]+ninth$coef[2]*0+ninth$coef[3]*0+
                 ninth$coef[4]*0+ninth$coef[5]*1))

upper.bound.l.1=c(plogis(quantile(pred.second[,1], c(0.95))+second$coef[2]*0+second$coef[3]*0+
                         second$coef[4]*0+second$coef[5]*0), 
                plogis(quantile(pred.third[,1], c(0.95))+third$coef[2]*0+third$coef[3]*0+
                           third$coef[4]*0+third$coef[5]*0), 
                plogis(quantile(pred.fourth[,1], c(0.95))+fourth$coef[2]*0+fourth$coef[3]*0+
                         fourth$coef[4]*0+fourth$coef[5]*0), 
                plogis(quantile(pred.fifth[,1], c(0.95))+fifth$coef[2]*0+fifth$coef[3]*0+
                         fifth$coef[4]*0+fifth$coef[5]*0), 
                plogis(quantile(pred.sixth[,1], c(0.95))+sixth$coef[2]*0+sixth$coef[3]*0+
                         sixth$coef[4]*0+sixth$coef[5]*0), 
                plogis(quantile(pred.seventh[,1], c(0.95))+seventh$coef[2]*0+seventh$coef[3]*0+
                         seventh$coef[4]*0+seventh$coef[5]*0), 
                plogis(quantile(pred.eighth[,1], c(0.95))+eighth$coef[2]*0+eighth$coef[3]*0+
                         eighth$coef[4]*0+eighth$coef[5]*0), 
                plogis(quantile(pred.ninth[,1], c(0.95))+ninth$coef[2]*0+ninth$coef[3]*0+
                         ninth$coef[4]*0+ninth$coef[5]*0))

lower.bound.l.1=c(plogis(quantile(pred.second[,1], c(0.05))+second$coef[2]*0+second$coef[3]*0+
                           second$coef[4]*0+second$coef[5]*0), 
                  plogis(quantile(pred.third[,1], c(0.05))+third$coef[2]*0+third$coef[3]*0+
                           third$coef[4]*0+third$coef[5]*0), 
                  plogis(quantile(pred.fourth[,1], c(0.05))+fourth$coef[2]*0+fourth$coef[3]*0+
                           fourth$coef[4]*0+fourth$coef[5]*0), 
                  plogis(quantile(pred.fifth[,1], c(0.05))+fifth$coef[2]*0+fifth$coef[3]*0+
                           fifth$coef[4]*0+fifth$coef[5]*0), 
                  plogis(quantile(pred.sixth[,1], c(0.05))+sixth$coef[2]*0+sixth$coef[3]*0+
                           sixth$coef[4]*0+sixth$coef[5]*0), 
                  plogis(quantile(pred.seventh[,1], c(0.05))+seventh$coef[2]*0+seventh$coef[3]*0+
                           seventh$coef[4]*0+seventh$coef[5]*0), 
                  plogis(quantile(pred.eighth[,1], c(0.05))+eighth$coef[2]*0+eighth$coef[3]*0+
                           eighth$coef[4]*0+eighth$coef[5]*0), 
                  plogis(quantile(pred.ninth[,1], c(0.05))+ninth$coef[2]*0+ninth$coef[3]*0+
                           ninth$coef[4]*0+ninth$coef[5]*0))

point.l.1=c(plogis(second$coef[1]+second$coef[2]*0+second$coef[3]*0+
                   second$coef[4]*0+second$coef[5]*0), 
          plogis(third$coef[1]+third$coef[2]*0+third$coef[3]*0+
                   third$coef[4]*0+third$coef[5]*0), 
          plogis(fourth$coef[1]+fourth$coef[2]*0+
                   fourth$coef[3]*0+fourth$coef[4]*0+
                   fourth$coef[5]*0), 
          plogis(fifth$coef[1]+fifth$coef[2]*0+fifth$coef[3]*0+
                   fifth$coef[4]*0+fifth$coef[5]*0), 
          plogis(sixth$coef[1]+sixth$coef[2]*0+sixth$coef[3]*0+
                   sixth$coef[4]*0+sixth$coef[5]*0), 
          plogis(seventh$coef[1]+seventh$coef[2]*0+seventh$coef[3]*0+
                   seventh$coef[4]*0+seventh$coef[5]*0), 
          plogis(eighth$coef[1]+eighth$coef[2]*0+eighth$coef[3]*0+
                   eighth$coef[4]*0+eighth$coef[5]*0), 
          plogis(ninth$coef[1]+ninth$coef[2]*0+ninth$coef[3]*0+
                   ninth$coef[4]*0+ninth$coef[5]*0))

majles=c(2,3,4,5,6,7,8,9)

plot.data.1<-data.frame(majles, lower.bound.c.1, point.c.1, upper.bound.c.1, 
                        lower.bound.l.1, point.l.1, upper.bound.l.1, means)

plot(plot.data.1$majles, plot.data.1$point.c.1, xlab="Majles", ylab="Probability", 
     main="Probability of Being Reelected \n(No College)", col="Red", pch=16, ylim=c(0, 1))

points(plot.data.1$majles, plot.data.1$lower.bound.c.1, 
       pch="-", col="red")

points(plot.data.1$majles, plot.data.1$upper.bound.c.1, add=TRUE, 
       pch="-", col="red")

points(plot.data.1$majles, plot.data.1$point.l.1, add=TRUE, 
       pch=16, col="blue")

points(plot.data.1$majles, plot.data.1$lower.bound.l.1, add=TRUE, 
       pch="-", col="blue")

points(plot.data.1$majles, plot.data.1$upper.bound.l.1, add=TRUE,
       pch="-", col="blue")

points(plot.data.1$majles, plot.data.1$means, pch=16, col="green")

legend(5, 1.1, legend=c("Clergy", "Non-Clergy", "Mean"), text.col="black", 
       col=c("red", "blue", "green"), pch=16, xpd=TRUE)

#Bacelors degree

upper.bound.c.2=c(plogis(second$coef[1]+second$coef[2]*1+second$coef[3]*0+
                           second$coef[4]*0+quantile(pred.second[,5], c(0.95))), 
                  plogis(third$coef[1]+third$coef[2]*1+third$coef[3]*0+
                           third$coef[4]*0+quantile(pred.third[,5], c(0.95))), 
                  plogis(fourth$coef[1]+fourth$coef[2]*1+fourth$coef[3]*0+
                           fourth$coef[4]*0+quantile(pred.fourth[,5], c(0.95))), 
                  plogis(fifth$coef[1]+fifth$coef[2]*1+fifth$coef[3]*0+
                           fifth$coef[4]*0+quantile(pred.fifth[,5], c(0.95))), 
                  plogis(sixth$coef[1]+sixth$coef[2]*1+sixth$coef[3]*0+
                           sixth$coef[4]*0+quantile(pred.sixth[,5], c(0.95))), 
                  plogis(seventh$coef[1]+seventh$coef[2]*1+seventh$coef[3]*0+
                           seventh$coef[4]*0+quantile(pred.seventh[,5], c(0.95))), 
                  plogis(eighth$coef[1]+eighth$coef[2]*1+eighth$coef[3]*0+
                           eighth$coef[4]*0+quantile(pred.eighth[,5], c(0.95))), 
                  plogis(ninth$coef[1]+ninth$coef[2]*1+ninth$coef[3]*0+
                           ninth$coef[4]*0+quantile(pred.ninth[,5], c(0.95))))

lower.bound.c.2=c(plogis(second$coef[1]+second$coef[2]*1+second$coef[3]*0+
                           second$coef[4]*0+quantile(pred.second[,5], c(0.05))), 
                  plogis(third$coef[1]+third$coef[2]*1+third$coef[3]*0+
                           third$coef[4]*0+quantile(pred.third[,5], c(0.05))), 
                  plogis(fourth$coef[1]+fourth$coef[2]*1+fourth$coef[3]*0+
                           fourth$coef[4]*0+quantile(pred.fourth[,5], c(0.05))), 
                  plogis(fifth$coef[1]+fifth$coef[2]*1+fifth$coef[3]*0+
                           fifth$coef[4]*0+quantile(pred.fifth[,5], c(0.05))), 
                  plogis(sixth$coef[1]+sixth$coef[2]*1+sixth$coef[3]*0+
                           sixth$coef[4]*0+quantile(pred.sixth[,5], c(0.05))), 
                  plogis(seventh$coef[1]+seventh$coef[2]*1+seventh$coef[3]*0+
                           seventh$coef[4]*0+quantile(pred.seventh[,5], c(0.05))), 
                  plogis(eighth$coef[1]+eighth$coef[2]*1+eighth$coef[3]*0+
                           eighth$coef[4]*0+quantile(pred.eighth[,5], c(0.05))), 
                  plogis(ninth$coef[1]+ninth$coef[2]*1+ninth$coef[3]*0+
                           ninth$coef[4]*0+quantile(pred.ninth[,5], c(0.05))))

point.c.2=c(plogis(second$coef[1]+second$coef[2]*1+second$coef[3]*0+
                     second$coef[4]*0+second$coef[5]*1), 
            plogis(third$coef[1]+third$coef[2]*1+third$coef[3]*0+
                     third$coef[4]*0+third$coef[5]*1), 
            plogis(fourth$coef[1]+fourth$coef[2]*1+
                     fourth$coef[3]*0+fourth$coef[4]*0+
                     fourth$coef[5]*1), 
            plogis(fifth$coef[1]+fifth$coef[2]*1+fifth$coef[3]*0+
                     fifth$coef[4]*0+fifth$coef[5]*1), 
            plogis(sixth$coef[1]+sixth$coef[2]*1+sixth$coef[3]*0+
                     sixth$coef[4]*0+sixth$coef[5]*1), 
            plogis(seventh$coef[1]+seventh$coef[2]*1+seventh$coef[3]*0+
                     seventh$coef[4]*0+seventh$coef[5]*1), 
            plogis(eighth$coef[1]+eighth$coef[2]*1+eighth$coef[3]*0+
                     eighth$coef[4]*0+eighth$coef[5]*1), 
            plogis(ninth$coef[1]+ninth$coef[2]*1+ninth$coef[3]*0+
                     ninth$coef[4]*0+ninth$coef[5]*1))

upper.bound.l.2=c(plogis(quantile(pred.second[,1], c(0.95))+second$coef[2]*1+second$coef[3]*0+
                           second$coef[4]*0+second$coef[5]*0), 
                  plogis(quantile(pred.third[,1], c(0.95))+third$coef[2]*1+third$coef[3]*0+
                           third$coef[4]*0+third$coef[5]*0), 
                  plogis(quantile(pred.fourth[,1], c(0.95))+fourth$coef[2]*1+fourth$coef[3]*0+
                           fourth$coef[4]*0+fourth$coef[5]*0), 
                  plogis(quantile(pred.fifth[,1], c(0.95))+fifth$coef[2]*1+fifth$coef[3]*0+
                           fifth$coef[4]*0+fifth$coef[5]*0), 
                  plogis(quantile(pred.sixth[,1], c(0.95))+sixth$coef[2]*1+sixth$coef[3]*0+
                           sixth$coef[4]*0+sixth$coef[5]*0), 
                  plogis(quantile(pred.seventh[,1], c(0.95))+seventh$coef[2]*1+seventh$coef[3]*0+
                           seventh$coef[4]*0+seventh$coef[5]*0), 
                  plogis(quantile(pred.eighth[,1], c(0.95))+eighth$coef[2]*1+eighth$coef[3]*0+
                           eighth$coef[4]*0+eighth$coef[5]*0), 
                  plogis(quantile(pred.ninth[,1], c(0.95))+ninth$coef[2]*1+ninth$coef[3]*0+
                           ninth$coef[4]*0+ninth$coef[5]*0))

lower.bound.l.2=c(plogis(quantile(pred.second[,1], c(0.05))+second$coef[2]*1+second$coef[3]*0+
                           second$coef[4]*0+second$coef[5]*0), 
                  plogis(quantile(pred.third[,1], c(0.05))+third$coef[2]*1+third$coef[3]*0+
                           third$coef[4]*0+third$coef[5]*0), 
                  plogis(quantile(pred.fourth[,1], c(0.05))+fourth$coef[2]*1+fourth$coef[3]*0+
                           fourth$coef[4]*0+fourth$coef[5]*0), 
                  plogis(quantile(pred.fifth[,1], c(0.05))+fifth$coef[2]*1+fifth$coef[3]*0+
                           fifth$coef[4]*0+fifth$coef[5]*0), 
                  plogis(quantile(pred.sixth[,1], c(0.05))+sixth$coef[2]*1+sixth$coef[3]*0+
                           sixth$coef[4]*0+sixth$coef[5]*0), 
                  plogis(quantile(pred.seventh[,1], c(0.05))+seventh$coef[2]*1+seventh$coef[3]*0+
                           seventh$coef[4]*0+seventh$coef[5]*0), 
                  plogis(quantile(pred.eighth[,1], c(0.05))+eighth$coef[2]*1+eighth$coef[3]*0+
                           eighth$coef[4]*0+eighth$coef[5]*0), 
                  plogis(quantile(pred.ninth[,1], c(0.05))+ninth$coef[2]*1+ninth$coef[3]*0+
                           ninth$coef[4]*0+ninth$coef[5]*0))

point.l.2=c(plogis(second$coef[1]+second$coef[2]*1+second$coef[3]*0+
                     second$coef[4]*0+second$coef[5]*0), 
            plogis(third$coef[1]+third$coef[2]*1+third$coef[3]*0+
                     third$coef[4]*0+third$coef[5]*0), 
            plogis(fourth$coef[1]+fourth$coef[2]*1+
                     fourth$coef[3]*0+fourth$coef[4]*0+
                     fourth$coef[5]*0), 
            plogis(fifth$coef[1]+fifth$coef[2]*1+fifth$coef[3]*0+
                     fifth$coef[4]*0+fifth$coef[5]*0), 
            plogis(sixth$coef[1]+sixth$coef[2]*1+sixth$coef[3]*0+
                     sixth$coef[4]*0+sixth$coef[5]*0), 
            plogis(seventh$coef[1]+seventh$coef[2]*1+seventh$coef[3]*0+
                     seventh$coef[4]*0+seventh$coef[5]*0), 
            plogis(eighth$coef[1]+eighth$coef[2]*1+eighth$coef[3]*0+
                     eighth$coef[4]*0+eighth$coef[5]*0), 
            plogis(ninth$coef[1]+ninth$coef[2]*1+ninth$coef[3]*0+
                     ninth$coef[4]*0+ninth$coef[5]*0))


plot.data.2<-data.frame(majles, lower.bound.c.2, point.c.2, upper.bound.c.2, 
                        lower.bound.l.2, point.l.2, upper.bound.l.2, means)

plot(plot.data.2$majles, plot.data.2$point.c.2, xlab="Majles", ylab="Probability", 
     main="Probability of Being Reelected \n(Bachelor's Degree)", col="Red", pch=16, ylim=c(0, 1))

points(plot.data.2$majles, plot.data.2$lower.bound.c.2, add=TRUE, 
       pch="-", col="red")

points(plot.data.1$majles, plot.data.2$upper.bound.c.2, add=TRUE, 
       pch="-", col="red")

points(plot.data.1$majles, plot.data.2$point.l.2, add=TRUE, 
       pch=16, col="blue")

points(plot.data.1$majles, plot.data.2$lower.bound.l.2, add=TRUE, 
       pch="-", col="blue")

points(plot.data.1$majles, plot.data.2$upper.bound.l.2, add=TRUE,
       pch="-", col="blue")

points(plot.data.2$majles, plot.data.2$means, pch=16, col="green")

legend(5, 1.1, legend=c("Clergy", "Non-Clergy", "Mean"), text.col="black", 
       col=c("red", "blue", "green"), pch=16, xpd=TRUE)

#Master's Degree

upper.bound.c.3=c(plogis(second$coef[1]+second$coef[2]*0+second$coef[3]*1+
                           second$coef[4]*0+quantile(pred.second[,5], c(0.95))), 
                  plogis(third$coef[1]+third$coef[2]*0+third$coef[3]*1+
                           third$coef[4]*0+quantile(pred.third[,5], c(0.95))), 
                  plogis(fourth$coef[1]+fourth$coef[2]*0+fourth$coef[3]*1+
                           fourth$coef[4]*0+quantile(pred.fourth[,5], c(0.95))), 
                  plogis(fifth$coef[1]+fifth$coef[2]*0+fifth$coef[3]*1+
                           fifth$coef[4]*0+quantile(pred.fifth[,5], c(0.95))), 
                  plogis(sixth$coef[1]+sixth$coef[2]*0+sixth$coef[3]*1+
                           sixth$coef[4]*0+quantile(pred.sixth[,5], c(0.95))), 
                  plogis(seventh$coef[1]+seventh$coef[2]*0+seventh$coef[3]*1+
                           seventh$coef[4]*0+quantile(pred.seventh[,5], c(0.95))), 
                  plogis(eighth$coef[1]+eighth$coef[2]*0+eighth$coef[3]*1+
                           eighth$coef[4]*0+quantile(pred.eighth[,5], c(0.95))), 
                  plogis(ninth$coef[1]+ninth$coef[2]*0+ninth$coef[3]*1+
                           ninth$coef[4]*0+quantile(pred.ninth[,5], c(0.95))))

lower.bound.c.3=c(plogis(second$coef[1]+second$coef[2]*0+second$coef[3]*1+
                           second$coef[4]*0+quantile(pred.second[,5], c(0.05))), 
                  plogis(third$coef[1]+third$coef[2]*0+third$coef[3]*1+
                           third$coef[4]*0+quantile(pred.third[,5], c(0.05))), 
                  plogis(fourth$coef[1]+fourth$coef[2]*0+fourth$coef[3]*1+
                           fourth$coef[4]*0+quantile(pred.fourth[,5], c(0.05))), 
                  plogis(fifth$coef[1]+fifth$coef[2]*0+fifth$coef[3]*1+
                           fifth$coef[4]*0+quantile(pred.fifth[,5], c(0.05))), 
                  plogis(sixth$coef[1]+sixth$coef[2]*0+sixth$coef[3]*1+
                           sixth$coef[4]*0+quantile(pred.sixth[,5], c(0.05))), 
                  plogis(seventh$coef[1]+seventh$coef[2]*0+seventh$coef[3]*1+
                           seventh$coef[4]*0+quantile(pred.seventh[,5], c(0.05))), 
                  plogis(eighth$coef[1]+eighth$coef[2]*0+eighth$coef[3]*1+
                           eighth$coef[4]*0+quantile(pred.eighth[,5], c(0.05))), 
                  plogis(ninth$coef[1]+ninth$coef[2]*0+ninth$coef[3]*1+
                           ninth$coef[4]*0+quantile(pred.ninth[,5], c(0.05))))

point.c.3=c(plogis(second$coef[1]+second$coef[2]*0+second$coef[3]*1+
                     second$coef[4]*0+second$coef[5]*1), 
            plogis(third$coef[1]+third$coef[2]*0+third$coef[3]*1+
                     third$coef[4]*0+third$coef[5]*1), 
            plogis(fourth$coef[1]+fourth$coef[2]*0+
                     fourth$coef[3]*1+fourth$coef[4]*0+
                     fourth$coef[5]*1), 
            plogis(fifth$coef[1]+fifth$coef[2]*0+fifth$coef[3]*1+
                     fifth$coef[4]*0+fifth$coef[5]*1), 
            plogis(sixth$coef[1]+sixth$coef[2]*0+sixth$coef[3]*1+
                     sixth$coef[4]*0+sixth$coef[5]*1), 
            plogis(seventh$coef[1]+seventh$coef[2]*0+seventh$coef[3]*1+
                     seventh$coef[4]*0+seventh$coef[5]*1), 
            plogis(eighth$coef[1]+eighth$coef[2]*0+eighth$coef[3]*1+
                     eighth$coef[4]*0+eighth$coef[5]*1), 
            plogis(ninth$coef[1]+ninth$coef[2]*0+ninth$coef[3]*1+
                     ninth$coef[4]*0+ninth$coef[5]*1))

upper.bound.l.3=c(plogis(quantile(pred.second[,1], c(0.95))+second$coef[2]*0+second$coef[3]*1+
                           second$coef[4]*0+second$coef[5]*0), 
                  plogis(quantile(pred.third[,1], c(0.95))+third$coef[2]*0+third$coef[3]*1+
                           third$coef[4]*0+third$coef[5]*0), 
                  plogis(quantile(pred.fourth[,1], c(0.95))+fourth$coef[2]*0+fourth$coef[3]*1+
                           fourth$coef[4]*0+fourth$coef[5]*0), 
                  plogis(quantile(pred.fifth[,1], c(0.95))+fifth$coef[2]*0+fifth$coef[3]*1+
                           fifth$coef[4]*0+fifth$coef[5]*0), 
                  plogis(quantile(pred.sixth[,1], c(0.95))+sixth$coef[2]*0+sixth$coef[3]*1+
                           sixth$coef[4]*0+sixth$coef[5]*0), 
                  plogis(quantile(pred.seventh[,1], c(0.95))+seventh$coef[2]*0+seventh$coef[3]*1+
                           seventh$coef[4]*0+seventh$coef[5]*0), 
                  plogis(quantile(pred.eighth[,1], c(0.95))+eighth$coef[2]*0+eighth$coef[3]*1+
                           eighth$coef[4]*0+eighth$coef[5]*0), 
                  plogis(quantile(pred.ninth[,1], c(0.95))+ninth$coef[2]*0+ninth$coef[3]*1+
                           ninth$coef[4]*0+ninth$coef[5]*0))

lower.bound.l.3=c(plogis(quantile(pred.second[,1], c(0.05))+second$coef[2]*0+second$coef[3]*1+
                           second$coef[4]*0+second$coef[5]*0), 
                  plogis(quantile(pred.third[,1], c(0.05))+third$coef[2]*0+third$coef[3]*1+
                           third$coef[4]*0+third$coef[5]*0), 
                  plogis(quantile(pred.fourth[,1], c(0.05))+fourth$coef[2]*0+fourth$coef[3]*1+
                           fourth$coef[4]*0+fourth$coef[5]*0), 
                  plogis(quantile(pred.fifth[,1], c(0.05))+fifth$coef[2]*0+fifth$coef[3]*1+
                           fifth$coef[4]*0+fifth$coef[5]*0), 
                  plogis(quantile(pred.sixth[,1], c(0.05))+sixth$coef[2]*0+sixth$coef[3]*1+
                           sixth$coef[4]*0+sixth$coef[5]*0), 
                  plogis(quantile(pred.seventh[,1], c(0.05))+seventh$coef[2]*0+seventh$coef[3]*1+
                           seventh$coef[4]*0+seventh$coef[5]*0), 
                  plogis(quantile(pred.eighth[,1], c(0.05))+eighth$coef[2]*0+eighth$coef[3]*1+
                           eighth$coef[4]*0+eighth$coef[5]*0), 
                  plogis(quantile(pred.ninth[,1], c(0.05))+ninth$coef[2]*0+ninth$coef[3]*1+
                           ninth$coef[4]*0+ninth$coef[5]*0))

point.l.3=c(plogis(second$coef[1]+second$coef[2]*0+second$coef[3]*1+
                     second$coef[4]*0+second$coef[5]*0), 
            plogis(third$coef[1]+third$coef[2]*0+third$coef[3]*1+
                     third$coef[4]*0+third$coef[5]*0), 
            plogis(fourth$coef[1]+fourth$coef[2]*0+
                     fourth$coef[3]*1+fourth$coef[4]*0+
                     fourth$coef[5]*0), 
            plogis(fifth$coef[1]+fifth$coef[2]*0+fifth$coef[3]*1+
                     fifth$coef[4]*0+fifth$coef[5]*0), 
            plogis(sixth$coef[1]+sixth$coef[2]*0+sixth$coef[3]*1+
                     sixth$coef[4]*0+sixth$coef[5]*0), 
            plogis(seventh$coef[1]+seventh$coef[2]*0+seventh$coef[3]*1+
                     seventh$coef[4]*0+seventh$coef[5]*0), 
            plogis(eighth$coef[1]+eighth$coef[2]*0+eighth$coef[3]*1+
                     eighth$coef[4]*0+eighth$coef[5]*0), 
            plogis(ninth$coef[1]+ninth$coef[2]*0+ninth$coef[3]*1+
                     ninth$coef[4]*0+ninth$coef[5]*0))


plot.data.3<-data.frame(majles, lower.bound.c.3, point.c.3, upper.bound.c.3, 
                        lower.bound.l.3, point.l.3, upper.bound.l.3, means)

plot(plot.data.3$majles, plot.data.3$point.c.3, xlab="Majles", ylab="Probability", 
     main="Probability of Being Reelected \n(Master's Degree)", col="Red", pch=16, ylim=c(0, 1))

points(plot.data.3$majles, plot.data.3$lower.bound.c.3, add=TRUE, 
       pch="-", col="red")

points(plot.data.3$majles, plot.data.3$upper.bound.c.3, add=TRUE, 
       pch="-", col="red")

points(plot.data.3$majles, plot.data.3$point.l.3, add=TRUE, 
       pch=16, col="blue")

points(plot.data.3$majles, plot.data.3$lower.bound.l.3, add=TRUE, 
       pch="-", col="blue")

points(plot.data.3$majles, plot.data.3$upper.bound.l.3, add=TRUE,
       pch="-", col="blue")

points(plot.data.3$majles, plot.data.3$means, pch=16, col="green")

legend(5, 1.1, legend=c("Clergy", "Non-Clergy", "Mean"), text.col="black", 
       col=c("red", "blue", "green"), pch=16, xpd=TRUE)

#Doctorate Degree

upper.bound.c.4=c(plogis(second$coef[1]+second$coef[2]*0+second$coef[3]*0+
                           second$coef[4]*1+quantile(pred.second[,5], c(0.95))), 
                  plogis(third$coef[1]+third$coef[2]*0+third$coef[3]*0+
                           third$coef[4]*1+quantile(pred.third[,5], c(0.95))), 
                  plogis(fourth$coef[1]+fourth$coef[2]*0+fourth$coef[3]*0+
                           fourth$coef[4]*1+quantile(pred.fourth[,5], c(0.95))), 
                  plogis(fifth$coef[1]+fifth$coef[2]*0+fifth$coef[3]*0+
                           fifth$coef[4]*1+quantile(pred.fifth[,5], c(0.95))), 
                  plogis(sixth$coef[1]+sixth$coef[2]*0+sixth$coef[3]*0+
                           sixth$coef[4]*0+quantile(pred.sixth[,5], c(0.95))), 
                  plogis(seventh$coef[1]+seventh$coef[2]*0+seventh$coef[3]*0+
                           seventh$coef[4]*1+quantile(pred.seventh[,5], c(0.95))), 
                  plogis(eighth$coef[1]+eighth$coef[2]*0+eighth$coef[3]*0+
                           eighth$coef[4]*1+quantile(pred.eighth[,5], c(0.95))), 
                  plogis(ninth$coef[1]+ninth$coef[2]*0+ninth$coef[3]*0+
                           ninth$coef[4]*1+quantile(pred.ninth[,5], c(0.95))))

lower.bound.c.4=c(plogis(second$coef[1]+second$coef[2]*0+second$coef[3]*0+
                           second$coef[4]*1+quantile(pred.second[,5], c(0.05))), 
                  plogis(third$coef[1]+third$coef[2]*0+third$coef[3]*0+
                           third$coef[4]*1+quantile(pred.third[,5], c(0.05))), 
                  plogis(fourth$coef[1]+fourth$coef[2]*0+fourth$coef[3]*0+
                           fourth$coef[4]*1+quantile(pred.fourth[,5], c(0.05))), 
                  plogis(fifth$coef[1]+fifth$coef[2]*0+fifth$coef[3]*0+
                           fifth$coef[4]*1+quantile(pred.fifth[,5], c(0.05))), 
                  plogis(sixth$coef[1]+sixth$coef[2]*0+sixth$coef[3]*0+
                           sixth$coef[4]*1+quantile(pred.sixth[,5], c(0.05))), 
                  plogis(seventh$coef[1]+seventh$coef[2]*0+seventh$coef[3]*0+
                           seventh$coef[4]*1+quantile(pred.seventh[,5], c(0.05))), 
                  plogis(eighth$coef[1]+eighth$coef[2]*0+eighth$coef[3]*0+
                           eighth$coef[4]*1+quantile(pred.eighth[,5], c(0.05))), 
                  plogis(ninth$coef[1]+ninth$coef[2]*0+ninth$coef[3]*0+
                           ninth$coef[4]*1+quantile(pred.ninth[,5], c(0.05))))

point.c.4=c(plogis(second$coef[1]+second$coef[2]*0+second$coef[3]*0+
                     second$coef[4]*1+second$coef[5]*1), 
            plogis(third$coef[1]+third$coef[2]*0+third$coef[3]*0+
                     third$coef[4]*1+third$coef[5]*1), 
            plogis(fourth$coef[1]+fourth$coef[2]*0+
                     fourth$coef[3]*0+fourth$coef[4]*1+
                     fourth$coef[5]*1), 
            plogis(fifth$coef[1]+fifth$coef[2]*0+fifth$coef[3]*0+
                     fifth$coef[4]*1+fifth$coef[5]*1), 
            plogis(sixth$coef[1]+sixth$coef[2]*0+sixth$coef[3]*0+
                     sixth$coef[4]*1+sixth$coef[5]*1), 
            plogis(seventh$coef[1]+seventh$coef[2]*0+seventh$coef[3]*0+
                     seventh$coef[4]*1+seventh$coef[5]*1), 
            plogis(eighth$coef[1]+eighth$coef[2]*0+eighth$coef[3]*0+
                     eighth$coef[4]*1+eighth$coef[5]*1), 
            plogis(ninth$coef[1]+ninth$coef[2]*0+ninth$coef[3]*0+
                     ninth$coef[4]*1+ninth$coef[5]*1))

upper.bound.l.4=c(plogis(quantile(pred.second[,1], c(0.95))+second$coef[2]*0+second$coef[3]*0+
                           second$coef[4]*1+second$coef[5]*0), 
                  plogis(quantile(pred.third[,1], c(0.95))+third$coef[2]*0+third$coef[3]*0+
                           third$coef[4]*1+third$coef[5]*0), 
                  plogis(quantile(pred.fourth[,1], c(0.95))+fourth$coef[2]*0+fourth$coef[3]*0+
                           fourth$coef[4]*1+fourth$coef[5]*0), 
                  plogis(quantile(pred.fifth[,1], c(0.95))+fifth$coef[2]*0+fifth$coef[3]*0+
                           fifth$coef[4]*1+fifth$coef[5]*0), 
                  plogis(quantile(pred.sixth[,1], c(0.95))+sixth$coef[2]*0+sixth$coef[3]*0+
                           sixth$coef[4]*1+sixth$coef[5]*0), 
                  plogis(quantile(pred.seventh[,1], c(0.95))+seventh$coef[2]*0+seventh$coef[3]*0+
                           seventh$coef[4]*1+seventh$coef[5]*0), 
                  plogis(quantile(pred.eighth[,1], c(0.95))+eighth$coef[2]*0+eighth$coef[3]*0+
                           eighth$coef[4]*1+eighth$coef[5]*0), 
                  plogis(quantile(pred.ninth[,1], c(0.95))+ninth$coef[2]*0+ninth$coef[3]*0+
                           ninth$coef[4]*1+ninth$coef[5]*0))

lower.bound.l.4=c(plogis(quantile(pred.second[,1], c(0.05))+second$coef[2]*0+second$coef[3]*0+
                           second$coef[4]*1+second$coef[5]*0), 
                  plogis(quantile(pred.third[,1], c(0.05))+third$coef[2]*0+third$coef[3]*0+
                           third$coef[4]*1+third$coef[5]*0), 
                  plogis(quantile(pred.fourth[,1], c(0.05))+fourth$coef[2]*0+fourth$coef[3]*0+
                           fourth$coef[4]*1+fourth$coef[5]*0), 
                  plogis(quantile(pred.fifth[,1], c(0.05))+fifth$coef[2]*0+fifth$coef[3]*0+
                           fifth$coef[4]*1+fifth$coef[5]*0), 
                  plogis(quantile(pred.sixth[,1], c(0.05))+sixth$coef[2]*0+sixth$coef[3]*0+
                           sixth$coef[4]*1+sixth$coef[5]*0), 
                  plogis(quantile(pred.seventh[,1], c(0.05))+seventh$coef[2]*0+seventh$coef[3]*0+
                           seventh$coef[4]*1+seventh$coef[5]*0), 
                  plogis(quantile(pred.eighth[,1], c(0.05))+eighth$coef[2]*0+eighth$coef[3]*0+
                           eighth$coef[4]*1+eighth$coef[5]*0), 
                  plogis(quantile(pred.ninth[,1], c(0.05))+ninth$coef[2]*0+ninth$coef[3]*0+
                           ninth$coef[4]*1+ninth$coef[5]*0))

point.l.4=c(plogis(second$coef[1]+second$coef[2]*0+second$coef[3]*0+
                     second$coef[4]*1+second$coef[5]*0), 
            plogis(third$coef[1]+third$coef[2]*0+third$coef[3]*0+
                     third$coef[4]*1+third$coef[5]*0), 
            plogis(fourth$coef[1]+fourth$coef[2]*0+
                     fourth$coef[3]*0+fourth$coef[4]*1+
                     fourth$coef[5]*0), 
            plogis(fifth$coef[1]+fifth$coef[2]*0+fifth$coef[3]*0+
                     fifth$coef[4]*1+fifth$coef[5]*0), 
            plogis(sixth$coef[1]+sixth$coef[2]*0+sixth$coef[3]*0+
                     sixth$coef[4]*1+sixth$coef[5]*0), 
            plogis(seventh$coef[1]+seventh$coef[2]*0+seventh$coef[3]*0+
                     seventh$coef[4]*1+seventh$coef[5]*0), 
            plogis(eighth$coef[1]+eighth$coef[2]*0+eighth$coef[3]*0+
                     eighth$coef[4]*1+eighth$coef[5]*0), 
            plogis(ninth$coef[1]+ninth$coef[2]*0+ninth$coef[3]*0+
                     ninth$coef[4]*1+ninth$coef[5]*0))


plot.data.4<-data.frame(majles, lower.bound.c.4, point.c.4, upper.bound.c.4, 
                        lower.bound.l.4, point.l.4, upper.bound.l.4, means)

plot(plot.data.4$majles, plot.data.4$point.c.4, xlab="Majles", ylab="Probability", 
     main="Probability of Being Reelected \n(Doctorate Degree)", col="Red", pch=16, ylim=c(0, 1))

points(plot.data.4$majles, plot.data.4$lower.bound.c.4, add=TRUE, 
       pch="-", col="red")

points(plot.data.4$majles, plot.data.4$upper.bound.c.4, add=TRUE, 
       pch="-", col="red")

points(plot.data.4$majles, plot.data.4$point.l.4, add=TRUE, 
       pch=16, col="blue")

points(plot.data.4$majles, plot.data.4$lower.bound.l.4, add=TRUE, 
       pch="-", col="blue")

points(plot.data.4$majles, plot.data.4$upper.bound.l.4, add=TRUE,
       pch="-", col="blue")

points(plot.data.3$majles, plot.data.3$means, pch=16, col="green")

legend(5, 1.1, legend=c("Clergy", "Non-Clergy", "Mean"), text.col="black", 
       col=c("red", "blue", "green"), pch=16, xpd=TRUE)



#Mean comparisons (across majleses)

t.test(means, point.c.1, data=plot.data.1)
t.test(means, point.l.1, data=plot.data.1)
t.test(means, point.c.2, data=plot.data.2)
t.test(means, point.l.2, data=plot.data.2)
t.test(means, point.c.3, data=plot.data.3)
t.test(means, point.l.3, data=plot.data.3)
t.test(means, point.c.4, data=plot.data.4)
t.test(means, point.l.4, data=plot.data.4)




