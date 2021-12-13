library(tidyverse)

output <- data.frame(matrix(nrow = 12, ncol = 4))
colnames(output) <- c("RR", "RL", "LR", "LL")

data.hand <- data.hand.bio
data.foot <- data.foot.bio
data.eye <- data.eye.bio

output[1,]  <- c(round(count(subset(data.hand, m.hand == 0 & p.hand == 0), hand != 0)$n[2]/count(subset(data.hand, m.hand == 0 & p.hand == 0))*100,2),
                 round(count(subset(data.hand, m.hand == 0 & p.hand != 0), hand != 0)$n[2]/count(subset(data.hand, m.hand == 0 & p.hand != 0))*100,2),
                 round(count(subset(data.hand, m.hand != 0 & p.hand == 0), hand != 0)$n[2]/count(subset(data.hand, m.hand != 0 & p.hand == 0))*100,2),
                 round(count(subset(data.hand, m.hand != 0 & p.hand != 0), hand != 0)$n[2]/count(subset(data.hand, m.hand != 0 & p.hand != 0))*100,2))

output[2,] <- c(round(count(subset(data.hand, m.hand == 0 & p.hand == 0), hand != 0)$n[2],0),
                round(count(subset(data.hand, m.hand == 0 & p.hand != 0), hand != 0)$n[2],0),
                round(count(subset(data.hand, m.hand != 0 & p.hand == 0), hand != 0)$n[2],0),
                round(count(subset(data.hand, m.hand != 0 & p.hand != 0), hand != 0)$n[2],0))

output[3,]  <- c(round(count(subset(data.foot, m.foot == 0 & p.foot == 0), foot != 0)$n[2]/count(subset(data.foot, m.foot == 0 & p.foot == 0))*100,2),
                 round(count(subset(data.foot, m.foot == 0 & p.foot != 0), foot != 0)$n[2]/count(subset(data.foot, m.foot == 0 & p.foot != 0))*100,2),
                 round(count(subset(data.foot, m.foot != 0 & p.foot == 0), foot != 0)$n[2]/count(subset(data.foot, m.foot != 0 & p.foot == 0))*100,2),
                 round(count(subset(data.foot, m.foot != 0 & p.foot != 0), foot != 0)$n[2]/count(subset(data.foot, m.foot != 0 & p.foot != 0))*100,2))

output[4,]  <- c(round(count(subset(data.foot, m.foot == 0 & p.foot == 0), foot != 0)$n[2],0),
                 round(count(subset(data.foot, m.foot == 0 & p.foot != 0), foot != 0)$n[2],0),
                 round(count(subset(data.foot, m.foot != 0 & p.foot == 0), foot != 0)$n[2],0),
                 round(count(subset(data.foot, m.foot != 0 & p.foot != 0), foot != 0)$n[2],0))

output[5,]  <- c(round(count(subset(data.eye, m.eye == 0 & p.eye == 0), eye != 0)$n[2]/count(subset(data.eye, m.eye == 0 & p.eye == 0))*100,2),
                 round(count(subset(data.eye, m.eye == 0 & p.eye != 0), eye != 0)$n[2]/count(subset(data.eye, m.eye == 0 & p.eye != 0))*100,2),
                 round(count(subset(data.eye, m.eye != 0 & p.eye == 0), eye != 0)$n[2]/count(subset(data.eye, m.eye != 0 & p.eye == 0))*100,2),
                 round(count(subset(data.eye, m.eye != 0 & p.eye != 0), eye != 0)$n[2]/count(subset(data.eye, m.eye != 0 & p.eye != 0))*100,2))

output[6,]  <- c(round(count(subset(data.eye, m.eye == 0 & p.eye == 0), eye != 0)$n[2],0),
                 round(count(subset(data.eye, m.eye == 0 & p.eye != 0), eye != 0)$n[2],0),
                 round(count(subset(data.eye, m.eye != 0 & p.eye == 0), eye != 0)$n[2],0),
                 round(count(subset(data.eye, m.eye != 0 & p.eye != 0), eye != 0)$n[2],0))

output[7,]  <- c(round(count(subset(data.hand, m.hand.lr == 0 & p.hand.lr == 0), hand.lr != 0)$n[2]/count(subset(data.hand, m.hand.lr == 0 & p.hand.lr == 0))*100,2),
                 round(count(subset(data.hand, m.hand.lr == 0 & p.hand.lr != 0), hand.lr != 0)$n[2]/count(subset(data.hand, m.hand.lr == 0 & p.hand.lr != 0))*100,2),
                 round(count(subset(data.hand, m.hand.lr != 0 & p.hand.lr == 0), hand.lr != 0)$n[2]/count(subset(data.hand, m.hand.lr != 0 & p.hand.lr == 0))*100,2),
                 round(count(subset(data.hand, m.hand.lr != 0 & p.hand.lr != 0), hand.lr != 0)$n[2]/count(subset(data.hand, m.hand.lr != 0 & p.hand.lr != 0))*100,2))

output[8,] <- c(round(count(subset(data.hand, m.hand.lr == 0 & p.hand.lr == 0), hand.lr != 0)$n[2],0),
                round(count(subset(data.hand, m.hand.lr == 0 & p.hand.lr != 0), hand.lr != 0)$n[2],0),
                round(count(subset(data.hand, m.hand.lr != 0 & p.hand.lr == 0), hand.lr != 0)$n[2],0),
                round(count(subset(data.hand, m.hand.lr != 0 & p.hand.lr != 0), hand.lr != 0)$n[2],0))

output[9,]  <- c(round(count(subset(data.foot, m.foot.lr == 0 & p.foot.lr == 0), foot.lr != 0)$n[2]/count(subset(data.foot, m.foot.lr == 0 & p.foot.lr == 0))*100,2),
                 round(count(subset(data.foot, m.foot.lr == 0 & p.foot.lr != 0), foot.lr != 0)$n[2]/count(subset(data.foot, m.foot.lr == 0 & p.foot.lr != 0))*100,2),
                 round(count(subset(data.foot, m.foot.lr != 0 & p.foot.lr == 0), foot.lr != 0)$n[2]/count(subset(data.foot, m.foot.lr != 0 & p.foot.lr == 0))*100,2),
                 round(count(subset(data.foot, m.foot.lr != 0 & p.foot.lr != 0), foot.lr != 0)$n[2]/count(subset(data.foot, m.foot.lr != 0 & p.foot.lr != 0))*100,2))

output[10,]  <- c(round(count(subset(data.foot, m.foot.lr == 0 & p.foot.lr == 0), foot.lr != 0)$n[2],0),
                  round(count(subset(data.foot, m.foot.lr == 0 & p.foot.lr != 0), foot.lr != 0)$n[2],0),
                  round(count(subset(data.foot, m.foot.lr != 0 & p.foot.lr == 0), foot.lr != 0)$n[2],0),
                  round(count(subset(data.foot, m.foot.lr != 0 & p.foot.lr != 0), foot.lr != 0)$n[2],0))

output[11,]  <- c(round(count(subset(data.eye, m.eye.lr == 0 & p.eye.lr == 0), eye.lr != 0)$n[2]/count(subset(data.eye, m.eye.lr == 0 & p.eye.lr == 0))*100,2),
                  round(count(subset(data.eye, m.eye.lr == 0 & p.eye.lr != 0), eye.lr != 0)$n[2]/count(subset(data.eye, m.eye.lr == 0 & p.eye.lr != 0))*100,2),
                  round(count(subset(data.eye, m.eye.lr != 0 & p.eye.lr == 0), eye.lr != 0)$n[2]/count(subset(data.eye, m.eye.lr != 0 & p.eye.lr == 0))*100,2),
                  round(count(subset(data.eye, m.eye.lr != 0 & p.eye.lr != 0), eye.lr != 0)$n[2]/count(subset(data.eye, m.eye.lr != 0 & p.eye.lr != 0))*100,2))

output[12,]  <- c(round(count(subset(data.eye, m.eye.lr == 0 & p.eye.lr == 0), eye.lr != 0)$n[2],0),
                  round(count(subset(data.eye, m.eye.lr == 0 & p.eye.lr != 0), eye.lr != 0)$n[2],0),
                  round(count(subset(data.eye, m.eye.lr != 0 & p.eye.lr == 0), eye.lr != 0)$n[2],0),
                  round(count(subset(data.eye, m.eye.lr != 0 & p.eye.lr != 0), eye.lr != 0)$n[2],0))

write.table(output, file = "outputs/Table_S2_S3.csv", sep = ",", col.names = NA, qmethod = "double")

write.table(output, file = "outputs/Table_S4_S5.csv", sep = ",", col.names = NA, qmethod = "double")
