
dat.area <- fread("area
1
1
1
1
1
1
1
1
1
1
1
2
2
2
2
2
2
2
2
2
2
2")

dat.age <- fread("age
15
16
17
18
19
20
21
22
23
24
25
25
26
27
28
29
30
31
32
33
34
35")

dat <- cbind(dat.age, dat.area)
dat

var_w <- dat[, (age - mean(age)) ^ 2, by = .(area)][, sum(V1)]
# 220

var_b <- dat[, .(n = .N, y = mean(age)), by = .(area)][, sum(n * (y - mean(y)) ^ 2)]
# 550

dat[, sum((age - mean(age)) ^ 2)]

n <- dat[, .N, by = .(area)][, mean(N)]
# 11

var_b / (var_b + var_w)

(var_b - n) / (var_b + (n - 1) * n)


tdata <- copy(dat[])
tdata[, area := factor(area)]
setDF(tdata)

a <- length(unique(tdata$area))

fmla <- formula(tdata)
!is.list(replications(fmla, tdata))

# tmp1 <- aggregate(tdata[, 1], list(tdata[, 2]), FUN = mean)
# tmp2 <- aggregate(tdata[, 1], list(tdata[, 2]), FUN = length)
# ord.data <- tdata[order(tdata[, 2]),]
# Treat.m <- rep(tmp1$x, tmp2$x)
# Among <- (Treat.m - rep(mean(tdata[, 1]), nrow(tdata)))^2
# Within <- (ord.data[, 1] - Treat.m)^2
# MS <- c(sum(Among), sum(Within)) / c(length(tmp2$x) - 1, length(tmp2$x) * (tmp2$x[1]-1))
# var.a <- (MS[1] - MS[2]) / tmp2$x[1]
# var.a / (var.a + MS[2])

tmpbb <- anova(aov(fmla, data = tdata))
tmpbb[3]

MSa <- tmpbb[3][1, 1]
tmp.outj <- aggregate(fmla, data = tdata, FUN = length)$age
var.a <- (MSa - tmpbb[3][2, 1]) / ((1 / (a - 1)) * (sum(tmp.outj) - (sum(tmp.outj ^ 2) / sum(tmp.outj))))
var.a / (tmpbb[3][2,1] + var.a)

tab.anova <- anova(aov(age ~ area, data = dat))
MeanSq <- tab.anova$`Mean Sq`

tab.psu <- dat[, .N, keyby = .(area)]
psu.size <- tab.psu[, N]
psu.count <- tab.psu[, .N]

var.a <- (MeanSq[1] - MeanSq[2]) /
  ((1 / (psu.count - 1)) * (sum(psu.size) - (sum(psu.size ^ 2) / sum(psu.size))))
var.a / (MeanSq[2] + var.a)
