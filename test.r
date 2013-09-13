repf <- "D:/L/alb/2012/assessment/Runs/a0.ref/plot-08.par.rep"
repf <- "D:/L/skj/2011/Results/plot-17.par.rep"
repf <- "D:/L/bet/2011/Model_runs/Run3j/plot-11.par.rep"
repf <- "L:/swo/2013/assessment/Model_runs/Ref.case/plot-10.par.rep"
a <- read.rep(repf)
a$yrs
a$alltimes
#plot(1:length(sort(unique(as.vector(a$Rlz.t.fsh)))),sort(unique(as.vector(a$Rlz.t.fsh))))
