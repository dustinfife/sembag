require(patchwork)
require(tidyverse)
require(flexplot)
# start with an efa
rm(list=ls())
tail(uni)
d = uni %>%
  select(starts_with("x")) %>%
  na.exclude()
nrow(d)

  # unsupervised RF
require(randomForest)
rf_unsup = randomForest(d%>%t, ntree=2000, proximity = T)
prox_mat = rf_unsup$proximity
importance(rf_unsup)

km = kmeans(prox_mat, 2)
pred.km = cbind(km$cluster, )

?cforest
rf_unsup = party::cforest(~x1 + x2 + x3 + x4, data=d)
prox_mat = rf_unsup$proximity
importance(rf_unsup)

km = kmeans(prox_mat, 2)
pred.km = cbind(km$cluster, )

# but RF doesn't work with my data because it's ignoring extremes

# factor analysis on proximity matrix?
factanal(Matrix::nearPD(prox_mat)$mat, factors=1)
d = uni
fa_results = factanal(uni%>%select(starts_with("x")), factors=2, scores = "Bartlett")
d$dv = fa_results$scores[,1]

# do rf
require(party)
rf_mod = cforest(dv~., data=d %>% select(starts_with("x"), dv))
est_rf = estimates(rf_mod)
rf_reduced = cforest(dv~x1 + x3 + x1 + x5 + x8, data=d)
#predicted =
a = compare.fits(dv~x5|x8, data=d, rf_reduced)
b = flexplot(latent~x5|x8, data=d)
a+b

a = flexplot(dv~x13, data=uni)
b = flexplot(latent~x13, data=uni)
a+b









#










uni$dv = fa_results$scores[,1]


iterations = 1
loops = 100
dv_raw = matrix(nrow=nrow(uni), ncol=loops)
dv_ma  = dv_raw
r = 1:iterations

current = fa_results$scores[,1]
i = 2


for (j in 1:iterations) {
  for (i in 1:loops) {
    # make a decision tree
    require(rpart)

    #randomly select variables
    vars = select(uni, starts_with("x")) %>% names
    vars_i = sample(vars, sqrt(length(vars)))
    form_i = paste0("dv~", paste0(vars_i, collapse="+"))
    tree = rpart(form_i, data=uni%>%select(-latent))


    dv_raw[,i] = predict(tree)
    #r[i] = cor(dv_raw[,i], uni$latent)

    cat(paste0("Iteration ", i, " of ", iterations, "\n"))
  }
  r[j] = cor(rowMeans(dv_raw), uni$latent)
  uni$dv_final = rowMeans(dv_raw)
}



a = flexplot(latent~x6 | x9, data=uni)+ labs(title="Latent")
b = flexplot(dv~x6 | x9, data=uni) + labs(title="FA")
c = flexplot(dv_final~x6 | x9, data=uni) + labs(title="CART")
require(patchwork)
a + b + c

a = flexplot(latent~x11, data=uni) + labs(title="Latent")
b = flexplot(dv~x11, data=uni) + labs(title="FA")
c = flexplot(dv_final~x11, data=uni) + labs(title="CART")
require(patchwork)
a + b + c



  # it picks up on an interaction
flexplot(latent~x11, data=uni)
flexplot(dv~x13, data=uni)
  # and picks up on interaction too!

plot(1:j, r)
#it's getting worse over time???
# it's predicting a prediction...there's going to be degradation

new_dv = rowMeans(dv_raw)

plot(rowMeans(dv_raw), uni$latent)

plot(uni$dv, uni$latent)
head(dv_raw)
head(dv_ma)

plot(1:100, r)
