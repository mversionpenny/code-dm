data=read.table("VisaPremier.txt",header=TRUE,na.strings =".")

# index of categorical features
ind_categ_feature=c(1,2,3,4,6,8,9,45,46,47)
# extraction of only the categ. features
data_categ=data[,ind_categ_feature]

# extraction of only the continuous features
data_continuous=data[,-ind_categ_feature]

# first have a look on the categorical feature
summary(data_categ)
# we can see that there is some NA (in departem and codeqlt), and some features are registred as integer
# whereas factor
# we will replace missing data for departem and after transform them into factor
tmp=sort(table(data_categ$departem),decreasing=TRUE)
data_categ$departem[is.na(data_categ$departem)]=as.integer(names(tmp)[1])
data_categ$departem=factor(data_categ$departem)

# we will replace missing data for codeqlt
data_categ$codeqlt=as.character(data_categ$codeqlt)
tmp=sort(table(data_categ$codeqlt),decreasing=TRUE)
data_categ$codeqlt[is.na(data_categ$codeqlt)]=names(tmp)[1]
data_categ$codeqlt=as.factor(data_categ$codeqlt)

# we transform matricul and ptvente into factor
data_categ$matricul=factor(data_categ$matricul) 
data_categ$ptvente=factor(data_categ$ptvente)

# the variable sexer and cartevpr are recoding of the variable cartevp and sexe,
# we don't need them
data_categ$sexer=NULL
data_categ$cartevpr=NULL
# we remove also the variable corresponding to the id of the customer
data_categ$matricul=NULL

# for the continous variables, we automatically replace the NA
summary(data_continuous)
for (j in 1:ncol(data_continuous)){
  if (sum(is.na(data_continuous[,j]))>0) data_continuous[is.na(data_continuous[,j]),j]=mean(data_continuous[,j],na.rm=TRUE)
}
summary(data_continuous)

# we remove the variable nbimpaye with is constant
data_continuous$nbimpaye=NULL

# PCAmix on categ + continuous features
library(PCAmixdata)
res=PCAmix(data_continuous,data_categ,rename.level=TRUE,ndim=50)
# we can remove the 2nd individual who is too different from the others
data_continuous=data_continuous[-2,]
data_categ=data_categ[-2,]

# when removing this customer, the variable mtbon and nbbon become constant,
# we can remove them
data_continuous$mtbon=NULL
data_continuous$nbbon=NULL
# we can redo the PCAmix analysis
res=PCAmix(data_continuous,data_categ,rename.level=TRUE,ndim=50)
# we can also plot the individual with different color following
# they have bought or not the VisaPremier credit card
plot(res,choice="ind",coloring.ind=data_categ$cartevp,posleg="bottomright")
plot(res,choice="cor")

# ======= Prediction ======

# --- selection de 1/3 des donnees pour l'echantillon test ---
ind_test=sample(1:nrow(data_continuous),nrow(data_continuous)/3)
data_continuous_test=data_continuous[ind_test,]
data_continuous_train=data_continuous[-ind_test,]
data_categ_test=data_categ[ind_test,]
data_categ_train=data_categ[-ind_test,]

for (j in 1:ncol(data_continuous_train)) print(length(levels(as.factor(data_continuous_train[,j]))))
data_continuous_train$mteparte=NULL # variable constante à l'interieur d'un groupe
data_continuous_test$mteparte=NULL # variable constante à l'interieur d'un groupe

# ============== LDA ===================
# --- LDA sur variables quanti ----
library('MASS')
df_continuous_train=data.frame(data_continuous_train,data_categ_train$cartevp)
mod4=lda(data_categ_train.cartevp~.,data=df_continuous_train)
summary(mod4)
# attention certaine variables sont colinéaires, ce qui empéchera QDA de fonctionner
# prediction de l'echantillon test
p=predict(object = mod4,newdata = data_continuous_test)

taux_bon_class_LDA=mean(p$class==data_categ_test$cartevp)
cat('Taux de bon classement par LDA sur les variables quanti :',taux_bon_class_LDA,'\n')

# --- QDA sur variables quanti ----
# mod5=qda(data_categ_train.cartevp~.,data=df_continuous_train)
# ne fonctionne pas, variable trop colineaires
# il faudrait en supprimer certaines

# ======== Modèles de mélanges gaussiens MCLUST ============
library(mclust)
# --- MclustDA sur variables quanti ----
# ---- on laisse MclustDA choisir (suivant BIC) le meilleur modele pour chaque classe
mod6=MclustDA(data_continuous_train,data_categ_train$cartevp)
summary(mod6)
p=predict(object = mod6,newdata = data_continuous_test)
taux_bon_class_MclustDA=sum(p$classification==data_categ_test$cartevp)/nrow(data_categ_test)
cat('Taux de bon classement par MclustDA sur les variables quanti :',taux_bon_class_MclustDA,'\n')
print(table(data_categ_test$cartevp,p$classification))

# ---- on choisit un modele particulier
mod6=MclustDA(data_continuous_train,data_categ_train$cartevp, modelType = "EDDA", modelNames = "EEE")
p=predict(object = mod6,newdata = data_continuous_test)
taux_bon_class_MclustDA=sum(p$classification==data_categ_test$cartevp)/nrow(data_categ_test)
cat('Taux de bon classement par MclustDA sur les variables quanti :',taux_bon_class_MclustDA,'\n')
print(table(data_categ_test$cartevp,p$classification))
# les résultats semblent meilleurs avec ce modèles,
# BIC semble échouer ici pour le meilleur des modèles par classe d'un point de vue classification


# == Modèles de mélanges gaussien grande dimension ========
library(HDclassif)
res=hdda(data_continuous_train,data_categ_train$cartevp)
res2=predict(res,data_continuous_test,data_categ_test$cartevp)


# ============== regression logistique =================
# --- regression logistique sur variables quanti ----
# regression logistique sur uniquement les variables quanti
# (pour utiliser les quali il faudrait transformer les quali en dummy variable
# mais on explose alors le nombre de variables )
df_continuous_train=data.frame(data_continuous_train,data_categ_train$cartevp)
mod2=glm(data_categ_train.cartevp~.,data=df_continuous_train,family=binomial(link = "logit"))
summary(mod2)
# avec selection backward
mod3=step(mod2,trace=2)
summary(mod3)
# prediction de l'echantillon test
p=predict(object = mod3,newdata = data_continuous_test)
taux_bon_class_RL_VQ=sum((p>0.5)==(data_categ_test$cartevp=="Coui"))/nrow(data_categ_test)
cat('Taux de bon classement par Reg Log sur les variables quanti :',taux_bon_class_RL_VQ,'\n')

# --- regression logistique sur variables quantitatives + categorielles ----
df_train=data.frame(data_continuous_train,data_categ_train)
df_test=data.frame(data_continuous_test,data_categ_test)
mod2_all=glm(cartevp~.,data=df_train,family=binomial(link = "logit"))
summary(mod2_all)
# avec selection backward
mod3_all=step(mod2_all,trace=0)
summary(mod3_all)
# prediction de l'echantillon test
p_all=predict(object = mod3_all,newdata = df_test)

taux_bon_class_RL_Vall=sum((p_all>0.5)==(data_categ_test$cartevp=="Coui"))/nrow(data_categ_test)
cat('Taux de bon classement par Reg Log sur les variables quantitatives et categorielles :',taux_bon_class_RL_Vall,'\n')

# --- knn ---
library(class)
# nous allons choisir le nombre N de plus proches voisins par Validation Croisée
tbc=NULL # tableau contenant les taux de bon classement
for (nbppv in 1:20){
tmp=knn.cv(data_continuous_train,data_categ_train$cartevp,k=nbppv)
tbc=c(tbc,mean(tmp==data_categ_train$cartevp))
}
plot(tbc,type='l')
# nous pouvons maintenant appliquer KNN avec le nombre K maximisant le taux de bon classement
mod7=knn(data_continuous_train,data_continuous_test,data_categ_train$cartevp,k=which.max(tbc))

taux_bon_class_KNN=mean(mod7==data_categ_test$cartevp)
cat('Taux de bon classement par ',which.max(tbc),'-NN sur les variables quanti :',taux_bon_class_KNN,'\n')
table(mod7,data_categ_test$cartevp)

# --- Arbre de regression ---
library('rpart')
mod8=rpart(data_categ_train.cartevp~.,data=df_continuous_train)
p=predict(object = mod8,newdata = data_continuous_test)

plot(mod8, compress = TRUE)
text(mod8, use.n = TRUE)

taux_bon_class_Tree=mean((p[,2]>0.5)==(data_categ_test$cartevp=="Coui"))
cat('Taux de bon classement par Arbre de classification sur les variables quanti :',taux_bon_class_Tree,'\n')
table(p[,2]>0.5,data_categ_test$cartevp=="Coui")

# affichage de tous les résultats
cat('Taux de bon classement par Arbre de classification sur les variables quanti :',taux_bon_class_Tree,'\n')
cat('Taux de bon classement par Reg Log sur les variables quanti :',taux_bon_class_RL_VQ,'\n')
cat('Taux de bon classement par Reg Log sur les variables quantitatives et categorielles :',taux_bon_class_RL_Vall,'\n')
cat('Taux de bon classement par LDA sur les variables quanti :',taux_bon_class_LDA,'\n')
cat('Taux de bon classement par ',which.max(tbc),'-NN sur les variables quanti :',taux_bon_class_KNN,'\n')
cat('Taux de bon classement par MclustDA sur les variables quanti :',taux_bon_class_MclustDA,'\n')
