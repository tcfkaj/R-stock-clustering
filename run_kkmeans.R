source("kkmeans_functions.R")
load("initial.Rda")
ls()

stocks <- as.matrix(master.kkm)

## Elbow


# costs <- c()
#
#
# kn.kern <- kernelMatrix(stocks,
#                         kernel=rbfdot(sigma=0.00003))
#
# for (i in 3:12){
#         print("Computing run with clusters: ")
#         print(i);
#         kn <- kkmeans(stocks,
#                         kernel=rbfdot(sigma=0.00003),
#                         centers=i);
#         print("Computing cost for: ")
#         print(i);
#         kn.cost <- cost_in_H(nclusters=i,
#                         clusters=kn@.Data,
#                         kernelMatrix=kn.kern);
#         costs <- c(costs, kn.cost)
# }
#
# save(costs, file="elbow3to12.Rda")

## Gaussian

k=10

print("Computing Gaussian Run...")

print("Computing kernel matrix...")

rbf.kernM <- kernelMatrix(stocks,
		kernel=rbfdot(sigma=1))

print("Running kkmeans...")
kclust <- kkmeans(stocks,
		kernel=rbfdot(sigma=1),
		centers=k)

print("Computing cost...")
kclust.cost <- cost_in_H(nclusters=k,
		clusters=kclust@.Data,
		kernelMatrix=rbf.kernM)

print("Computing seperation matrix...")
kclust.sepM <- smat(nclusters=k,
		clusters=kclust@.Data,
		kernelMatrix=rbf.kernM)

print("RBF Results")
kclust
print("Cost: ")
kclust.cost
print("Seperation Matrix: ")
kclust.sepM

save(rbf.kernM, kclust, kclust.cost, kclust.sepM, file="gamma1.Rda")




## Run 1 poly 2

# print("Computing kernel matrix...")
# poly2.kernM <- kernelMatrix(stocks,
#                 kernel=polydot(degree=2))
#
# print("Running kkmeans...")
# poly2 <- kkmeans(stocks,
#                 kernel=polydot(degree=2),
#                 centers=10)
#
# print("Computing cost...")
# poly2.cost <- cost_in_H(nclusters=10,
#                 clusters=poly2@.Data,
#                 kernelMatrix=poly2.kernM)
#
# print("Computing seperation matrix...")
# poly2.sepM <- smat(nclusters=10,
#                 clusters=poly2@.Data,
#                 kernelMatrix=poly2.kernM)
#
# print("Run 1 Poly 2 Results")
# poly2
# print("Cost: ")
# poly2.cost
# print("Seperation Matrix: ")
# poly2.sepM
# save(poly2.kernM, poly2, poly2.cost, poly2.sepM, file="poly2.Rda")

## Run 2 poly 3

# print("Computing Run 2...")
#
# r2.poly3.kernM <- kernelMatrix(stocks,
#                         kernel=polydot(degree=3))
# r2.poly3 <- kkmeans(stocks,
#                         kernel=polydot(degree=3),
#                         centers=10)
# r2.poly3.cost <- cost_in_H(nclusters=10,
#                         clusters=r2.poly3@.Data,
#                         kernelMatrix=r2.poly3.kernM)
#
# r2.poly3.sepM <- smat(nclusters=10,
#                         clusters=r2.poly3@.Data,
#                         kernelMatrix=r2.poly3.kernM)
#
# print("Run 2 Poly 3 Results")
# r2.poly3
# print("Cost: ")
# r2.poly3.cost
# print("Seperation Matrix: ")
# r2.poly3.sepM
#
##     Run 3 poly 4
#
# print("Computing Run 3...")
#
# r3.poly4.kernM <- kernelMatrix(stocks,
#                         kernel=polydot(degree=4))
# r3.poly4 <- kkmeans(stocks,
#                         kernel=polydot(degree=4),
#                         centers=10)
# r3.poly4.cost <- cost_in_H(nclusters=10,
#                         clusters=r3.poly4@.Data,
#                         kernelMatrix=r3.poly4.kernM)
#
# r3.poly4.sepM <- smat(nclusters=10,
#                         clusters=r3.poly4@.Data,
#                         kernelMatrix=r3.poly4.kernM)
#
# print("Run 3 Poly 4 Results")
# r3.poly4
# print("Cost: ")
# r3.poly4.cost
# print("Seperation Matrix: ")
# r3.poly4.sepM

