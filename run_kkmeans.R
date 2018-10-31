source("kkmeans_functions.R")
load("initial.Rda")
ls()

stocks <- as.matrix(master.kkm)


## Gaussian

print("Computing Gaussian Run...")

print("Computing kernel matrix...")

rbf.kernM <- kernelMatrix(stocks,
			kernel=rbfdot(sigma=.000000002))

print("Running kkmeans...")
rbf.run <- kkmeans(stocks,
			kernel=rbfdot(sigma=.000000002),
			centers=10)

print("Computing cost...")
rbf.cost <- cost_in_H(nclusters=10,
			clusters=rbf.run@.Data,
			kernelMatrix=rbf.kernM)

print("Computing seperation matrix...")
rbf.sepM <- smat(nclusters=10,
			clusters=rbf.run@.Data,
			kernelMatrix=rbf.kernM)

print("RBF Results")
rbf.run
print("Cost: ")
rbf.cost
print("Seperation Matrix: ")
rbf.sepM

save(rbf.kernM, rbf.run, rbf.cost, rbf.sepM, file="run15.Rda")

## Run 1 poly 2

# print("Computing kernel matrix...")
# r1.poly2.kernM <- kernelMatrix(stocks,
#                         kernel=polydot(degree=2))
#
# print("Running kkmeans...")
# r1.poly2 <- kkmeans(stocks,
#                         kernel=polydot(degree=2),
#                         centers=10)
#
# print("Computing cost...")
# r1.poly2.cost <- cost_in_H(nclusters=10,
#                         clusters=r1.poly2@.Data,
#                         kernelMatrix=r1.poly2.kernM)
#
# print("Computing seperation matrix...")
# r1.poly2.sepM <- smat(nclusters=10,
#                         clusters=r1.poly2@.Data,
#                         kernelMatrix=r1.poly2.kernM)
#
# print("Run 1 Poly 2 Results")
# r1.poly2
# print("Cost: ")
# r1.poly2.cost
# print("Seperation Matrix: ")
# r1.poly2.sepM


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

