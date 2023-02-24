library(pcalg)

data("gmG", package = "pcalg")

varNames <- gmG8$g@nodes
suffStat <- list(C = cor(gmG8$x), n = nrow(gmG8$x)) 

skel.gmG8 <- skeleton(suffStat, indepTest = gaussCItest, labels = varNames, alpha = 0.01)
pc.gmG8 <- pc(suffStat, indepTest = gaussCItest, labels = varNames, alpha = 0.01)

## Used to generate "gmG"
set.seed(40)
p <- 8
n <- 5000
## true DAG:
vars <- c("Author", "Bar", "Ctrl", "Goal", paste0("V",5:8))
gGtrue <- randomDAG(p, prob = 0.3, V = vars)
gmG  <- list(x = rmvDAG(n, gGtrue, back.compatible=TRUE), g = gGtrue)
gmG8 <- list(x = rmvDAG(n, gGtrue),                       g = gGtrue)
plot(gGtrue)
