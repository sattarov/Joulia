library(copula)
rho_list <- seq(0,1,length=100)
write("rho's are in interval from [0,1]", file = "copula_output.txt", append = FALSE)
for (rho in rho_list)
	{
		write(paste("rho = ", rho ,sep = " "), file = "copula_output.txt" , append = TRUE,  ncolumns = 2)
		for (i in 1:100) 
			{
				write(paste("--------------------- ", i, " iteration ---------------------"), file = "copula_output.txt" , append = TRUE,  ncolumns = 2)
				norm.cop <- normalCopula(dim = 2, dispstr = "un", param = rho)
				copula <- rCopula(100, norm.cop)
				write(copula, file = "copula_output.txt" , append = TRUE,  ncolumns = 2)
			}
	}
	
norm.cop <- normalCopula(0.4,3)
norm.cop
dCopula(c(0.5, 0.5), norm.cop)
pCopula(c(0.2), norm.cop)
u <- rCopula(100, norm.cop)
u
plot(u)
dCopula(u, norm.cop)
pCopula(u, norm.cop)
persp(norm.cop, dCopula)
contour(norm.cop, pCopula)

u <- rCopula(100, normalCopula(0.5, dim = 2))
if(require(scatterplot3d))
  scatterplot3d(u)
