gLRT <-
function(A, k=2, method=c("glrt1", "glrt2", "glrt3", "score"), M=50, rho=0, gamma=0, EMstep=TRUE, ICMstep=TRUE, tol=1e-07, maxiter=1000, inf=Inf)
{
if(method == "glrt1")
gLRT1(A, k=k, M = M, EMstep=EMstep, ICMstep=ICMstep, tol=tol, maxiter=maxiter, inf=inf)
else if(method == "glrt2")
gLRT2(A, k=k, rho=rho, gamma=gamma, EMstep=EMstep, ICMstep=ICMstep, tol=tol, maxiter=maxiter, inf=inf)
else if(method == "glrt3")
gLRT3(A, k=k, rho=rho, gamma=gamma, EMstep=EMstep, ICMstep=ICMstep, tol=tol, maxiter=maxiter, inf=inf)
else if(method == "score")
ScoreTest(A, k=k, EMstep=EMstep, ICMstep=ICMstep, tol = tol, maxiter=maxiter, inf=inf)
else
stop("Invalid method was specified!")
}

