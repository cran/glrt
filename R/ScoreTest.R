ScoreTest <-
function(A, k=2, EMstep=TRUE, ICMstep=TRUE, tol = 1e-6, maxiter=1000, inf=Inf)
{
if(ncol(A) == 3 && all(A[,2] >= A[,1]) && length(unique(A[,3])) == k && all(A[,3]>=0) && all(A[,3]< k) )
{
AA = A[,-3] 
trt = A[,3]
est = ModifiedEMICM(AA, EMstep=EMstep, ICMstep=ICMstep, tol=tol, maxiter=maxiter)
U = ScoreStat(trt, k, est) 
V = VarScoreStat(trt, k, est)
chisq = t(U[-k]) %*% solve(V[1:(k-1), 1:(k-1)]) %*% U[-k]
p = 1-pchisq(chisq, k-1)
}
else
{
stop("Please Verify data format, # of samples, and treatment indicator!")
}
out = data.frame()
class(out) = "Score Test"
out$method = "Score Test under the proportional hazards model (Finkelstein, 1986)"
out$u = U
out$var = V
out$chisq = chisq
out$df = k - 1
out$p = p
out
}

