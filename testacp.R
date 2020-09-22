#Test para ACP


kmo <- function(x)
{
x <- subset(x, complete.cases(x)) # Omit missing values
r <- cor(x) # Correlation matrix
r2 <- r^2 # Squared correlation coefficients
i <- solve(r) # Inverse matrix of correlation matrix
d <- diag(i) # Diagonal elements of inverse matrix
p2 <- (-i/sqrt(outer(d, d)))^2 # Squared partial correlation coefficients
diag(r2) <- diag(p2) <- 0 # Delete diagonal elements
KMO <- sum(r2)/(sum(r2)+sum(p2))
MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
return(list(KMO=KMO, MSA=MSA))
}


Bartlett.sphericity.test <- function(x)
{
method <- "Bartletts test of sphericity"
data.name <- deparse(substitute(x))
x <- subset(x, complete.cases(x)) # Omit missing values
n <- nrow(x)
p <- ncol(x)
chisq <- (1-n+(2*p+5)/6)*log(det(cor(x)))
df <- p*(p-1)/2
p.value <- pchisq(chisq, df, lower.tail=FALSE)
names(chisq) <- "X-squared"
names(df) <- "df"
return(structure(list(statistic=chisq, parameter=df, p.value=p.value,
method=method, data.name=data.name), class="htest"))
}