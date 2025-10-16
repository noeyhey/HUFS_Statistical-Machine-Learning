# 03. Numerical Analysis Basics

set.seed(0)

# 전치행렬 
print(A <- round(matrix(rnorm(12), 4, 3), 2))
t(A)

# 내적&외적
x <- 1:3
y <- 4:6

## 내적
sum(x*y)
t(x) %*% y
drop(t(x) %*% y)

## 외적
outer(x, y)
x %o% y
x %*% t(y)

### 외적의 활용
outer(x, y, FUN = "*")
outer(x, y, FUN = "+")

# f(x,y)=sin√x2+y2/√x2+y2

x <- seq(-10, 10, length = 30)
y <- x
f <- function(x, y) 
{ 
  r <- sqrt(x^2 + y^2)
  return(sin(r)/r) 
}
z <- outer(x, y, f)
z[is.na(z)] <- 1
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "Sinc( r )", 
      cex.lab = 0.8, cex.axis = 0.7)

## 반복 실행

N <- 10
d <- 5
X <- round(matrix(rnorm(N*d), N, d), 2)
X
## Use image() to visualize a matrix. 
## Need to transpose the matrix and then flip it horizontally.
image(t(X)[,N:1], axes = FALSE, col = gray.colors(128))

apply(X = X, MARGIN = 1, FUN = mean) # 행 방향으로 평균 계산 반복 실행
apply(X, 2, mean) # 열 빙향으로 평균 계산 반복 실행

# for ( n in 1:N ) mean(X[n,])  # Not run...
# apply(X, 1, mean) # Not run...
head(rowMeans(X))

head(colMeans(X))
head(rowSums(X))
head(colSums(X))


Z <- scale(X) # scale(X, center = TRUE, scale = TRUE)
head(Z)

print(R <- cov(Z))
cor(X)  # should be equal to R


# 행렬곱
A

t(A) %*% A

### 하다마드 곱
A * A # A⊙B

## 행렬곱과 선형변환 
X <- matrix(runif(400 * 2), 400, 2)
P <- matrix(c(10, 4, 4, 10), 2, 2)

par(mfrow = c(1, 2))
plot(X, xlab = "", ylab = "", pch = 19, col = "gray")
plot(X %*% P, col = "skyblue", pch = 19)


# 역행렬
A <- t(X) %*% X
A.inv <- solve(A)
A.inv

A %*% A.inv
A.inv %*% A
round(A %*% A.inv, 12)
round(A.inv %*% A, 12)


Amat <- matrix(c(1,  2,  3,
                 2, -5, -1,
                 0,  1, -1),
               ncol = 3, byrow = TRUE)
bvec <- c(5, 7, 0)
x <- solve(Amat, bvec)
x

Amat %*% x   # should be equal to bvec 


# 행렬식

det(A)

plot(X, xlab = "", ylab = "", pch = 19, col = "gray")

print(P <- matrix(c(1, 0.4, 0.4, 1), 2, 2))
print(Q <- matrix(c(1, 0.8, 0.8, 1), 2, 2))
print(R <- matrix(c(1, 0.98, 0.98, 1), 2, 2))
print(S <- matrix(c(0.4, 1, 1, 0.4), 2, 2))

det(P)
det(Q)
det(R)
det(S)

par(mfrow = c(2, 2))
plot(X %*% P, col = "skyblue", pch = 19)
plot(X %*% Q, col = "coral", pch = 19)
plot(X %*% R, col = "darkred", pch = 19)
plot(X %*% S, col = "darkgreen", pch = 19)


# 행렬의 분해
## 상삼각행렬, 하삼각행렬
print(A <- round(A, 2))
upper.tri(A)
lower.tri(A)
A[lower.tri(A)] <- 0
A