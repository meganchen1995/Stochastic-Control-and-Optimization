Question 1
==========

From matrix property we know (*A**B*)<sup>*T*</sup> = *B*<sup>*T*</sup>*A*<sup>*T*</sup>, then
\begin{align}
LHS\times A^{T} &= (A^{-1})^{T}A^{T}\\
&= (AA^{-1})^{T}\\
&= I^{T}\\
&= I\\
\end{align}
Also,
\begin{align}
RHS\times A^{T} &= I\\
\end{align}
Hence, (*A*<sup>−1</sup>)<sup>*T*</sup> = (*A*<sup>*T*</sup>)<sup>−1</sup>.

Question 2
==========

Let dollar amount of first mortgage, second mortgage, home improvement and personal overdraf be *x*<sub>1</sub>,*x*<sub>2</sub>,*x*<sub>3</sub>,*x*<sub>4</sub>. Then
\begin{equation}
  \begin{cases}
  x_{1} + x_{2} + x_{3} + x_{4} &= 250\\
  0.45x_{1} - 0.55x_{2}&=0\\
  x_{2} &= 250 \times 0.25\\
  0.14x_{1} + 0.2x_{2} + 0.2x_{3} + 0.1x_{4} &= 250 \times 0.15\\
  \end{cases}
\end{equation}
Hence, we have a system of equations which can be solved using matrix form *A***x** = **b**, where

\begin{equation}
A = 
  \begin{bmatrix}
  1 & 1 & 1 & 1 \\
  1 & 1 & 0 & 0 \\
  0 & 1 & 0 & 0 \\
  0.14 & 0.2 & 0.2 & 0.1 \\
  \end{bmatrix},
\quad
\textbf{x} = 
  \begin{bmatrix}
  x_{1}\\
  x_{2}\\
  x_{3}\\
  x_{4}\\
  \end{bmatrix},
\quad
\textbf{b} = 
  \begin{bmatrix}
  250\\
  0\\
  62.5\\
  37.5\\
  \end{bmatrix}
\end{equation}
we could use R to solve for **x**,

``` r
b <- c(250,0,62.5,37.5)
A <- matrix(c(1,0.45,0,0.14,1,-0.55,1,0.2,1,0,0,0.2,1,0,0,0.1),4,4) 
x <- solve(A)%*%b 
print(x)
```

    ##          [,1]
    ## [1,] 76.38889
    ## [2,] 62.50000
    ## [3,] 31.94444
    ## [4,] 79.16667

Hence,
$$\\textbf{x} = \\begin{bmatrix}
  76.39\\\\
  62.50\\\\
  31.94\\\\
  79.17\\\\
\\end{bmatrix}$$

Question 3
==========

Let number of units manufactured for each type of variant be *x*<sub>1</sub>,*x*<sub>2</sub>,*x*<sub>3</sub>,*x*<sub>4</sub>. Then the objective is to maximize

1.5*x*<sub>1</sub> + 2.5*x*<sub>2</sub> + 3*x*<sub>3</sub> + 4.5*x*<sub>4</sub>

subject to the constraints
\begin{equation}
  \begin{cases}
  2x_{1} + 4x_{2} + 3x_{3} + 7x_{4} &\leq 100000\\
  3x_{1} + 2x_{2} + 3x_{3} + 4x_{4} &\leq 50000\\
  2x_{1} + 3x_{2} + 2x_{3} + 5x_{4} &\leq 60000\\
  x_{1},x_{2},x_{3},x_{4} &\geq 0\\
  \end{cases}
\end{equation}
We could rewrite the objective function and contraints in matrix forms **c**<sup>*T*</sup>**x** and *A***x** ≤ **b** respectively, where

\begin{equation}
\textbf{c} = 
  \begin{bmatrix}
  1.5\\
  2.5\\
  3\\
  4.5\\
  \end{bmatrix},
\quad
A = 
  \begin{bmatrix}
  1 & 1 & 1 & 1 \\
  1 & 1 & 0 & 0 \\
  0 & 1 & 0 & 0 \\
  0.14 & 0.2 & 0.2 & 0.1 \\
  \end{bmatrix},
\quad
\textbf{x} = 
  \begin{bmatrix}
  x_{1}\\
  x_{2}\\
  x_{3}\\
  x_{4}\\
  \end{bmatrix},
\quad
\textbf{b} = 
  \begin{bmatrix}
  250\\
  0\\
  62.5\\
  37.5\\
  \end{bmatrix}
\end{equation}
we could use R to solve for **x**,

``` r
library(lpSolve)
c <- c(1.5,2.5,3,4.5)
A <- matrix(c(2,3,2,4,2,3,3,3,2,7,4,5),3,4)
dir <- c('<=','<=','<=')
b <- c(100000,50000,60000)
s <- lp('max',c,A,dir,b)
print(s$solution)
```

    ## [1]     0 16000  6000     0

Hence,
$$\\textbf{x} = \\begin{bmatrix}
  0\\\\
  16000\\\\
  6000\\\\
  0\\\\
\\end{bmatrix}$$

Question 4
==========

Given *b*<sub>*k*</sub> is the difference in points in the match *k*, *r*<sub>*i*</sub> and *r*<sub>*j*</sub> are the ratings for teams *i*,*j* and *b*<sub>*k*</sub> = *r*<sub>*i*</sub> − *r*<sub>*j*</sub>, we have
\begin{equation}
  \begin{cases}
  r_{1} - r_{2} = -45\\
  r_{1} - r_{3} = -3\\
  r_{1} - r_{4} = -31\\
  r_{1} - r_{5} = -45\\
  r_{2} - r_{3} = 18\\
  r_{2} - r_{4} = 8\\
  r_{2} - r_{5} = 20\\
  r_{3} - r_{4} = 2\\
  r_{3} - r_{5} = -27\\
  r_{4} - r_{5} = -38\\
  \end{cases}
\end{equation}
Then, rewrite it into a matrix form *A***r** = **b** we have,
\begin{equation}
A = 
  \begin{bmatrix}
  1 & -1 & 0 & 0 & 0\\
  1 & 0 & -1 & 0 & 0\\
  1 & 0 & 0 & -1 & 0\\
  1 & 0 & 0 & 0 & -1\\
  0 & 1 & -1 & 0 & 0\\
  0 & 1 & 0 & -1 & 0\\
  0 & 1 & 0 & 0 & -1\\
  0 & 0 & 1 & -1 & 0\\
  0 & 0 & 1 & 0 & -1\\
  0 & 0 & 0 & 1 & -1\\
  \end{bmatrix},
\quad
\textbf{r} = 
  \begin{bmatrix}
  r_{1}\\
  r_{2}\\
  r_{3}\\
  r_{4}\\
  r_{5}\\
  \end{bmatrix},
\quad
\textbf{b} = 
  \begin{bmatrix}
  -45\\
  -3\\
  -31\\
  -45\\
  18\\
  8\\
  20\\
  2\\
  -27\\
  38\\
  \end{bmatrix}
\end{equation}
Then we have
\begin{equation}
A^{T}A = 
  \begin{bmatrix}
  4 & -1 & -1 & -1 & -1\\
  -1 & 4 & -1 & -1 & -1\\
  -1 & -1 & 4 & -1 & -1\\
  -1 & -1 & -1 & 4 & -1\\
  -1 & -1 & -1 & -1 & 4\\
  \end{bmatrix},
\quad
A^{T}\textbf{b} = 
  \begin{bmatrix}
  -124\\
  91\\
  -40\\
  -17\\
  90\\
  \end{bmatrix}
\end{equation}
After adding the constraint that all ratings sum up to 0, we are solving for $\\hat{A}\\textbf{r} = \\hat{\\textbf{b}}$ where
\begin{equation}
\hat{A} = 
  \begin{bmatrix}
  4 & -1 & -1 & -1 & -1\\
  -1 & 4 & -1 & -1 & -1\\
  -1 & -1 & 4 & -1 & -1\\
  -1 & -1 & -1 & 4 & -1\\
  -1 & -1 & -1 & -1 & 4\\
  1 & 1 & 1 & 1 & 1\\
  \end{bmatrix},
\quad
\hat{\textbf{b}} = 
  \begin{bmatrix}
  -124\\
  91\\
  -40\\
  -17\\
  90\\
  0\\
  \end{bmatrix}
\end{equation}
we could use R to solve for **r**,

``` r
library(limSolve)
A <- matrix(c(1,1,1,1,0,0,0,0,0,0,
              -1,0,0,0,1,1,1,0,0,0,
              0,-1,0,0,-1,0,0,1,1,0,
              0,0,-1,0,0,-1,0,-1,0,1,
              0,0,0,-1,0,0,-1,0,-1,-1),
            10,5)
b <- c(-45,-3,-31,-45,18,8,20,2,-27,-38)
A_t <- t(A)
A_hat <- rbind(A_t%*%A,rep(1,5))
b_hat <- rbind(A_t%*%b,0)
r <- Solve(A_hat,b_hat)
print(r)
```

    ## [1] -24.8  18.2  -8.0  -3.4  18.0

Hence,
$$\\textbf{r} = \\begin{bmatrix}
  -24.8\\\\
  18.2\\\\
  -8\\\\
  -3.4\\\\
  18\\\\
\\end{bmatrix}$$
