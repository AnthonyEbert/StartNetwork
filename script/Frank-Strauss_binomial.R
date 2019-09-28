
# Triangles

A = matrix(c(c(-1,0,0,0), c(-1,0,2,0), c(-1,0,2,1),c(-1,1,0,3)), nrow = 4, byrow = TRUE)

b = dbinom(0:3,size = 3, prob = 0.6, log = TRUE)

x = solve(A,b)

1/exp(x[1])

exp(x[3]*2)/exp(x[1])

exp(x[3]*2 + x[4]*1)/exp(x[1])

exp(x[2] * 1 + x[3]*0 + x[4]*3)/exp(x[1])

# squares

Asquare = matrix(c(c(-1,0,0,0,0), c(-1,0,2,0,0), c(-1,0,3,0,1), c(-1,1,1,2,1), c(-1,2,0,2,2)), ncol = 5, byrow = TRUE)

b = log(dbinom(c(0,1,3,4,5), size = 6, prob = 0.6) * c(1,1,4/20,12/15,1))

xsquare = solve(Asquare, b)
