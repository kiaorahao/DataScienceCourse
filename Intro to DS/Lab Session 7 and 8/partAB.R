# assignment A
m1 = c(16.03,16.04,16.05,16.05,16.02,16.01,15.96,15.98,16.02,15.99)
m2 = c(16.02,15.97,15.96,16.01,15.99,16.03,16.04,16.02,16.01,16.00)
y1 = mean(m1)
y2 = mean(m2)
S1 = sd(m1)
S2 = sd(m2)
n1 = 10
n2 = 10
sp= sqrt(((n1 - 1)*S1^2 + (n2-1)*S2^2)/(n1 + n2 - 2))
t0 = (y1 - y2)/(sp*sqrt(1/n1 + 1/n2))
print(t0)

t1 = t.test(m1,m2)
print(t1)

a = 0.05
# +- t 0.025,18 = +1 2.101
t = 2.101
if (t0 <= t) {
  print("Accept")
} else {
  print("Reject")
  }

# assignment B
# H0;
u = 100
# H1;u != 100
x = 110
Q = 10
n = 30
z = (x-u)/(Q/sqrt(n))
print(z)
a = 0.01
# 1-0.5a = 0.995
# In table, only find nearest value 0.9951
z0 = 2.5+0.08 
if (z <= z0*(-1) | z > z0 ) {
  print("Reject")
} else {
  print("Accept")
}

L = 110 - z0 * Q/sqrt(n)
U = 110 + z0 * Q/sqrt(n)
print(L)
print(U)
