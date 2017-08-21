def f(x):
    return 2*x*x + 3*x + 1

def trapez(a, b, n):
    h = (b-a)/n
    sum = 0.0
    x = a
    i = 0
    while i < (n-1):
        x += h
        sum += f(x)
        i += 1
    sum += 0.5*(f(a) + f(b))
    return sum*h
