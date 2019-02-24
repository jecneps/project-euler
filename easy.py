import math

def mult3or5():
    s = 0
    for n in range(1000):
        if n % 3 == 0 or n % 5 == 0:
           s = s + n
    print("mult3or5")
    print(s)
    return s


def evenFib():
    d = dict()
    d[1] = 1
    d[2] = 2
    s = 2
    n = 3
    while(True):
        fib = d[n-1] + d[n-2]
        d[n] = fib
        if fib > 4000000:
            print("evenFib")
            print(s)
            return
        elif fib % 2 == 0:
            s = s + fib
        n = n + 1

def isPrime(n):
    if n == 2:
        return True
    elif n == 1 or n % 2 == 0:
        return False
    for i in range(3, int(math.sqrt(n)) + 1):
        if n % i == 0:
            return False
    return True
        
def largestPrime():
    for n in range(int(math.sqrt(600851475143)), 3, -1):
        if 600851475143 % n == 0 and isPrime(n):
            print("largestPrime")
            print(n)
            return
        





mult3or5()
evenFib()
largestPrime()
