from random import randrange
from time import process_time

    
def genere_liste(N,M):
    return [randrange(0,M) for _ in range(N)]

def indmin(L):
    n=len(L)
    m=0
    for i in range(n) :
        if L[i] < L[m] :
            m=i
    return m

def trirec(L) :
    if len(L)<=1 :
        return L[:]
    else :
        m=indmin(L)
        return [L[m]]+trirec(L[:m]+L[m+1:])

def passage(L,p):
    pase=False
    for i in range(0,p-1):
        if L[i]>L[i+1]:
            pase=True
            L[i],L[i+1]=L[i+1],L[i]
    return pase

def tribul(L):
    i=len(L)
    while passage(L,i) :
        i=i-1
    return L

def fusion(L1,L2):
    L=[]
    n1, n2 = len(L1), len(L2)
    i1, i2 = 0, 0
    for i in range(n1+n2):
        if i2==n2 or i1<n1 and L1[i1]<=L2[i2]:
            L.append(L1[i1])
            i1+=1
        else:
            L.append(L2[i2])
            i2+=1
    return L

def tri_fusion(L):
    n=len(L)
    if n<=1 :
        return L[:]
    else :
        m=n//2
        return fusion(tri_fusion(L[:m]),tri_fusion(L[m:]))

print(tri_fusion([3, 1, 2, 0]))
