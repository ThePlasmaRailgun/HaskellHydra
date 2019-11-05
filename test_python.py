def S(T):
  return 0 if T==1 else [S(T[0])]+T[1:]

def Helper(T, c, B=None):
    pass


def R(T, c, B=None):
  U=T[0]
  V=T[1:]
  #global B

  print(T)

  try:
    print("B:", B)
  except:
    pass

  if T[-1]==0:
    B=T

  try:
    print("B':", B, "\n")
  except:
    pass

  if U==1: 
    return [S(B)]+V, B
  return ([R(U, c, B)]*c+V if U else V), B

#A=[[[1,1],1],0]
A = [[1, 0], 1]

#c=9
"""while A:
  A=R(A)
  #c*=c
  c += 1
  print("+1")"""    

def DoR(T, B=None, c=0):
    if len(T) == 0:
        return c
    else:
        print("+1")
        return DoR(*R(T, c, B), c + 1)
    
print(DoR(A))
