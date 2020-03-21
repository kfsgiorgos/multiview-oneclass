#def solution(N):
    # write your code in Python 3.6
    
#   binaryformat = "{0:b}".format(N)

#factorial = 1
#for i in range (1, 10, 2):
#    factorial1 = factorial + i
#    print factorial1



length = len(a)
print(length)    


list1 = []
for i in range(0, length-1):
    jj = a[i] + a[i+1]
    list1.append(jj)
    if i == length:
        pass

print(list1)


duplicates = []
duplicates1 = []
ii = -1
for item in list1:
    ii = ii + 1
    if a.count(item) > 1:
        duplicates.append(item)
        duplicates1.append(ii)
print(duplicates)
print(duplicates1)


res1 = [((duplicates1[i+1] -  duplicates1[i] - 1) * 2) + 1 for i in range(0, len(duplicates1), 2)]
print(max(res1))










if len(duplicates1) == 0:
    return1 = -1







#rr = [i not in set(duplicates) for i in a]
#res = [i for i, val in enumerate(rr) if val] 
#print(a[res[0]])

#print("kadfsfd")
#for item1 in a:
#    if item1 not in set(duplicates):
#        odd_elemnt.append(item1)


#print(odd_elemnt[0])

