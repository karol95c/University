import math
list = [];
for x in range (0, 256, 2):
    list.append(int(127.5 * (1 + math.cos(math.pi*x/255))))
print(list)
