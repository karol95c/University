import matplotlib.pyplot as plt
import numpy as np
import matplotlib.animation as animation 

ON = 255
OFF = 0
vals = [ON, OFF] 

SIZE = 100
UPDATE_TIME = 500

def randomGrid(): 
	return np.random.choice(vals, SIZE*SIZE, p=[0.1, 0.9]).reshape(SIZE, SIZE) 

def neighbours_count(grid, i, j):
    return int((grid[i, (j-1)%SIZE] + grid[i, (j+1)%SIZE] +
		grid[(i-1)%SIZE, j] + grid[(i+1)%SIZE, j] +
		grid[(i-1)%SIZE, (j-1)%SIZE] + grid[(i-1)%SIZE, (j+1)%SIZE] +
		grid[(i+1)%SIZE, (j-1)%SIZE] + grid[(i+1)%SIZE, (j+1)%SIZE])/ON)

def update(frameNum, img, grid): 
	newGrid = grid.copy() 
	for i in range(SIZE):
		for j in range(SIZE):
			total = neighbours_count(grid, i, j)
			if grid[i, j] == ON: 
				if (total < 2) or (total > 3): 
					newGrid[i, j] = OFF 
			else: 
				if total == 3: 
					newGrid[i, j] = ON

	# for i in range(SIZE):
	# 	for j in range(SIZE):
	# 		if (grid[i,j] = ON)

	grid[:] = newGrid[:]

	img.set_data(grid)
	return img

grid = randomGrid()
fig, ax = plt.subplots()
img = ax.imshow(grid) 
ani = animation.FuncAnimation(fig, update, fargs=(img, grid, ), 
								frames = 10, 
								interval=UPDATE_TIME) 
plt.show()