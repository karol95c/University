import itertools, random
import pygame

SQUARE_SIZE = 50
H = [2, 1, 3, 1, 1]
V = [1, 3, 1, 2, 1]



def print_table(table, size):
    pygame.init()      # Prepare the pygame module for use
    background_size = size * SQUARE_SIZE   # Desired physical surface size, in pixels.
    main_surface = pygame.display.set_mode((background_size, background_size))
    main_surface.fill((255, 200, 255))
    shadow_color = (128,128,128)
    for i in range(size):
        for j in range(size):
            if table[j][i] == 1:
                small_rect = (i * SQUARE_SIZE, j * SQUARE_SIZE, SQUARE_SIZE, SQUARE_SIZE)
                main_surface.fill(shadow_color, small_rect)
    
    line_color = (0, 0, 0)
    for i in range(size + 1):
        line_pos = i * SQUARE_SIZE;
        if (i == size):
            line_pos -=1
        horizontal_line = (0, line_pos, background_size, 1)
        main_surface.fill(line_color, horizontal_line)
        vertical_line = (line_pos, 0, 1, background_size)
        main_surface.fill(line_color, vertical_line)
    pygame.display.flip()

# print (list(product(product([0, 1], repeat=4)), repeat = 4))
def create_table(h, v):
    random.seed()
    size = len(h)

    v_perm = []
    h_perm = []
    res = []
    for i in range(size):
        res.append([])
    for i in range(size):
        v_values = [1] * v[i]
        v_values += [0] * (size - v[i])
        v_perm = itertools.permutations(v_values)
        [res[i].append(x) for x in v_perm if x not in res[i]]  
    table = []
    h_product = itertools.product(*res)
    for x in h_product:
        sum = 0
        correct = True
        for i in range(size):
            sum = 0
            for j in range(size):
                sum+= x[j][i]
            if (sum!=h[i]):
                correct = False
                break
        if correct:
            table.append(x)
    
    print (len(table))
    rand = random.randrange(len(table))
    print_table(table[rand], size)
            
        
            

def main():
    table = create_table(H, V)
    input("Press Enter to exit...")

main()