# Script to generate LaTeX code for figures in the paper

import matplotlib.pyplot as plt
import numpy as np
import os

# Create the directory if it doesn't exist
output_dir = 'Paper Plots/latex_text_files'
os.makedirs(output_dir, exist_ok=True)
os.chdir(output_dir)

# We will produce n journeys that criss-cross the city
# Then convert these into LaTeX code for the figure

# Choose n random start and end points on the boundaries of the square from (0,0) and (5,5)
np.random.seed(1)

def generate_border_points(n, lower, upper):
    points = []
    for _ in range(n):
        if np.random.rand() > 0.5:
            x = np.random.choice([lower, upper])
            y = np.random.uniform(lower, upper)
        else:
            x = np.random.uniform(lower, upper)
            y = np.random.choice([lower, upper])
        points.append((x, y))
    return points

# Generate trajectories from start to end points
def generate_trajectory(start, end, interval=0.2, noise_sd=0.1):
    x1, y1 = start
    x2, y2 = end
    distance = np.hypot(x2 - x1, y2 - y1)
    num_points = int(distance / interval)
    x_values = np.linspace(x1, x2, num_points)
    y_values = np.linspace(y1, y2, num_points)
    
    trajectory = []
    for x, y in zip(x_values, y_values):
        noisy_x = x + np.random.normal(0, noise_sd)
        noisy_y = y + np.random.normal(0, noise_sd)
        trajectory.append((noisy_x, noisy_y))
    
    return trajectory

# Assign colours to points and generate LaTeX code for the figure

def select_colour(point, colours, raw=True):
    x, y = point
    distance = np.hypot(x, y - 2.5)

    if raw: 
        probabilities = [1 / (1 + abs(distance -1)), 1 / (1 + abs(distance - 3)), 1 / (1 + abs(distance - 5)), 1 / (1 + abs(distance - 7)), 1 / (1 + abs(distance - 9))]
        probabilities = probabilities[:len(colours)]
        probabilities = np.array(probabilities) / sum(probabilities)
        return np.random.choice(colours, p=probabilities)
    
    else:
        uniform = np.random.rand()
        if uniform < 0.1:
            return np.random.choice(colours)
        
        else:
            if distance < 2:
                return colours[0]
            elif distance < 3:
                return colours[1]
            elif distance < 4:
                return colours[2]
            elif distance < 4.5:
                return colours[3]
            else:
                return colours[4]

def generate_latex_code(trajectories, colours, r=0.1):

    for traj_id, trajectory in enumerate(trajectories):

        latex_code = []
        sublist_id = np.random.choice([0,1,2])
        sublist = colours[sublist_id: sublist_id+3]

        for point in trajectory:
            colour = select_colour(point, sublist)
            x, y = point
            latex_code.append(f"\\fill[{colour}] ({x:.2f}, {y:.2f}) circle ({r});")
        
        latex_code.append("\\draw[black, thick] (0,0) rectangle (5,5);")
        
        with open(f'trajectories_latex_{traj_id}.txt', 'w') as f:
            for line in latex_code:
                f.write(line + '\n')

# Write LaTeX code for second step

def generate_combined_latex_code(trajectories, colours, raw=True, r=0.1):
    latex_code = []

    for trajectory in trajectories:
        for point in trajectory:
            colour = select_colour(point, colours, raw)
            x, y = point
            latex_code.append(f"\\fill[{colour}] ({x:.2f}, {y:.2f}) circle ({r});")
    
    latex_code.append("\\draw[black, thick] (0,0) rectangle (5,5);")
    
    filename = 'combined_trajectories_latex_raw.txt' if raw else 'combined_trajectories_latex_detrend.txt'

    with open(filename, 'w') as f:
        for line in latex_code:
            f.write(line + '\n')

def generate_grid_from_trajectories():
    
    with open('combined_trajectories_latex_detrend.txt', 'r') as f:
        lines = f.readlines()

        points = []
        for line in lines:
            if line.startswith("\\fill"):
                parts = line.split()
                colour = parts[0].split('[')[1].split(']')[0]
                x = float(parts[1][1:-1])
                y = float(parts[2][0:-1].strip())
                points.append((x, y, colour))

        grid_code = []
        for x in np.arange(0.05, 5.05, 0.5):
            for y in np.arange(0.05, 5.05, 0.5):
                colour = 'white'
                for px, py, pcolour in points:
                    if x <= px < x + 0.4 and y <= py < y + 0.4:
                        colour = pcolour
                        break
            
                grid_code.append(f"\\fill[{colour}] ({x:.2f}, {y:.2f}) rectangle ({x+0.4:.2f}, {y+0.4:.2f});")

        with open('grid_latex.txt', 'w') as f:
            for line in grid_code:
                f.write(line + '\n')

        return points

if __name__ == "__main__":
    
    n_traj = 20

    start_points    = generate_border_points(n_traj, 0, 5)
    end_points      = generate_border_points(n_traj, 0, 5)

    print("Start points:", start_points)
    print("End points:", end_points)
    
    trajectories = [generate_trajectory(start, end) for start, end in zip(start_points, end_points)]

    for i, trajectory in enumerate(trajectories):
        print(f"Trajectory {i+1}: {trajectory}")

    colours = ['redPM', 'orangePM', 'yellowPM', 'lightgreenPM', 'greenPM']
    generate_latex_code(trajectories, colours)
    generate_combined_latex_code(trajectories, colours, True)
    generate_combined_latex_code(trajectories, colours, False)
    points = generate_grid_from_trajectories()
    print(points)

    fig, ax = plt.subplots()
    for trajectory in trajectories:
        x_values, y_values = zip(*trajectory)
        ax.plot(x_values, y_values, 'k-', alpha=0.5)
    ax.set_xlim(0, 5)
    ax.set_ylim(0, 5)
    ax.set_aspect('equal')
    ax.axis('off')
    plt.savefig('trajectories.png', dpi=300)