#!/usr/bin/env python3
# This script should be run from the project root: python3 scripts/plot_distributions.py

import pandas as pd
import matplotlib.pyplot as plt
import sys
import os

def plot_csv(csv_file, title):
    # Read CSV data
    data = pd.read_csv(csv_file)
    
    # Create plot
    plt.figure(figsize=(10, 6))
    plt.plot(data['x'], data['density'], 'b-', linewidth=2, label='PDF')
    plt.title(title)
    plt.xlabel('x')
    plt.ylabel('Density')
    plt.grid(True, alpha=0.3)
    plt.legend()
    
    # Save plot
    png_file = csv_file.replace('.csv', '.png')
    plt.savefig(png_file, dpi=150, bbox_inches='tight')
    print(f"Saved plot to {png_file}")
    plt.close()

if __name__ == "__main__":
    # Plot all CSV files in imgs directory
    img_dir = "imgs"
    if os.path.exists(img_dir):        
        for file in os.listdir(img_dir):
            print(f"Processing {file}...")
            if file.endswith('.csv'):
                csv_path = os.path.join(img_dir, file)
                title = file.replace('.csv', '').replace('_', ' ').title()
                plot_csv(csv_path, title)
    else:
        print("No imgs directory found")
