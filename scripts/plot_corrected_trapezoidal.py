
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import sys
import os

# Add support for Chinese fonts if available
plt.rcParams['font.sans-serif'] = ['DejaVu Sans', 'SimHei', 'Arial Unicode MS']
plt.rcParams['axes.unicode_minus'] = False

# Read the data
df = pd.read_csv('./imgs/corrected_trapezoidal_plots.csv')

# Create the plot
fig, ax = plt.subplots(figsize=(12, 8))

# Plot each distribution
colors = ['blue', 'red']
linestyles = ['-', '--']

for i, col in enumerate(df.columns[1:]):  # Skip 'x' column
    ax.plot(df['x'], df[col], color=colors[i % len(colors)], 
            linestyle=linestyles[i % len(linestyles)], 
            linewidth=2, label=col, alpha=0.8)

# Formatting
ax.set_xlabel('x', fontsize=12)
ax.set_ylabel('PDF', fontsize=12)
ax.set_title('Corrected Trapezoidal Distributions Comparison', fontsize=14)
ax.grid(True, alpha=0.3)
ax.legend(fontsize=11)

# Set reasonable y-axis limits
ax.set_ylim(0, max(df.iloc[:, 1:].max()) * 1.1)

# Add annotations for key parameters
ax.text(0.02, 0.98, 
        'Trapezoidal(a=2, b=3, c=5, d=8)\nTrapezoidalPlateau(a=2, b=8, plateau=3)\nplateau = length of flat section', 
        transform=ax.transAxes, fontsize=10,
        verticalalignment='top',
        bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.8))

plt.tight_layout()
plt.savefig('./imgs/corrected_trapezoidal_distributions.png', dpi=300, bbox_inches='tight')
print("Plot saved as: corrected_trapezoidal_distributions.png")

# Show parameter details
print("\nParameter Analysis:")
print("Trapezoidal(2,3,5,8): Traditional 4-parameter form")
print("  - Rising: 2→3, Flat: 3→5, Falling: 5→8")
print("TrapezoidalPlateau(2,8,3): 3-parameter form (CORRECTED)")
print("  - Range: 2→8 (width=6)")
print("  - Plateau length: 3")
print("  - Slope widths: (6-3)/2 = 1.5 each")
print("  - Rising: 2→3.5, Flat: 3.5→6.5, Falling: 6.5→8")
