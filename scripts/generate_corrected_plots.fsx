#!/usr/bin/env fsx
open System.IO

// Build the project first
let buildProcess = System.Diagnostics.Process.Start("dotnet", "build metro-base.fsproj")
buildProcess.WaitForExit()

// Reference the built DLL
#r "bin/Debug/net8.0/metro-base.dll"
open metro_base.metro

// Ensure imgs directory exists
let imgsDir = "imgs"
if not (Directory.Exists(imgsDir)) then
    Directory.CreateDirectory(imgsDir) |> ignore

// Generate visualization data for the corrected trapezoidal distributions
let distributions = [
    ("Trapezoidal(2,3,5,8)", Trapezoidal(2.0, 3.0, 5.0, 8.0))
    ("TrapezoidalPlateau(2,8,3)", TrapezoidalPlateau(2.0, 8.0, 3.0))
]

// Generate x values
let xMin, xMax = 0.0, 10.0
let step = 0.1
let xValues = [xMin .. step .. xMax]

// Create CSV data
let createCsvData() =
    let header = "x," + (distributions |> List.map fst |> String.concat ",")
    let rows = 
        xValues 
        |> List.map (fun x -> 
            let pdfValues = distributions |> List.map (fun (_, dist) -> pdf dist x |> sprintf "%.6f")
            sprintf "%.1f,%s" x (String.concat "," pdfValues))
    header :: rows
    |> String.concat "\n"

let csvData = createCsvData()
let csvPath = Path.Combine(imgsDir, "corrected_trapezoidal_plots.csv")
File.WriteAllText(csvPath, csvData)

// Generate Python plotting script
let pythonScript = """#!/usr/bin/env python3
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import sys
import os

# Add support for Chinese fonts if available
plt.rcParams['font.sans-serif'] = ['DejaVu Sans', 'SimHei', 'Arial Unicode MS']
plt.rcParams['axes.unicode_minus'] = False

# Read the data from imgs directory
df = pd.read_csv('imgs/corrected_trapezoidal_plots.csv')

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
ax.set_title('Corrected Trapezoidal Distributions Comparison\\n修正后的梯形分布对比', fontsize=14)
ax.grid(True, alpha=0.3)
ax.legend(fontsize=11)

# Set reasonable y-axis limits
ax.set_ylim(0, max(df.iloc[:, 1:].max()) * 1.1)

# Add annotations for key parameters
ax.text(0.02, 0.98, 
        'Trapezoidal(a=2, b=3, c=5, d=8)\\nTrapezoidalPlateau(a=2, b=8, plateau=3)\\nplateau = length of flat section\\n高原 = 平坦段长度', 
        transform=ax.transAxes, fontsize=10,
        verticalalignment='top',
        bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.8))

plt.tight_layout()
plt.savefig('imgs/corrected_trapezoidal_distributions.png', dpi=300, bbox_inches='tight')
print("Plot saved as: imgs/corrected_trapezoidal_distributions.png")

# Show parameter details
print("\\nParameter Analysis:")
print("Trapezoidal(2,3,5,8): Traditional 4-parameter form")
print("  - Rising: 2→3, Flat: 3→5, Falling: 5→8")
print("TrapezoidalPlateau(2,8,3): 3-parameter form (CORRECTED)")
print("  - Range: 2→8 (width=6)")
print("  - Plateau length: 3")
print("  - Slope widths: (6-3)/2 = 1.5 each")
print("  - Rising: 2→3.5, Flat: 3.5→6.5, Falling: 6.5→8")
"""

let pythonScriptPath = Path.Combine(imgsDir, "plot_corrected_trapezoidal.py")
File.WriteAllText(pythonScriptPath, pythonScript)

printfn "Generated corrected trapezoidal distributions data and plotting script"
printfn "Files created:"
printfn "  - %s" csvPath
printfn "  - %s" pythonScriptPath
printfn ""
printfn "To generate the plot, run from project root:"
printfn "  python3 %s" pythonScriptPath
