#!/usr/bin/env fsx
open System
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

let plotDistribution (dist: Distribution) (title: string) (filename: string) =
    printfn "Generating data for: %s" title
    
    // Generate data points
    let xValues = [ for x in -20.0 .. 0.1 .. 20.0 -> x ]
    let data = 
        xValues 
        |> List.map (fun x -> 
            let y = pdf dist x
            if System.Double.IsInfinity(y) || System.Double.IsNaN(y) then
                (x, 0.0)
            else
                (x, y))
        |> List.filter (fun (x, y) -> y > 1e-10) // Filter out very small values
    
    // Save to CSV for easy plotting with other tools
    let csvFilename = Path.Combine(imgsDir, Path.GetFileNameWithoutExtension(filename) + ".csv")
    let csvLines = 
        "x,density" :: 
        (data |> List.map (fun (x, y) -> sprintf "%.6f,%.10f" x y))
    
    File.WriteAllLines(csvFilename, csvLines)
    printfn "Saved data to %s" csvFilename
    
    // Also print some basic statistics
    let meanVal = mean dist
    let stdevVal = stdev dist
    printfn "Distribution stats: mean=%.3f, stdev=%.3f" meanVal stdevVal
    printfn ""

// Create a Python plotting script
let createPythonPlotScript() =
    let pythonScript = """#!/usr/bin/env python3
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
            if file.endswith('.csv'):
                csv_path = os.path.join(img_dir, file)
                title = file.replace('.csv', '').replace('_', ' ').title()
                plot_csv(csv_path, title)
    else:
        print("No imgs directory found")
"""
    
    let pythonScriptPath = Path.Combine(imgsDir, "plot_distributions.py")
    File.WriteAllText(pythonScriptPath, pythonScript)
    printfn "Created Python plotting script: %s" pythonScriptPath
    printfn "Run it with: python3 %s" pythonScriptPath
    printfn ""

// Create the Python plotting script first
createPythonPlotScript()

printfn "Generating distribution visualizations..."
printfn "All data and images will be saved to: %s/" imgsDir
printfn ""

// Generate all distributions
plotDistribution (Normal(0.0, 10.0)) "Normal Distribution (mean=0, stdev=10)" "normal_distribution.png"
plotDistribution (Uniform(5.0, 2.0)) "Uniform Distribution (mean=5, stdev=2)" "uniform_distribution.png"
plotDistribution (Triangular(5.0, 7.0, 9.0)) "Triangular Distribution (min=5, mode=7, max=9)" "triangular_distribution.png"    
plotDistribution (Trapezoidal(5.0, 6.0, 8.0, 9.0)) "Trapezoidal Distribution (a=5, b=6, c=8, d=9)" "trapezoidal_distribution.png"  
plotDistribution (TrapezoidalPlateau(5.0, 9.0, 2.0)) "TrapezoidalPlateau Distribution (a=5, b=9, plateau=2)" "trapezoidalplateau_distribution.png"
plotDistribution (UShape(2.0, 8.0)) "U-Shape Distribution (min=2, max=8)" "ushape_distribution.png"
plotDistribution (Rayleigh(2.0)) "Rayleigh Distribution (scale=2)" "rayleigh_distribution.png"
plotDistribution (LogNormal(1.0, 0.5)) "Log-Normal Distribution (mu=1, sigma=0.5)" "lognormal_distribution.png"
plotDistribution (InvSine(1.0, 5.0)) "Inverse Sine Distribution (min=1, max=5)" "invsine_distribution.png"
plotDistribution (Bootstrap(1000, [| 5.0; 7.0; 9.0; 11.0; 13.0 |])) "Bootstrap Distribution (n=1000, samples=[5,7,9,11,13])" "bootstrap_distribution.png"
plotDistribution (Bootstrap(500, [| 5.0; 7.0; 9.0; 11.0; 13.0 |])) "Bootstrap Distribution (n=500, samples=[5,7,9,11,13])" "bootstrap_distribution_500.png"

printfn "Distribution data generation complete!"
printfn ""
printfn "To generate plots, run from project root:"
printfn "  python3 imgs/plot_distributions.py"
printfn ""
printfn "All CSV files and images are in the imgs/ directory."