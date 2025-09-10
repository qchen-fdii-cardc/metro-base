#!/usr/bin/env fsx


// Build the project first (relative to project root)
let buildProcess =
    System.Diagnostics.Process.Start("dotnet", "build metro-base.fsproj")

buildProcess.WaitForExit()

// Reference the built DLL (relative to project root)
#r "nuget: metro-base.fsharp.qc, 0.2.5"

open System
open System.IO
open metro_base.stat

let plotDistribution (dist: Distribution) (title: string) (filename: string) =
    printfn "Generating data for: %s" title

    // Generate data points
    let xValues = [ for x in -10.0 .. 0.05..15.0 -> x ]

    let data =
        xValues
        |> List.map (fun x ->
            let y = pdf dist x

            if System.Double.IsInfinity(y) || System.Double.IsNaN(y) then
                (x, 0.0)
            else
                (x, y))
        |> List.filter (fun (x, y) -> y > 1e-10) // Filter out very small values

    // Save to CSV for easy plotting with other tools (relative to project root)
    let csvFilename = "../imgs/" + Path.GetFileNameWithoutExtension(filename) + ".csv"

    let csvLines =
        "x,density" :: (data |> List.map (fun (x, y) -> sprintf "%.6f,%.10f" x y))

    File.WriteAllLines(csvFilename, csvLines)
    printfn "Saved data to %s" csvFilename

    // Also print some basic statistics
    let meanVal = mean dist
    let stdevVal = stdev dist
    printfn "Distribution stats: mean=%.3f, stdev=%.3f" meanVal stdevVal
    printfn ""

// Test both trapezoidal distributions
plotDistribution (Trapezoidal(1.0, 3.0, 5.0, 7.0)) "Trapezoidal Distribution (1,3,5,7)" "imgs/trapezoidal_fixed.png"

plotDistribution
    (TrapezoidalPlateau(2.0, 8.0, 5.0))
    "TrapezoidalPlateau Distribution (2,8,5)"
    "imgs/trapezoidalplateau_fixed.png"
// This script should be// Test both trapezoidal distributions (files saved to imgs directory)
plotDistribution (Trapezoidal(1.0, 3.0, 5.0, 7.0)) "Trapezoidal Distribution (1,3,5,7)" "trapezoidal_fixed.png"

plotDistribution
    (TrapezoidalPlateau(2.0, 8.0, 3.0))
    "TrapezoidalPlateau Distribution (2,8,3)"
    "trapezoidalplateau_fixed.png" //n from the project root: dotnet fsi scripts/test_fixed_trapezoidal_viz.fsx

printfn "Run 'python3 plot_distributions.py' to generate PNG plots"
