#!/usr/bin/env fsx
// This script should be run from the project root: dotnet fsi scripts/test_corrected_trapezoidalplateau.fsx

// Build the project first (relative to project root)
let buildProcess = System.Diagnostics.Process.Start("dotnet", "build metro-base.fsproj")
buildProcess.WaitForExit()

// Reference the built DLL (relative to project root)
#r "../bin/Debug/net8.0/metro-base.dll"
open System
open metro_base.metro

// Test the corrected TrapezoidalPlateau implementation
// Parameters: a=2.0, b=8.0, plateau=3.0 means:
// - Range from 2 to 8 (total width = 6)
// - Plateau length = 3.0
// - Slope widths = (6 - 3) / 2 = 1.5 each
// - Rising slope: 2.0 to 3.5  
// - Flat plateau: 3.5 to 6.5
// - Falling slope: 6.5 to 8.0

let trapPlateau = TrapezoidalPlateau(2.0, 8.0, 3.0)

printfn "=== TrapezoidalPlateau(2.0, 8.0, 3.0) Corrected ==="
printfn "Total width: %.1f, Plateau length: %.1f" (8.0 - 2.0) 3.0
printfn "Slope widths: %.1f each" ((8.0 - 2.0 - 3.0) / 2.0)
printfn "Rising slope: 2.0 to %.1f" (2.0 + (8.0 - 2.0 - 3.0) / 2.0)
printfn "Flat plateau: %.1f to %.1f" (2.0 + (8.0 - 2.0 - 3.0) / 2.0) (8.0 - (8.0 - 2.0 - 3.0) / 2.0)
printfn "Falling slope: %.1f to 8.0" (8.0 - (8.0 - 2.0 - 3.0) / 2.0)
printfn ""

// Test PDF values at key points
printfn "PDF values:"
for x in [1.5; 2.0; 2.5; 3.0; 3.5; 4.0; 5.0; 6.0; 6.5; 7.0; 7.5; 8.0; 8.5] do
    let pdfVal = pdf trapPlateau x
    printfn "  PDF(%.1f) = %.6f" x pdfVal

printfn ""

// Test CDF values
printfn "CDF values:"
for x in [2.0; 3.0; 3.5; 4.0; 5.0; 6.0; 6.5; 7.0; 8.0] do
    let cdfVal = cdf trapPlateau x
    printfn "  CDF(%.1f) = %.6f" x cdfVal

printfn ""

// Test mean and std
printfn "Mean: %.6f (expected: 5.0)" (mean trapPlateau)
printfn "Stdev: %.6f" (stdev trapPlateau)

printfn ""

// Test inverse CDF
printfn "Inverse CDF tests:"
for p in [0.1; 0.25; 0.5; 0.75; 0.9] do
    let x = invCdf trapPlateau p
    let p_back = cdf trapPlateau x
    printfn "  invCDF(%.2f) = %.6f, CDF(%.6f) = %.6f" p x x p_back

printfn ""

// Numerical integration test
let integrate f a b n =
    let h = (b - a) / float n
    let sum = 
        [0..n] 
        |> List.map (fun i -> 
            let x = a + float i * h
            let weight = if i = 0 || i = n then 1.0 else 2.0
            weight * f x)
        |> List.sum
    sum * h / 2.0

printfn "PDF integration (should be ~1.0): %.6f" (integrate (pdf trapPlateau) 1.0 9.0 1000)
