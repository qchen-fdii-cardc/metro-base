#!/usr/bin/env fsx
// This script should be run from the project root: dotnet fsi scripts/test_trapezoidal.fsx

// Build the project first (relative to project root)
let buildProcess = System.Diagnostics.Process.Start("dotnet", "build metro-base.fsproj")
buildProcess.WaitForExit()

// Reference the built DLL (relative to project root)
#r "../bin/Debug/net8.0/metro-base.dll"
open System
open metro_base.metro

// Test Trapezoidal distributions
let testDist1 = Trapezoidal(1.0, 3.0, 5.0, 7.0)  // Should be trapezoid with flat top from 3 to 5
let testDist2 = TrapezoidalPlateau(2.0, 8.0, 5.0) // Should be trapezoid with plateau at 5.0

printfn "=== Testing Trapezoidal(1,3,5,7) ==="
printfn "PDF at x=2.0 (rising): %.6f" (pdf testDist1 2.0)
printfn "PDF at x=4.0 (plateau): %.6f" (pdf testDist1 4.0)
printfn "PDF at x=6.0 (falling): %.6f" (pdf testDist1 6.0)
printfn "PDF at x=8.0 (outside): %.6f" (pdf testDist1 8.0)
printfn "Mean: %.6f" (mean testDist1)
printfn ""

printfn "=== Testing TrapezoidalPlateau(2,8,5) ==="
printfn "PDF at x=3.0 (rising): %.6f" (pdf testDist2 3.0)
printfn "PDF at x=5.0 (plateau): %.6f" (pdf testDist2 5.0)
printfn "PDF at x=7.0 (falling): %.6f" (pdf testDist2 7.0)
printfn "PDF at x=9.0 (outside): %.6f" (pdf testDist2 9.0)
printfn "Mean: %.6f" (mean testDist2)
