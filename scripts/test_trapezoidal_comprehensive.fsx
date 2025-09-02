#r "/home/qchen/metro-base/bin/Debug/net8.0/metro-base.dll"
open System
open metro_base.metro

// Numerical integration using trapezoidal rule
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

printfn "=== Comprehensive Trapezoidal Distribution Tests ==="
printfn ""

// Test Trapezoidal(1,3,5,7)
let trap1 = Trapezoidal(1.0, 3.0, 5.0, 7.0)
printfn "Trapezoidal(1,3,5,7):"
printfn "  PDF integration (should be ~1.0): %.6f" (integrate (pdf trap1) 0.0 8.0 1000)
printfn "  CDF(1.0) = %.6f, CDF(7.0) = %.6f" (cdf trap1 1.0) (cdf trap1 7.0)
printfn "  Mean: %.6f (expected: 4.0)" (mean trap1)
printfn "  Stdev: %.6f" (stdev trap1)

// Add more test points
printfn "  PDF values:"
for x in [0.5; 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 7.5] do
    printfn "    PDF(%.1f) = %.6f" x (pdf trap1 x)

printfn ""

// Test TrapezoidalPlateau(2,8,5)
let trap2 = TrapezoidalPlateau(2.0, 8.0, 5.0)
printfn "TrapezoidalPlateau(2,8,5):"
printfn "  PDF integration (should be ~1.0): %.6f" (integrate (pdf trap2) 1.0 9.0 1000)
printfn "  CDF(2.0) = %.6f, CDF(8.0) = %.6f" (cdf trap2 2.0) (cdf trap2 8.0)
printfn "  Mean: %.6f (expected: 5.0)" (mean trap2)
printfn "  Stdev: %.6f" (stdev trap2)

// Add more test points
printfn "  PDF values:"
for x in [1.5; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 8.5] do
    printfn "    PDF(%.1f) = %.6f" x (pdf trap2 x)

printfn ""

// Test inverse CDF
printfn "=== Inverse CDF Tests ==="
printfn "Trapezoidal(1,3,5,7):"
for p in [0.1; 0.25; 0.5; 0.75; 0.9] do
    let x = invCdf trap1 p
    let p_back = cdf trap1 x
    printfn "  invCDF(%.2f) = %.6f, CDF(%.6f) = %.6f" p x x p_back

printfn ""
printfn "TrapezoidalPlateau(2,8,5):"
for p in [0.1; 0.25; 0.5; 0.75; 0.9] do
    let x = invCdf trap2 p
    let p_back = cdf trap2 x
    printfn "  invCDF(%.2f) = %.6f, CDF(%.6f) = %.6f" p x x p_back
