#r "/home/qchen/metro-base/bin/Debug/net8.0/metro-base.dll"
open System
open System.IO
open metro_base.metro

let plotDistribution (dist: Distribution) (title: string) (filename: string) =
    printfn "Generating data for: %s" title
    
    // Generate data points
    let xValues = [ for x in -10.0 .. 0.05 .. 15.0 -> x ]
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
    let csvFilename = filename.Replace(".png", ".csv")
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

// Test both trapezoidal distributions
plotDistribution (Trapezoidal(1.0, 3.0, 5.0, 7.0)) "Trapezoidal Distribution (1,3,5,7)" "imgs/trapezoidal_fixed.png"
plotDistribution (TrapezoidalPlateau(2.0, 8.0, 5.0)) "TrapezoidalPlateau Distribution (2,8,5)" "imgs/trapezoidalplateau_fixed.png"

printfn "Run 'python3 plot_distributions.py' to generate PNG plots"
