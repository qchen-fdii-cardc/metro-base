
open System
open metro_base
[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    
    // Test Normal distribution
    printfn "\n=== NORMAL DISTRIBUTION ==="
    let normalDist = metro.Normal(0.0, 10.0)
    printfn "Mean: %f, Stdev: %f" (metro.mean normalDist) (metro.stdev normalDist)
    printfn "InvCDF(0.95): %f" (metro.invCdf normalDist 0.95)
    printfn "Kp(0.95): %f" (metro.kp normalDist 0.95)
    
    // Test Uniform distribution
    printfn "\n=== UNIFORM DISTRIBUTION ==="
    let uniformDist = metro.Uniform(5.0, 2.0)
    printfn "Mean: %f, Stdev: %f" (metro.mean uniformDist) (metro.stdev uniformDist)
    printfn "InvCDF(0.95): %f" (metro.invCdf uniformDist 0.95)
    printfn "Kp(0.95): %f" (metro.kp uniformDist 0.95)
    
    // Test U-Shape distribution
    printfn "\n=== U-SHAPE DISTRIBUTION ==="
    let ushapeDist = metro.UShape(2.0, 8.0)
    printfn "Mean: %f, Stdev: %f" (metro.mean ushapeDist) (metro.stdev ushapeDist)
    printfn "InvCDF(0.95): %f" (metro.invCdf ushapeDist 0.95)
    printfn "Kp(0.95): %f" (metro.kp ushapeDist 0.95)
    
    // Test Rayleigh distribution
    printfn "\n=== RAYLEIGH DISTRIBUTION ==="
    let rayleighDist = metro.Rayleigh(2.0)
    printfn "Mean: %f, Stdev: %f" (metro.mean rayleighDist) (metro.stdev rayleighDist)
    printfn "InvCDF(0.95): %f" (metro.invCdf rayleighDist 0.95)
    printfn "Kp(0.95): %f" (metro.kp rayleighDist 0.95)
    
    // Test LogNormal distribution
    printfn "\n=== LOG-NORMAL DISTRIBUTION ==="
    let logNormalDist = metro.LogNormal(1.0, 0.5)
    printfn "Mean: %f, Stdev: %f" (metro.mean logNormalDist) (metro.stdev logNormalDist)
    printfn "InvCDF(0.95): %f" (metro.invCdf logNormalDist 0.95)
    printfn "Kp(0.95): %f" (metro.kp logNormalDist 0.95)
    
    // Test InvSine distribution
    printfn "\n=== INVERSE SINE DISTRIBUTION ==="
    let invSineDist = metro.InvSine(1.0, 5.0)
    printfn "Mean: %f, Stdev: %f" (metro.mean invSineDist) (metro.stdev invSineDist)
    printfn "InvCDF(0.95): %f" (metro.invCdf invSineDist 0.95)
    printfn "Kp(0.95): %f" (metro.kp invSineDist 0.95)


    let v1 = metro.Distribution(metro.Normal(10.0, 2.0))
    let v2 = metro.Distribution(metro.Uniform(5.0, 1.0))
    let v3 = metro.add v1 v2
    let v4 = metro.multiply v1 v2
    let v5 = metro.divide v1 v2
    let v6 = metro.power v1 2.0

    let result = metro.eval v6 100000
    printfn "\n=== MONTE CARLO SIMULATION ==="
    printfn "Mean: %f, Stdev: %f" (metro.mean result) (metro.stdev result)
    printfn "InvCDF(0.95): %f" (metro.invCdf result 0.95)
    printfn "Kp(0.95): %f" (metro.kp result 0.95)
    printfn "Expanded Uncertainty (95%%): %f" (metro.expandedUncertainty result 0.95)

    0 // Return 0 for success
