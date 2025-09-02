
open System
open metro_base.metro
[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    
    // Test Normal distribution
    printfn "\n=== NORMAL DISTRIBUTION ==="
    let normalDist = Normal(0.0, 10.0)
    printfn "Mean: %f, Stdev: %f" (mean normalDist) (stdev normalDist)
    printfn "InvCDF(0.95): %f" (invCdf normalDist 0.95)
    printfn "Kp(0.95): %f" (kp normalDist 0.95)

    // Test Uniform distribution
    printfn "\n=== UNIFORM DISTRIBUTION ==="
    let uniformDist = Uniform(5.0, 2.0)
    printfn "Mean: %f, Stdev: %f" (mean uniformDist) (stdev uniformDist)
    printfn "InvCDF(0.95): %f" (invCdf uniformDist 0.95)
    printfn "Kp(0.95): %f" (kp uniformDist 0.95)

    // Test U-Shape distribution
    printfn "\n=== U-SHAPE DISTRIBUTION ==="
    let ushapeDist = UShape(2.0, 8.0)
    printfn "Mean: %f, Stdev: %f" (mean ushapeDist) (stdev ushapeDist)
    printfn "InvCDF(0.95): %f" (invCdf ushapeDist 0.95)
    printfn "Kp(0.95): %f" (kp ushapeDist 0.95)
    
    // Test Rayleigh distribution
    printfn "\n=== RAYLEIGH DISTRIBUTION ==="
    let rayleighDist = Rayleigh(2.0)
    printfn "Mean: %f, Stdev: %f" (mean rayleighDist) (stdev rayleighDist)
    printfn "InvCDF(0.95): %f" (invCdf rayleighDist 0.95)
    printfn "Kp(0.95): %f" (kp rayleighDist 0.95)
    
    // Test LogNormal distribution
    printfn "\n=== LOG-NORMAL DISTRIBUTION ==="
    let logNormalDist = LogNormal(1.0, 0.5)
    printfn "Mean: %f, Stdev: %f" (mean logNormalDist) (stdev logNormalDist)
    printfn "InvCDF(0.95): %f" (invCdf logNormalDist 0.95)
    printfn "Kp(0.95): %f" (kp logNormalDist 0.95)
    
    // Test InvSine distribution
    printfn "\n=== INVERSE SINE DISTRIBUTION ==="
    let invSineDist = InvSine(1.0, 5.0)
    printfn "Mean: %f, Stdev: %f" (mean invSineDist) (stdev invSineDist)
    printfn "InvCDF(0.95): %f" (invCdf invSineDist 0.95)
    printfn "Kp(0.95): %f" (kp invSineDist 0.95)


    let v1 = Distribution(Normal(10.0, 2.0))
    let v2 = Distribution(Uniform(5.0, 1.0))
    
    // Using overloaded operators instead of function calls
    let v3 = v1 + v2        // Addition
    let v4 = v1 * v2        // Multiplication
    let v5 = v1 / v2        // Division
    let v6 = v1 * v1        // Power (square)
    let v7 = v1 - v2        // Subtraction
    let v8 = -v1            // Negation
    let v9 = v1 + 5.0       // Mixed with float
    let v10 = 2.0 * v1      // Float multiplication
    let v11 = v10 ** 2.0 // Power with float exponent

    let result = eval v11 100000
    printfn "\n=== MONTE CARLO SIMULATION (with operators) ==="
    // compare mean Value and mean (sample value n)
    printfn "Mean: %f" (mean result)
    printfn "Stdev: %f" (stdev result)
    printfn "Kp(0.95): %f" (kp result 0.95)
    printfn "Expanded Uncertainty (95%%): %f" (expandedUncertainty result 0.95)

    // a long expression
    let complexExpr = ((v1 + v2) * (v1 - v2) / v2 + 3.0) ** 2.0 - (v1 * v2 + 4.0)
    let result = eval complexExpr 100000
    printfn "\n=== MONTE CARLO SIMULATION (complex expression) ==="
    // compare mean Value and mean (sample value n)
    printfn "Mean: %f" (mean result)
    printfn "Stdev: %f" (stdev result)
    printfn "Kp(0.95): %f" (kp result 0.95)
    printfn "Expanded Uncertainty (95%%): %f" (expandedUncertainty result 0.95)

    // sin of a uniform distribution -> inv sine distribution
    let v12 = Distribution(Uniform(0.0, Math.PI / 2.0))
    let v13 = sinVal v12
    let result = eval v13 100000
    printfn "\n=== MONTE CARLO SIMULATION (sin of uniform) ==="
    // compare mean Value and mean (sample value n)
    printfn "Mean: %f" (mean result)
    printfn "Stdev: %f" (stdev result)
    printfn "Kp(1.0): %f" (kp result 1.0)
    printfn "Expanded Uncertainty (95%%): %f" (expandedUncertainty result 0.95)


    let v14 = Distribution(InvSine(0.0, 1.0)).Eval 100000
    printfn "\n=== INVERSE SINE DISTRIBUTION DIRECTLY ==="
    printfn "Mean: %f, Stdev: %f" (mean v14) (stdev v14)
    printfn "Kp(0.95): %f" (kp v14 0.95)
    printfn "Kp(1.0): %f" (kp v14 1.0)
    printfn "Expanded Uncertainty (95%%): %f" (expandedUncertainty v14 0.95)

    // Test Bootstrap distribution
    printfn "\n=== BOOTSTRAP DISTRIBUTION ==="
    let sampleData = [| 5.0; 7.0; 9.0; 11.0; 13.0 |]
    printfn "Sample Data: %A" sampleData
    printfn "Sample Size: %d" (Array.length sampleData)
    printfn "Mean of Sample Data: %f" (Array.average sampleData)
    printfn "stdev of Sample Data: %f" (sqrt (Array.sumBy (fun x -> (x - Array.average sampleData) ** 2.0) sampleData / float (Array.length sampleData - 1)))
    
    printfn "\nBootstrap with 1000 samples:"
    let bootstrapDist = Bootstrap(1000, sampleData)
    printfn "Mean: %f, Stdev: %f" (mean bootstrapDist) (stdev bootstrapDist)
    printfn "InvCDF(0.95): %f" (invCdf bootstrapDist 0.95)
    printfn "Kp(0.95): %f" (kp bootstrapDist 0.95)
    printfn "Expanded Uncertainty (95%%): %f" (expandedUncertainty bootstrapDist 0.95)

    printfn "\nBootstrap with 10000 samples:"
    let bootstrapDist = Bootstrap(10000, sampleData)
    printfn "Mean: %f, Stdev: %f" (mean bootstrapDist) (stdev bootstrapDist)
    printfn "InvCDF(0.95): %f" (invCdf bootstrapDist 0.95)
    printfn "Kp(0.95): %f" (kp bootstrapDist 0.95)
    printfn "Expanded Uncertainty (95%%): %f" (expandedUncertainty bootstrapDist 0.95)
    0 // Return 0 for success
