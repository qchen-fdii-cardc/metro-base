open metro_base

[<EntryPoint>]
let main argv =
    // // Test Normal distribution
    // printfn "\n=== NORMAL DISTRIBUTION ==="
    // let normalDist = norm 0.0 10.0
    // printfn "Mean: %f, Stdev: %f" normalDist.Mean normalDist.Stdev
    // printfn "InvCDF(0.95): %f" normalDist.InvCdf 0.95
    // printfn "Kp(0.95): %f" normalDist.Kp 0.95

    // // Test Uniform distribution
    // printfn "\n=== UNIFORM DISTRIBUTION ==="
    // let uniformDist = uniform 0.0 10.0
    // printfn "Mean: %f, Stdev: %f" uniformDist.Mean uniformDist.Stdev
    // printfn "InvCDF(0.95): %f" uniformDist.InvCdf 0.95
    // printfn "Kp(0.95): %f" uniformDist.Kp 0.95

    // // Test U-Shape distribution
    // printfn "\n=== U-SHAPE DISTRIBUTION ==="
    // let ushapeDist = ushape 1.0 5.0
    // printfn "Mean: %f, Stdev: %f" ushapeDist.Mean ushapeDist.Stdev
    // printfn "InvCDF(0.95): %f" ushapeDist.InvCdf 0.95
    // printfn "Kp(0.95): %f" ushapeDist.Kp 0.95

    // // Test Rayleigh distribution
    // printfn "\n=== RAYLEIGH DISTRIBUTION ==="
    // let rayleighDist = rayleigh 2.0
    // printfn "Mean: %f, Stdev: %f" rayleighDist.Mean rayleighDist.Stdev
    // printfn "InvCDF(0.95): %f" rayleighDist.InvCdf 0.95
    // printfn "Kp(0.95): %f" rayleighDist.Kp 0.95

    // // Test LogNormal distribution
    // printfn "\n=== LOG-NORMAL DISTRIBUTION ==="
    // let logNormalDist = logNormal 1.0 0.5
    // printfn "Mean: %f, Stdev: %f" logNormalDist.Mean logNormalDist.Stdev
    // printfn "InvCDF(0.95): %f" logNormalDist.InvCdf 0.95
    // printfn "Kp(0.95): %f" logNormalDist.Kp 0.95

    // // Test InvSine distribution
    // printfn "\n=== INVERSE SINE DISTRIBUTION ==="
    // let invSineDist = invSine 1.0 5.0
    // printfn "Mean: %f, Stdev: %f" invSineDist.Mean invSineDist.Stdev
    // printfn "InvCDF(0.95): %f" invSineDist.InvCdf 0.95
    // printfn "Kp(0.95): %f" invSineDist.Kp 0.95


    // let v1 = norm 0.0 1.0
    // let v2 = uniform 5.0 10.0

    // // Using overloaded operators instead of function calls
    // let v3 = v1 + v2 // Addition
    // let v4 = v1 * v2 // Multiplication
    // let v5 = v1 / v2 // Division
    // let v6 = v1 * v1 // Power (square)
    // let v7 = v1 - v2 // Subtraction
    // let v8 = -v1 // Negation
    // let v9 = v1 + 5.0 // Mixed with float
    // let v10 = 2.0 * v1 // Float multiplication
    // let v11 = v10 ** 2.0 // Power with float exponent

    // let result = eval v11 100000
    // printfn "\n=== MONTE CARLO SIMULATION (with operators) ==="
    // // compare mean Value and mean (sample value n)
    // printfn "Mean: %f" result.Mean
    // printfn "Stdev: %f" result.Stdev
    // printfn "Kp(0.95): %f" result.Kp 0.95
    // printfn "Expanded Uncertainty (95%%): %f" result.ExpandedUncertainty 0.95

    // // a long expression
    // let complexExpr = ((v1 + v2) * (v1 - v2) / v2 + 3.0) ** 2.0 - (v1 * v2 + 4.0)
    // let result = eval complexExpr 100000
    // printfn "\n=== MONTE CARLO SIMULATION (complex expression) ==="
    // // compare mean Value and mean (sample value n)
    // printfn "Mean: %f" result.Mean
    // printfn "Stdev: %f" result.Stdev
    // printfn "Kp(0.95): %f" result.Kp 0.95
    // printfn "Expanded Uncertainty (95%%): %f" result.ExpandedUncertainty 0.95

    // // sin of a uniform distribution -> inv sine distribution
    // let v12 = uniform 0.0 Math.PI / 2.0
    // let v13 = sinVal v12
    // let result = eval v13 100000
    // printfn "\n=== MONTE CARLO SIMULATION (sin of uniform) ==="
    // // compare mean Value and mean (sample value n)
    // printfn "Mean: %f" result.Mean
    // printfn "Stdev: %f" result.Stdev
    // printfn "Kp(1.0): %f" result.Kp 1.0
    // printfn "Expanded Uncertainty (95%%): %f" result.ExpandedUncertainty 0.95


    // let v14 = Distribution(InvSine(0.0, 1.0)).Eval 100000
    // printfn "\n=== INVERSE SINE DISTRIBUTION DIRECTLY ==="
    // printfn "Mean: %f, Stdev: %f" v14.Mean v14.Stdev
    // printfn "Kp(0.95): %f" v14.Kp 0.95
    // printfn "Kp(1.0): %f" v14.Kp 1.0
    // printfn "Expanded Uncertainty (95%%): %f" v14.ExpandedUncertainty 0.95

    // // Test Bootstrap distribution
    // printfn "\n=== BOOTSTRAP DISTRIBUTION ==="
    // let sampleData = [| 5.0; 7.0; 9.0; 11.0; 13.0 |]
    // printfn "Sample Data: %A" sampleData
    // printfn "Sample Size: %d" (Array.length sampleData)
    // printfn "Mean of Sample Data: %f" (Array.average sampleData)

    // printfn
    //     "stdev of Sample Data: %f"
    //     (sqrt (
    //         Array.sumBy (fun x -> (x - Array.average sampleData) ** 2.0) sampleData
    //         / float (Array.length sampleData - 1)
    //     ))

    // printfn "\nBootstrap with 1000 samples:"
    // let bootstrapDist = bootstrap 1000 sampleData
    // printfn "Mean: %f, Stdev: %f" (mean bootstrapDist) (stdev bootstrapDist)
    // printfn "InvCDF(0.95): %f" (invCdf bootstrapDist 0.95)
    // printfn "Kp(0.95): %f" (kp bootstrapDist 0.95)
    // printfn "Expanded Uncertainty (95%%): %f" (expandedUncertainty bootstrapDist 0.95)

    // printfn "\nBootstrap with 10000 samples:"
    // let bootstrapDist = Bootstrap(10000, sampleData)
    // printfn "Mean: %f, Stdev: %f" (mean bootstrapDist) (stdev bootstrapDist)
    // printfn "InvCDF(0.95): %f" (invCdf bootstrapDist 0.95)
    // printfn "Kp(0.95): %f" (kp bootstrapDist 0.95)
    // printfn "Expanded Uncertainty (95%%): %f" (expandedUncertainty bootstrapDist 0.95)
    
    let n = measurement.norm 0.0 1.0
    let u = measurement.uniform -1.0 1.0
    printfn $"N(0.0, 1.0) -> U = %f{n.Mean ()} ± %f{n.ExpandedUncertainty 0.99}"
    printfn $"U(-1.0, 1.0) -> U = %f{u.Mean ()} ± %f{u.ExpandedUncertainty 0.99}"
    let ss = n + u
    printf $"%A{ss}\n"
    let sample = ss.Eval 1000
    printf $"Sample: %A{sample}\n"
    printf $"Sample Mean: %f{stat.mean sample}\n"
    printfn $"N(0.0, 1.0)+U(-1.0, 1.0) -> U = %f{ss.Mean ()} ± %f{ss.ExpandedUncertainty 0.99}"
    
    
    0 // Return 0 for success
