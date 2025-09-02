// For more information see https://aka.ms/fsharp-console-apps





module metro =
    type Distribution = 
        | Uniform of float * float
        | Normal of float * float
        | Triangular of float * float * float
        | Trapezoidal of float * float * float * float
        | TrapezoidalPlateau of float * float * float
        | UShape of float * float // min, max
        | Rayleigh of float // scale parameter σ
        | LogNormal of float * float // μ (log-scale mean), σ (log-scale std)
        | InvSine of float * float // min, max

    let mean dist =
        match dist with
        | Uniform(mu, _) -> mu
        | Normal(a, b) -> (a + b) / 2.0
        | Triangular(min, mode, max) -> (min + mode + max) / 3.0
        | Trapezoidal(a, b, c, d) -> (a + b + c + d) / 4.0
        | TrapezoidalPlateau(a, b, plateau) -> (a + b + 2.0 * plateau) / 4.0
        | UShape(min, max) -> (min + max) / 2.0
        | Rayleigh(sigma) -> sigma * sqrt(System.Math.PI / 2.0)
        | LogNormal(mu, sigma) -> exp(mu + sigma * sigma / 2.0)
        | InvSine(min, max) -> (min + max) / 2.0
    
    let stdev dist =
        match dist with
        | Uniform(_, sigma) -> sigma
        | Normal(a, b) -> (b - a) / 4.0
        | Triangular(min, mode, max) -> sqrt((min * min + mode * mode + max * max - min * mode - min * max - mode * max) / 6.0)
        | Trapezoidal(a, b, c, d) -> sqrt((a * a + b * b + c * c + d * d - a * b - a * c - a * d - b * c - b * d - c * d) / 24.0)
        | TrapezoidalPlateau(a, b, plateau) -> sqrt((a * a + b * b + 2.0 * plateau * plateau - a * b - 2.0 * a * plateau - 2.0 * b * plateau) / 24.0)
        | UShape(min, max) -> (max - min) / (2.0 * sqrt(2.0))
        | Rayleigh(sigma) -> sigma * sqrt(2.0 - System.Math.PI / 2.0)
        | LogNormal(mu, sigma) -> sqrt((exp(sigma * sigma) - 1.0) * exp(2.0 * mu + sigma * sigma))
        | InvSine(min, max) -> (max - min) / (2.0 * sqrt(2.0))

    let erf z =
        let t = 1.0 / (1.0 + 0.5 * abs z)
        let tau = t * exp(-z * z - 1.26551223 + t * (1.00002368 + t * (0.37409196 + t * (0.09678418 + t * (-0.18628806 + t * (0.27886807 + t * (-1.13520398 + t * (1.48851587 + t * (-0.82215223 + t * 0.17087277)))))))))
        if z >= 0.0 then 1.0 - tau else tau - 1.0   

    let cdf dist x =
        match dist with
        | Uniform(mu, sigma) -> if x < mu - sqrt(3.0) * sigma then 0.0 elif x > mu + sqrt(3.0) * sigma then 1.0 else (x - (mu - sqrt(3.0) * sigma)) / (2.0 * sqrt(3.0) * sigma)
        | Normal(a, b) -> let z = (x - (a + b) / 2.0) / ((b - a) / 4.0) in 0.5 * (1.0 + erf(z / sqrt(2.0)))
        | Triangular(min, mode, max) -> if x < min then 0.0 elif x > max then 1.0 elif x < mode then (x - min) * (x - min) / ((max - min) * (mode - min)) else 1.0 - (max - x) * (max - x) / ((max - min) * (max - mode))
        | Trapezoidal(a, b, c, d) -> if x < a then 0.0 elif x > d then 1.0 elif x < b then (x - a) * (x - a) / ((b - a) * (c - a)) elif x < c then (x - a) / (d - a) else 1.0 - (d - x) * (d - x) / ((d - c) * (d - b))
        | TrapezoidalPlateau(a, b, plateau) -> if x < a then 0.0 elif x > b then 1.0 elif x < plateau then (x - a) * (x - a) / ((b - a) * (plateau - a)) elif x < b then 1.0 - (b - x) * (b - x) / ((b - plateau) * (b - a)) else 1.0
        | UShape(min, max) -> if x < min || x > max then (if x < min then 0.0 else 1.0) else let u = (x - min) / (max - min) in (2.0 / System.Math.PI) * asin(sqrt(u))
        | Rayleigh(sigma) -> if x < 0.0 then 0.0 else 1.0 - exp(-x * x / (2.0 * sigma * sigma))
        | LogNormal(mu, sigma) -> if x <= 0.0 then 0.0 else let z = (log(x) - mu) / sigma in 0.5 * (1.0 + erf(z / sqrt(2.0)))
        | InvSine(min, max) -> if x < min || x > max then (if x < min then 0.0 else 1.0) else let u = (x - min) / (max - min) in (2.0 / System.Math.PI) * asin(sqrt(u))

    let pdf dist x =
        match dist with
        | Uniform(mu, sigma) -> if x < mu - sqrt(3.0) * sigma || x > mu + sqrt(3.0) * sigma then 0.0 else 1.0 / (2.0 * sqrt(3.0) * sigma)
        | Normal(a, b) -> let z = (x - (a + b) / 2.0) / ((b - a) / 4.0) in (1.0 / ((b - a) / 4.0 * sqrt(2.0 * System.Math.PI))) * exp(-0.5 * z * z)
        | Triangular(min, mode, max) -> if x < min || x > max then 0.0 elif x < mode then 2.0 * (x - min) / ((max - min) * (mode - min)) else 2.0 * (max - x) / ((max - min) * (max - mode))
        | Trapezoidal(a, b, c, d) -> if x < a || x > d then 0.0 elif x < b then 2.0 * (x - a) / ((b - a) * (c - a)) elif x < c then 1.0 / (d - a) else 2.0 * (d - x) / ((d - c) * (d - b))
        | TrapezoidalPlateau(a, b, plateau) -> if x < a || x > b then 0.0 elif x < plateau then 2.0 * (x - a) / ((b - a) * (plateau - a)) elif x < b then 2.0 * (b - x) / ((b - plateau) * (b - a)) else 1.0 / (b - a)
        | UShape(min, max) -> if x < min || x > max then 0.0 else let u = (x - min) / (max - min) in 1.0 / (System.Math.PI * sqrt(u * (1.0 - u)) * (max - min))
        | Rayleigh(sigma) -> if x < 0.0 then 0.0 else (x / (sigma * sigma)) * exp(-x * x / (2.0 * sigma * sigma))
        | LogNormal(mu, sigma) -> if x <= 0.0 then 0.0 else (1.0 / (x * sigma * sqrt(2.0 * System.Math.PI))) * exp(-0.5 * ((log(x) - mu) / sigma) ** 2.0)
        | InvSine(min, max) -> if x < min || x > max then 0.0 else let u = (x - min) / (max - min) in 1.0 / (System.Math.PI * sqrt(u * (1.0 - u)) * (max - min))
    
    // Fast analytical inverse CDF for each distribution
    let invCdfFast dist p =
        match dist with
        | Uniform(mu, sigma) -> 
            let a = mu - sqrt(3.0) * sigma
            let b = mu + sqrt(3.0) * sigma
            a + p * (b - a)
        | Normal(a, b) -> 
            let mu = (a + b) / 2.0
            let sigma = (b - a) / 4.0
            // Approximate inverse error function using rational approximation
            let invErf z =
                let a = 0.147
                let ln1minusZ2 = log(1.0 - z * z)
                let term1 = 2.0 / (System.Math.PI * a) + ln1minusZ2 / 2.0
                let term2 = ln1minusZ2 / a
                let sign = if z >= 0.0 then 1.0 else -1.0
                sign * sqrt(sqrt(term1 * term1 - term2) - term1)
            mu + sigma * sqrt(2.0) * invErf(2.0 * p - 1.0)
        | Triangular(min, mode, max) ->
            let fc = (mode - min) / (max - min)
            if p < fc then
                min + sqrt(p * (max - min) * (mode - min))
            else
                max - sqrt((1.0 - p) * (max - min) * (max - mode))
        | Trapezoidal(a, b, c, d) ->
            let p1 = (b - a) / (2.0 * (d - a))
            let p2 = (c - a) / (d - a)
            let p3 = 1.0 - (d - c) / (2.0 * (d - a))
            if p <= p1 then
                a + sqrt(2.0 * p * (b - a) * (c - a))
            elif p <= p2 then
                a + p * (d - a)
            elif p <= p3 then
                a + p * (d - a)
            else
                d - sqrt(2.0 * (1.0 - p) * (d - c) * (d - b))
        | TrapezoidalPlateau(a, b, plateau) ->
            let p1 = (plateau - a) / (2.0 * (b - a))
            let p2 = 1.0 - (b - plateau) / (2.0 * (b - a))
            if p <= p1 then
                a + sqrt(2.0 * p * (b - a) * (plateau - a))
            elif p <= p2 then
                plateau // In the plateau region
            else
                b - sqrt(2.0 * (1.0 - p) * (b - plateau) * (b - a))
        | UShape(min, max) ->
            let u = sin(p * System.Math.PI / 2.0) ** 2.0
            min + u * (max - min)
        | Rayleigh(sigma) ->
            sigma * sqrt(-2.0 * log(1.0 - p))
        | LogNormal(mu, sigma) ->
            let invErf z =
                let a = 0.147
                let ln1minusZ2 = log(1.0 - z * z)
                let term1 = 2.0 / (System.Math.PI * a) + ln1minusZ2 / 2.0
                let term2 = ln1minusZ2 / a
                let sign = if z >= 0.0 then 1.0 else -1.0
                sign * sqrt(sqrt(term1 * term1 - term2) - term1)
            exp(mu + sigma * sqrt(2.0) * invErf(2.0 * p - 1.0))
        | InvSine(min, max) ->
            let u = sin(p * System.Math.PI / 2.0) ** 2.0
            min + u * (max - min)
    
    // Fallback binary search inverse CDF
    let invCdf dist p =
        let rec binarySearch low high =
            if high - low < 1e-6 then (low + high) / 2.0
            else
                let mid = (low + high) / 2.0
                let cdfMid = cdf dist mid
                if cdfMid < p then binarySearch mid high
                else binarySearch low mid
        binarySearch -1e6 1e6
    
    // Fast analytical inverse PDF for each distribution (where meaningful)
    let invPdfFast dist p =
        match dist with
        | Uniform(mu, sigma) -> 
            let pdfValue = 1.0 / (2.0 * sqrt(3.0) * sigma)
            if abs(p - pdfValue) < 1e-10 then mu // Only one solution at the mean
            else failwith "No solution: uniform PDF is constant"
        | Normal(a, b) -> 
            let mu = (a + b) / 2.0
            let sigma = (b - a) / 4.0
            let maxPdf = 1.0 / (sigma * sqrt(2.0 * System.Math.PI))
            if p > maxPdf then failwith "PDF value too high for normal distribution"
            elif abs(p - maxPdf) < 1e-10 then mu // At the peak
            else
                // Solve: p = (1/(σ√(2π))) * exp(-0.5 * ((x-μ)/σ)²)
                let z = sqrt(-2.0 * log(p * sigma * sqrt(2.0 * System.Math.PI)))
                mu + sigma * z // Return positive solution (could also return mu - sigma * z)
        | Triangular(min, mode, max) ->
            let maxPdf = 2.0 / (max - min)
            if p > maxPdf then failwith "PDF value too high for triangular distribution"
            elif abs(p - maxPdf) < 1e-10 then mode // At the mode
            else
                // For ascending part: p = 2(x-min)/((max-min)(mode-min))
                let x1 = min + p * (max - min) * (mode - min) / 2.0
                if x1 <= mode then x1
                else
                    // For descending part: p = 2(max-x)/((max-min)(max-mode))
                    max - p * (max - min) * (max - mode) / 2.0
        | _ -> 
            // For more complex distributions, fall back to binary search
            let rec binarySearch low high =
                if high - low < 1e-6 then (low + high) / 2.0
                else
                    let mid = (low + high) / 2.0
                    let pdfMid = pdf dist mid
                    if pdfMid < p then 
                        if mid < (mean dist) then binarySearch mid high
                        else binarySearch low mid
                    else 
                        if mid < (mean dist) then binarySearch low mid
                        else binarySearch mid high
            let m = mean dist
            let s = stdev dist
            binarySearch (m - 3.0 * s) (m + 3.0 * s)

    // Fallback binary search inverse PDF
    // Note: invPdf finds x where pdf(x) = p, which may not always be meaningful
    // for all distributions and probability values
    let invPdf dist p =
        let rec binarySearch low high =
            if high - low < 1e-6 then (low + high) / 2.0
            else
                let mid = (low + high) / 2.0
                let pdfMid = pdf dist mid
                if pdfMid < p then 
                    // PDF too small, need to move towards the mode
                    if mid < (mean dist) then binarySearch mid high
                    else binarySearch low mid
                else 
                    // PDF too large, need to move away from the mode
                    if mid < (mean dist) then binarySearch low mid
                    else binarySearch mid high
        // Start search around the mean of the distribution
        let m = mean dist
        let s = stdev dist
        binarySearch (m - 3.0 * s) (m + 3.0 * s) 


    /// Returns the expanded uncertainty for a given level of confidence p
    let expandedUncertainty dist p =
        let centeredPercentile = (1.0 + p) / 2.0 // For centered coverage interval
        let upperQuantile = invCdfFast dist centeredPercentile
        let distMean = mean dist
        let distStdev = stdev dist
        abs(upperQuantile - distMean)

    let kp dist p =
        expandedUncertainty dist p / (stdev dist)

    let quantile dist p =
        invCdf dist (1.0 - p)

    let quantileTwoSided dist p =
        let alpha = p / 2.0
        let lower = invCdf dist alpha
        let upper = invCdf dist (1.0 - alpha)
        (lower, upper)


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    
    // Test Normal distribution
    printfn "\n=== NORMAL DISTRIBUTION ==="
    let normalDist = metro.Normal(0.0, 10.0)
    printfn "Mean: %f, Stdev: %f" (metro.mean normalDist) (metro.stdev normalDist)
    printfn "InvCDF Fast(0.95): %f" (metro.invCdfFast normalDist 0.95)
    printfn "InvCDF Slow(0.95): %f" (metro.invCdf normalDist 0.95)
    printfn "Kp(0.95): %f" (metro.kp normalDist 0.95)
    
    // Test Uniform distribution
    printfn "\n=== UNIFORM DISTRIBUTION ==="
    let uniformDist = metro.Uniform(5.0, 2.0)
    printfn "Mean: %f, Stdev: %f" (metro.mean uniformDist) (metro.stdev uniformDist)
    printfn "InvCDF Fast(0.95): %f" (metro.invCdfFast uniformDist 0.95)
    printfn "InvCDF Slow(0.95): %f" (metro.invCdf uniformDist 0.95)
    printfn "Kp(0.95): %f" (metro.kp uniformDist 0.95)
    
    // Test Triangular distribution
    printfn "\n=== TRIANGULAR DISTRIBUTION ==="
    let triangularDist = metro.Triangular(2.0, 5.0, 8.0)
    printfn "Mean: %f, Stdev: %f" (metro.mean triangularDist) (metro.stdev triangularDist)
    printfn "InvCDF Fast(0.95): %f" (metro.invCdfFast triangularDist 0.95)
    printfn "InvCDF Slow(0.95): %f" (metro.invCdf triangularDist 0.95)
    printfn "Kp(0.95): %f" (metro.kp triangularDist 0.95)

    0 // Return 0 for success
