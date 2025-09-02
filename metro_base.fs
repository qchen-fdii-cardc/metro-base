// For more information see https://aka.ms/fsharp-console-apps

namespace metro_base
module metro =

    type BootstrapSamples = struct
        val NumSamples: int
        val Samples: float array
        val SampledArray : float array
        new(numSamples: int, samples: float array) = 
            let rand = System.Random()
            let sampledArray = Array.zeroCreate<float> numSamples
            for i in 0 .. numSamples - 1 do
                sampledArray.[i] <- samples.[rand.Next(samples.Length)]            
            { NumSamples = numSamples; Samples = samples; SampledArray =  sampledArray}

        member this.Sample() =
            this.SampledArray
        member this.Mean() =
            Array.average (this.Sample())
        member this.Stdev() =
            let sampled = this.Sample()
            let mean = this.Mean()
            sqrt(Array.average (Array.map (fun x -> (x - mean) ** 2.0) sampled))
        
    end

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
        | Sample of float array // empirical distribution from samples
        | Bootstrap of int * float array // empirical distribution from bootstrap samples

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
        | Sample(samples) -> Array.average samples
        | Bootstrap(n, samples) -> 
            let bs = BootstrapSamples(n, samples)
            bs.Mean()
    
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
        | Sample(samples) -> 
            let mean = Array.average samples
            sqrt(Array.average (Array.map (fun x -> (x - mean) ** 2.0) samples))

        | Bootstrap(n, samples) -> 
            let bs = BootstrapSamples(n, samples)
            bs.Stdev()
        

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
        | Sample(samples) -> 
            let n = float (Array.length samples)
            if n = 0.0 then failwith "Sample distribution has no samples"
            else
                let count = Array.filter (fun v -> v <= x) samples |> Array.length |> float
                count / n
        | Bootstrap(n, samples) ->
            let bs = BootstrapSamples(n, samples)
            let sampled = bs.Sample()
            let count = Array.filter (fun v -> v <= x) sampled |> Array.length |> float
            count / float n
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
        | Sample(samples) -> 
            let n = float (Array.length samples)
            if n = 0.0 then failwith "Sample distribution has no samples"
            else
                let sampleStdev = sqrt(Array.average (Array.map (fun x -> let mean = Array.average samples in (x - mean) ** 2.0) samples))
                let bandwidth = 1.06 * sampleStdev * (n ** (-1.0 / 5.0)) // Silverman's rule of thumb
                let kernel u = (1.0 / sqrt(2.0 * System.Math.PI)) * exp(-0.5 * u * u)
                let density = Array.sum (Array.map (fun v -> kernel ((x - v) / bandwidth)) samples)
                density / (n * bandwidth)

        | Bootstrap(n, samples) ->
            let bs = BootstrapSamples(n, samples)
            let sampled = bs.Sample()
            let sampleStdev = sqrt(Array.average (Array.map (fun x -> let mean = Array.average sampled in (x - mean) ** 2.0) sampled))
            let bandwidth = 1.06 * sampleStdev * (float n ** (-1.0 / 5.0)) // Silverman's rule of thumb
            let kernel u = (1.0 / sqrt(2.0 * System.Math.PI)) * exp(-0.5 * u * u)
            let density = Array.sum (Array.map (fun v -> kernel ((x - v) / bandwidth)) sampled)
            density / (float n * bandwidth)
    // Analytical inverse CDF for each distribution
    let invCdf dist p =
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
        | Sample(samples) -> 
            let n = float (Array.length samples)
            if n = 0.0 then failwith "Sample distribution has no samples"
            else
                let sortedSamples = Array.sort samples
                let rank = p * (n - 1.0)
                let lowerIndex = int (floor rank)
                let upperIndex = int (ceil rank)
                if lowerIndex = upperIndex then
                    sortedSamples.[lowerIndex]
                else
                    let weight = rank - float lowerIndex
                    sortedSamples.[lowerIndex] * (1.0 - weight) + sortedSamples.[upperIndex] * weight

        | Bootstrap(n, samples) ->
            let bs = BootstrapSamples(n, samples)
            let sampled = bs.Sample()
            let n = float (Array.length sampled)
            if n = 0.0 then failwith "Bootstrap distribution has no samples"
            else
                let sortedSamples = Array.sort sampled
                let rank = p * (n - 1.0)
                let lowerIndex = int (floor rank)
                let upperIndex = int (ceil rank)
                if lowerIndex = upperIndex then
                    sortedSamples.[lowerIndex]
                else
                    let weight = rank - float lowerIndex
                    sortedSamples.[lowerIndex] * (1.0 - weight) + sortedSamples.[upperIndex] * weight

    // Analytical inverse PDF for each distribution (where meaningful)
    let invPdf dist p =
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
        | Rayleigh(sigma) ->
            if p <= 0.0 then failwith "PDF value must be positive for Rayleigh distribution"
            else
                // Solve: p = (x/σ²) * exp(-x²/(2σ²))
                // This requires numerical methods for general case, using Newton-Raphson
                let rec solve x_guess iter =
                    if iter > 20 then x_guess
                    else
                        let pdf_val = (x_guess / (sigma * sigma)) * exp(-x_guess * x_guess / (2.0 * sigma * sigma))
                        let pdf_deriv = (1.0 / (sigma * sigma)) * exp(-x_guess * x_guess / (2.0 * sigma * sigma)) * (1.0 - x_guess * x_guess / (sigma * sigma))
                        let x_new = x_guess - (pdf_val - p) / pdf_deriv
                        if abs(x_new - x_guess) < 1e-10 then x_new
                        else solve x_new (iter + 1)
                solve sigma 0
        | LogNormal(mu, sigma) ->
            if p <= 0.0 then failwith "PDF value must be positive for LogNormal distribution"
            else
                // For log-normal, the mode occurs at exp(μ - σ²)
                let mode = exp(mu - sigma * sigma)
                let maxPdf = 1.0 / (mode * sigma * sqrt(2.0 * System.Math.PI))
                if p > maxPdf then failwith "PDF value too high for LogNormal distribution"
                elif abs(p - maxPdf) < 1e-10 then mode
                else
                    // Use Newton-Raphson for general inverse
                    let rec solve x_guess iter =
                        if iter > 20 || x_guess <= 0.0 then max x_guess 1e-10
                        else
                            let pdf_val = (1.0 / (x_guess * sigma * sqrt(2.0 * System.Math.PI))) * exp(-0.5 * ((log(x_guess) - mu) / sigma) ** 2.0)
                            let log_x = log(x_guess)
                            let z = (log_x - mu) / sigma
                            let pdf_deriv = -(1.0 / (x_guess * x_guess * sigma * sqrt(2.0 * System.Math.PI))) * exp(-0.5 * z * z) * (1.0 + z * z)
                            let x_new = x_guess - (pdf_val - p) / pdf_deriv
                            if abs(x_new - x_guess) < 1e-10 then x_new
                            else solve x_new (iter + 1)
                    solve mode 0
        | Sample(samples) ->
            // For empirical distributions, use kernel density estimation to find approximate inverse
            let n = float (Array.length samples)
            if n = 0.0 then failwith "Sample distribution has no samples"
            else
                let sampleStdev = sqrt(Array.average (Array.map (fun x -> let mean = Array.average samples in (x - mean) ** 2.0) samples))
                let bandwidth = 1.06 * sampleStdev * (n ** (-1.0 / 5.0)) // Silverman's rule of thumb
                let kernel u = (1.0 / sqrt(2.0 * System.Math.PI)) * exp(-0.5 * u * u)
                let rec binarySearch low high =
                    if high - low < 1e-6 then (low + high) / 2.0
                    else
                        let mid = (low + high) / 2.0
                        let density = Array.sum (Array.map (fun v -> kernel ((mid - v) / bandwidth)) samples) / (n * bandwidth)
                        if density < p then binarySearch mid high
                        else binarySearch low mid
                let minSample = Array.min samples
                let maxSample = Array.max samples
                binarySearch (minSample - 3.0 * bandwidth) (maxSample + 3.0 * bandwidth)
        | Bootstrap(n, samples) ->
            let bs = BootstrapSamples(n, samples)
            let sampled = bs.Sample()
            let n = float (Array.length sampled)
            if n = 0.0 then failwith "Bootstrap distribution has no samples"    
            else
                let sampleStdev = sqrt(Array.average (Array.map (fun x -> let mean = Array.average sampled in (x - mean) ** 2.0) sampled))
                let bandwidth = 1.06 * sampleStdev * (n ** (-1.0 / 5.0)) // Silverman's rule of thumb
                let kernel u = (1.0 / sqrt(2.0 * System.Math.PI)) * exp(-0.5 * u * u)
                let rec binarySearch low high =
                    if high - low < 1e-6 then (low + high) / 2.0
                    else
                        let mid = (low + high) / 2.0
                        let density = Array.sum (Array.map (fun v -> kernel ((mid - v) / bandwidth)) sampled) / (n * bandwidth)
                        if density < p then binarySearch mid high
                        else binarySearch low mid
                let minSample = Array.min sampled
                let maxSample = Array.max sampled
                binarySearch (minSample - 3.0 * bandwidth) (maxSample + 3.0 * bandwidth)
        | _ -> 
            // For other distributions, use binary search as fallback
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


    /// Returns the expanded uncertainty for a given level of confidence p
    let expandedUncertainty dist p =
        let centeredPercentile = (1.0 + p) / 2.0 // For centered coverage interval
        let upperQuantile = invCdf dist centeredPercentile
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


    type Value = 
        | Exact of float
        | Distribution of Distribution
        | Addition of Value * Value
        | Bias of Value * float
        | Multiplication of Value * Value
        | Division of Value * Value
        | Power of Value * float
        | Min of Value * Value
        | Max of Value * Value
        | Abs of Value
        | Sqrt of Value
        | Ln of Value
        | Exp of Value
        | Sin of Value
        | Cos of Value
        | Tan of Value
        | ASin of Value
        | ACos of Value
        | ATan of Value
        | ATan2 of Value * Value
        | Sinh of Value
        | Cosh of Value
        | Tanh of Value
        | Floor of Value
        | Ceil of Value
        | Round of Value
        | Log10 of Value
        | Log2 of Value
        | Log of Value * float // log base
        | PowerOf10 of Value

    let add v1 v2 = Addition(v1, v2)
    let multiply v1 v2 = Multiplication(v1, v2)
    let divide v1 v2 = Division(v1, v2)
    let power v p = Power(v, p)
    let minVal v1 v2 = Min(v1, v2)
    let maxVal v1 v2 = Max(v1, v2)
    let absVal v = Abs v
    let sqrtVal v = Sqrt v
    let lnVal v = Ln v
    let expVal v = Exp v
    let sinVal v = Sin v
    let cosVal v = Cos v
    let tanVal v = Tan v
    let asinVal v = ASin v
    let acosVal v = ACos v
    let atanVal v = ATan v
    let atan2Val v1 v2 = ATan2(v1, v2)
    let sinhVal v = Sinh v
    let coshVal v = Cosh v
    let tanhVal v = Tanh v
    let floorVal v = Floor v
    let ceilVal v = Ceil v
    let roundVal v = Round v
    let log10Val v = Log10 v
    let log2Val v = Log2 v
    let logVal v b = Log(v, b)
    let powerOf10Val v = PowerOf10 v

    // Operator overloads for Value type
    type Value with
        static member (+) (v1: Value, v2: Value) = Addition(v1, v2)
        static member (+) (v: Value, f: float) = Addition(v, Exact(f))
        static member (+) (f: float, v: Value) = Addition(Exact(f), v)
        
        static member (-) (v1: Value, v2: Value) = Addition(v1, Multiplication(Exact(-1.0), v2))
        static member (-) (v: Value, f: float) = Addition(v, Exact(-f))
        static member (-) (f: float, v: Value) = Addition(Exact(f), Multiplication(Exact(-1.0), v))
        
        static member (*) (v1: Value, v2: Value) = Multiplication(v1, v2)
        static member (*) (v: Value, f: float) = Multiplication(v, Exact(f))
        static member (*) (f: float, v: Value) = Multiplication(Exact(f), v)
        
        static member (/) (v1: Value, v2: Value) = Division(v1, v2)
        static member (/) (v: Value, f: float) = Division(v, Exact(f))
        static member (/) (f: float, v: Value) = Division(Exact(f), v)
        
        static member Pow (v: Value, p: float) = Power(v, p)
        static member (~-) (v: Value) = Multiplication(Exact(-1.0), v)

    let sum values = List.reduce (+) values
    let product values = List.reduce (*) values
    let average values = (sum values) / (Exact (float (List.length values)))
    let meanValue values = average values
    let square v = v * v
    let cube v = v * v * v
    let pow v p = Power(v, p)  // Helper function for power operations

    let rec sample (r: System.Random) value =
        match value with
        | Exact v -> v 
        | Distribution dist -> 
            match dist with
            | Uniform(mu, sigma) -> 
                let a = mu - sqrt(3.0) * sigma
                let b = mu + sqrt(3.0) * sigma
                let u = r.NextDouble()
                a + u * (b - a)
            | Normal(a, b) -> 
                let mu = (a + b) / 2.0
                let sigma = (b - a) / 4.0
                // Box-Muller transform
                let u1 = r.NextDouble()
                let u2 = r.NextDouble()
                mu + sigma * sqrt(-2.0 * log(u1)) * cos(2.0 * System.Math.PI * u2)
            | Triangular(min, mode, max) ->
                let u = r.NextDouble()
                let fc = (mode - min) / (max - min)
                if u < fc then
                    min + sqrt(u * (max - min) * (mode - min))
                else
                    max - sqrt((1.0 - u) * (max - min) * (max - mode))
            | Trapezoidal(a, b, c, d) ->
                let u = r.NextDouble()
                let p1 = (b - a) / (2.0 * (d - a))
                let p2 = (c - a) / (d - a)
                let p3 = 1.0 - (d - c) / (2.0 * (d - a))
                if u <= p1 then
                    a + sqrt(2.0 * u * (b - a) * (c - a))
                elif u <= p2 then
                    a + u * (d - a)
                elif u <= p3 then
                    a + u * (d - a)
                else
                    d - sqrt(2.0 * (1.0 - u) * (d - c) * (d - b))
            | TrapezoidalPlateau(a, b, plateau) ->
                let u = r.NextDouble()
                let p1 = (plateau - a) / (2.0 * (b - a))
                let p2 = 1.0 - (b - plateau) / (2.0 * (b - a))
                if u <= p1 then
                    a + sqrt(2.0 * u * (b - a) * (plateau - a))
                elif u <= p2 then
                    plateau // In the plateau region
                else
                    b - sqrt(2.0 * (1.0 - u) * (b - plateau) * (b - a))
            | UShape(min, max) ->
                let u = r.NextDouble()
                let v = sin(u * System.Math.PI / 2.0) ** 2
                min + v * (max - min)
            | Rayleigh(sigma) ->
                let u = r.NextDouble()
                sigma * sqrt(-2.0 * log(1.0 - u))
            | LogNormal(mu, sigma) ->
                let u1 = r.NextDouble()
                let u2 = r.NextDouble()
                let z = sqrt(-2.0 * log(u1)) * cos(2.0 * System.Math.PI * u2)
                exp(mu + sigma * z)
            | InvSine(min, max) ->
                let u = r.NextDouble()
                let v = sin(u * System.Math.PI / 2.0) ** 2
                min + v * (max - min)
            | Sample(samples) -> 
                if samples.Length = 0 then failwith "Sample distribution has no samples"
                else
                    let index = r.Next(0, samples.Length)
                    samples.[index]

            | Bootstrap(n, samples) -> 
                if samples.Length = 0 then failwith "Bootstrap distribution has no samples"
                else
                    let bs = BootstrapSamples(n, samples)
                    let sampled = bs.Sample()
                    let index = r.Next(0, sampled.Length)
                    sampled.[index]
        | Addition(v1, v2) -> sample r v1 + sample r v2
        | Bias(v, b) -> sample r v + b
        | Multiplication(v1, v2) -> sample r v1 * sample r v2
        | Division(v1, v2) -> sample r v1 / sample r v2
        | Power(v, p) -> (sample r v) ** p
        | Min(v1, v2) -> min (sample r v1) (sample r v2)
        | Max(v1, v2) -> max (sample r v1) (sample r v2)
        | Abs v -> abs (sample r v)
        | Sqrt v -> sqrt (sample r v)
        | Ln v -> log (sample r v)
        | Exp v -> exp (sample r v)
        | Sin v -> sin (sample r v)
        | Cos v -> cos (sample r v)
        | Tan v -> tan (sample r v)
        | ASin v -> asin (sample r v)
        | ACos v -> acos (sample r v)
        | ATan v -> atan (sample r v)
        | ATan2(v1, v2) -> atan2 (sample r v1) (sample r v2)
        | Sinh v -> sinh (sample r v)
        | Cosh v -> cosh (sample r v)
        | Tanh v -> tanh (sample r v)
        | Floor v -> floor (sample r v)
        | Ceil v -> ceil (sample r v)
        | Round v -> round (sample r v)
        | Log10 v -> log10 (sample r v)
        | Log2 v -> log (sample r v) / log 2.0
        | Log(v, b) -> log (sample r v) / log b
        | PowerOf10 v -> 10.0 ** (sample r v)   

    let eval value numSamples =
        let r = System.Random()
        let samples = Array.init numSamples (fun _ -> sample r value)
        Sample(samples)

    type Value with
        member this.Eval(numSamples: int) =
            eval this numSamples