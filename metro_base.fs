// F# Statistical Metrology Library - Supporting multiple probability distributions and uncertainty analysis

namespace metro_base

module math =

    // Bootstrap sampling structure for statistical resampling analysis
    type BootstrapSamples =
        struct
            val NumSamples: int // Number of samples
            val Samples: float array // Original sample data
            val SampledArray: float array // Sampled result array

            new(numSamples: int, samples: float array) =
                let rand = System.Random()
                let sampledArray = Array.zeroCreate<float> numSamples
                // Perform Bootstrap resampling
                for i in 0 .. numSamples - 1 do
                    sampledArray.[i] <- samples.[rand.Next(samples.Length)]

                { NumSamples = numSamples
                  Samples = samples
                  SampledArray = sampledArray }

            member this.Sample() = this.SampledArray
            member this.Mean() = Array.average (this.Sample())
            member this.Stdev() =
                let sampled = this.Sample()
                let mean = this.Mean()
                sqrt (Array.average (Array.map (fun x -> (x - mean) ** 2.0) sampled))

        end

    // Mathematical helper functions module
    module MathHelpers =
    // Inverse error function using Abramowitz & Stegun approximation
        let invErf x =
            let a = 0.147 // Approximation parameter
            let ln_term = log (1.0 - x * x)
            let first_term = 2.0 / (System.Math.PI * a) + ln_term / 2.0
            let second_term = ln_term / a
            let sqrt_term = sqrt (first_term * first_term - second_term)
            let result = sqrt_term - first_term
            (if x >= 0.0 then 1.0 else -1.0) * sqrt (result)

    // Inverse CDF for standard normal distribution
        let invStandardNormal p =
            if p <= 0.0 then System.Double.NegativeInfinity
            elif p >= 1.0 then System.Double.PositiveInfinity
            else sqrt (2.0) * invErf (2.0 * p - 1.0)

    // Mathematical constant π
        let pi = System.Math.PI


module stat =
    // Distribution union type - supporting multiple statistical distributions
    type Distribution =
    | Uniform of float * float // Uniform distribution (lower, upper)
    | Normal of float * float // Normal distribution (mean, std)
    | Triangular of float * float * float // Triangular distribution (min, mode, max)
    | Trapezoidal of float * float * float * float // Trapezoidal distribution
    | TrapezoidalPlateau of float * float * float // Plateau trapezoidal distribution
    | UShape of float * float // U-shape distribution (min, max)
    | Rayleigh of float // Rayleigh distribution (scale parameter σ)
    | LogNormal of float * float // Log-normal distribution (μ, σ)
    | InvSine of float * float // Inverse sine distribution (min, max)
    | Sample of float array // Empirical distribution from samples
    | Bootstrap of int * float array // Bootstrap sampling distribution
    | BootstrapStruct of math.BootstrapSamples // Bootstrap samples structure

    // Calculate distribution mean
    let mean dist =
        match dist with
        | Uniform(a, b) -> (a + b) / 2.0
        | Normal(mu, _) -> mu
        | Triangular(min, mode, max) -> (min + mode + max) / 3.0
        | Trapezoidal(a, b, c, d) -> (a + b + c + d) / 4.0
        | TrapezoidalPlateau(a, b, plateau) -> (a + b) / 2.0
        | UShape(min, max) -> (min + max) / 2.0
        | Rayleigh(sigma) -> sigma * sqrt (math.MathHelpers.pi / 2.0)
        | LogNormal(mu, sigma) -> exp (mu + sigma * sigma / 2.0)
        | InvSine(min, max) -> (min + max) / 2.0
        | Sample(samples) -> Array.average samples
        | Bootstrap(n, samples) -> math.BootstrapSamples(n, samples).Mean()
        | BootstrapStruct(bs) -> bs.Mean()

    // Calculate distribution standard deviation
    let stdev dist =
        match dist with
        | Uniform(a, b) -> (b - a) / (2.0 * sqrt (3.0))
        | Normal(_, sigma) -> sigma
        | Triangular(min, mode, max) ->
            sqrt (
                (min * min + mode * mode + max * max - min * mode - min * max - mode * max)
                / 6.0
            )
        | Trapezoidal(a, b, c, d) ->
            let mean_val = (a + b + c + d) / 4.0
            let var =
                ((a - mean_val) ** 2.0
                 + (b - mean_val) ** 2.0
                 + (c - mean_val) ** 2.0
                 + (d - mean_val) ** 2.0
                 + (a - mean_val) * (b - mean_val)
                 + (b - mean_val) * (c - mean_val)
                 + (c - mean_val) * (d - mean_val))
                / 18.0
            sqrt (abs (var))
        | TrapezoidalPlateau(a, b, plateau) ->
            let total_width = b - a
            let slope_width = (total_width - plateau) / 2.0
            let var = (total_width * total_width - plateau * plateau) / 18.0
            sqrt (abs (var))
        | UShape(min, max) -> (max - min) / (2.0 * sqrt (2.0))
        | Rayleigh(sigma) -> sigma * sqrt (2.0 - math.MathHelpers.pi / 2.0)
        | LogNormal(mu, sigma) -> sqrt ((exp (sigma * sigma) - 1.0) * exp (2.0 * mu + sigma * sigma))
        | InvSine(min, max) -> (max - min) / (2.0 * sqrt (2.0))
        | Sample(samples) ->
            let mean = Array.average samples
            sqrt (Array.average (Array.map (fun x -> (x - mean) ** 2.0) samples))
        | Bootstrap(n, samples) -> math.BootstrapSamples(n, samples).Stdev()
        | BootstrapStruct(bs) -> bs.Stdev()
    // Evaluate polynomial using Horner's method
    let evalPolyHorner t (coeffs: float list) =
        List.foldBack (fun c acc -> c + t * acc) coeffs 0.0

    // Error function (Abramowitz & Stegun approximation)
    let erf z =
        let t = 1.0 / (1.0 + 0.5 * abs z)

        // Using Horner's method to calculate polynomial part of Abramowitz & Stegun approximation
        let poly = evalPolyHorner t
                      [ 1.00002368
                        0.37409196
                        0.09678418
                        -0.18628806
                        0.27886807
                        -1.13520398
                        1.48851587
                        -0.82215223
                        0.17087277 ]

        let tau = t * exp (-z * z - 1.26551223 + t * poly)

        if z >= 0.0 then 1.0 - tau else tau - 1.0

    // Calculate cumulative distribution function
    let cdf dist x =
        match dist with
        | Uniform(a, b) ->
            if x < a then 0.0
            elif x > b then 1.0
            else (x - a) / (b - a)
        | Normal(mu, sigma) ->
            let z = (x - mu) / sigma
            0.5 * (1.0 + erf (z / sqrt (2.0)))
        | Triangular(min, mode, max) ->
            if x < min then
                0.0
            elif x > max then
                1.0
            elif x < mode then
                (x - min) * (x - min) / ((max - min) * (mode - min))
            else
                1.0 - (max - x) * (max - x) / ((max - min) * (max - mode))
        | Trapezoidal(a, b, c, d) ->
            if x < a then
                0.0
            elif x > d then
                1.0
            elif x <= b then
                (x - a) * (x - a) / ((b - a) * (d + c - b - a))
            elif x <= c then
                (2.0 * x - a - b) / (d + c - b - a)
            else
                1.0 - (d - x) * (d - x) / ((d - c) * (d + c - b - a))
        | TrapezoidalPlateau(a, b, plateau) ->
            if x < a then
                0.0
            elif x > b then
                1.0
            else
                let total_width = b - a
                let slope_width = (total_width - plateau) / 2.0
                let left_slope_end = a + slope_width
                let right_slope_start = b - slope_width
                let h = 2.0 / (total_width + plateau)
                if x <= left_slope_end then
                    h * (x - a) * (x - a) / (2.0 * slope_width)
                elif x >= right_slope_start then
                    1.0 - h * (b - x) * (b - x) / (2.0 * slope_width)
                else
                    let triangle_area = h * slope_width / 2.0
                    let rect_width = x - left_slope_end
                    triangle_area + h * rect_width
        | UShape(min, max) -> // U-shape distribution CDF
            if x < min || x > max then
                (if x < min then 0.0 else 1.0)
            else
                let u = (x - min) / (max - min) in (2.0 / math.MathHelpers.pi) * asin (sqrt (u))
        | Rayleigh(sigma) -> // Rayleigh distribution CDF
            if x < 0.0 then
                0.0
            else
                1.0 - exp (-x * x / (2.0 * sigma * sigma))
        | LogNormal(mu, sigma) -> // Log-normal distribution CDF
            if x <= 0.0 then
                0.0
            else
                let z = (log (x) - mu) / sigma in 0.5 * (1.0 + erf (z / sqrt (2.0)))
        | InvSine(min, max) -> // Inverse sine distribution CDF
            if x < min || x > max then
                (if x < min then 0.0 else 1.0)
            else
                let u = (x - min) / (max - min) in (2.0 / math.MathHelpers.pi) * asin (sqrt (u))
        | Sample(samples) -> // Sample distribution CDF
            let n = float (Array.length samples)

            if n = 0.0 then
                failwith "Sample distribution has no samples"
            else
                let count = Array.filter (fun v -> v <= x) samples |> Array.length |> float
                count / n
        | Bootstrap(n, samples) -> // Bootstrap sampling CDF
            let bootstrapSamples = math.BootstrapSamples(n, samples).Sample()
            let count = Array.filter (fun v -> v <= x) bootstrapSamples |> Array.length |> float
            count / float bootstrapSamples.Length
        | BootstrapStruct(bs) -> // Bootstrap samples structure CDF
            let sampled = bs.Sample()
            let count = Array.filter (fun v -> v <= x) sampled |> Array.length |> float
            count / float sampled.Length

    // Calculate probability density function
    let pdf dist x =
        match dist with
        | Uniform(a, b) ->
            if x < a || x > b then 0.0 else 1.0 / (b - a)
        | Normal(mu, sigma) ->
            let z = (x - mu) / sigma
            (1.0 / (sigma * sqrt (2.0 * math.MathHelpers.pi))) * exp (-0.5 * z * z)
        | Triangular(min, mode, max) ->
            if x < min || x > max then
                0.0
            elif x < mode then
                2.0 * (x - min) / ((max - min) * (mode - min))
            else
                2.0 * (max - x) / ((max - min) * (max - mode))
        | Trapezoidal(a, b, c, d) ->
            if x < a || x > d then
                0.0
            elif x <= b then
                2.0 * (x - a) / ((b - a) * (d + c - b - a))
            elif x <= c then
                2.0 / (d + c - b - a)
            else
                2.0 * (d - x) / ((d - c) * (d + c - b - a))
        | TrapezoidalPlateau(a, b, plateau) ->
            if x < a || x > b then
                0.0
            else
                let total_width = b - a
                let slope_width = (total_width - plateau) / 2.0
                let left_slope_end = a + slope_width
                let right_slope_start = b - slope_width
                let h = 2.0 / (total_width + plateau)
                if x <= left_slope_end then
                    h * (x - a) / slope_width
                elif x >= right_slope_start then
                    h * (b - x) / slope_width
                else
                    h
        | UShape(min, max) -> // U-shape distribution PDF
            if x < min || x > max then
                0.0
            else
                let u = (x - min) / (max - min) in 1.0 / (math.MathHelpers.pi * sqrt (u * (1.0 - u)) * (max - min))
        | Rayleigh(sigma) -> // Rayleigh distribution PDF
            if x < 0.0 then
                0.0
            else
                (x / (sigma * sigma)) * exp (-x * x / (2.0 * sigma * sigma))
        | LogNormal(mu, sigma) -> // Log-normal distribution PDF
            if x <= 0.0 then
                0.0
            else
                (1.0 / (x * sigma * sqrt (2.0 * math.MathHelpers.pi)))
                * exp (-0.5 * ((log (x) - mu) / sigma) ** 2.0)
        | InvSine(min, max) -> // Inverse sine distribution PDF
            if x < min || x > max then
                0.0
            else
                let u = (x - min) / (max - min) in 1.0 / (math.MathHelpers.pi * sqrt (u * (1.0 - u)) * (max - min))
        | Sample(samples) -> // Sample distribution PDF (using kernel density estimation)
            let n = float (Array.length samples)

            if n = 0.0 then
                failwith "Sample distribution has no samples"
            else
                let sampleStdev =
                    sqrt (
                        Array.average (
                            Array.map (fun x -> let mean = Array.average samples in (x - mean) ** 2.0) samples
                        )
                    )

                let bandwidth = 1.06 * sampleStdev * (n ** (-1.0 / 5.0)) // Silverman's rule of thumb

                let kernel u =
                    (1.0 / sqrt (2.0 * math.MathHelpers.pi)) * exp (-0.5 * u * u) // Gaussian kernel

                let density = Array.sum (Array.map (fun v -> kernel ((x - v) / bandwidth)) samples)
                density / (n * bandwidth)
        | Bootstrap(n, samples) -> // Bootstrap sampling PDF
            let bootstrapSamples = math.BootstrapSamples(n, samples).Sample()

            if bootstrapSamples.Length = 0 then
                failwith "Bootstrap distribution has no samples"
            else
                let sampleStdev =
                    sqrt (
                        Array.average (
                            Array.map
                                (fun x -> let mean = Array.average bootstrapSamples in (x - mean) ** 2.0)
                                bootstrapSamples
                        )
                    )

                let bandwidth = 1.06 * sampleStdev * (float bootstrapSamples.Length ** (-1.0 / 5.0)) // Silverman's rule of thumb

                let kernel u =
                    (1.0 / sqrt (2.0 * math.MathHelpers.pi)) * exp (-0.5 * u * u)

                let density =
                    Array.sum (Array.map (fun v -> kernel ((x - v) / bandwidth)) bootstrapSamples)

                density / (float bootstrapSamples.Length * bandwidth)
        | BootstrapStruct(bs) -> // Bootstrap samples structure PDF
            let sampled = bs.Sample()
            let n = float (Array.length sampled)

            if n = 0.0 then
                failwith "Bootstrap distribution has no samples"
            else
                let sampleStdev =
                    sqrt (
                        Array.average (
                            Array.map (fun x -> let mean = Array.average sampled in (x - mean) ** 2.0) sampled
                        )
                    )

                let bandwidth = 1.06 * sampleStdev * (n ** (-1.0 / 5.0)) // Silverman's rule of thumb

                let kernel u =
                    (1.0 / sqrt (2.0 * math.MathHelpers.pi)) * exp (-0.5 * u * u)

                let density = Array.sum (Array.map (fun v -> kernel ((x - v) / bandwidth)) sampled)
                density / (n * bandwidth)

    // Analytical inverse CDF for each distribution
    let invCdf dist p =
        match dist with
        | Uniform(a, b) ->
            a + p * (b - a)
        | Normal(mu, sigma) ->
            mu + sigma * math.MathHelpers.invStandardNormal (p)
        | Triangular(min, mode, max) ->
            let fc = (mode - min) / (max - min)
            if p < fc then
                min + sqrt (p * (max - min) * (mode - min))
            else
                max - sqrt ((1.0 - p) * (max - min) * (max - mode))
        | Trapezoidal(a, b, c, d) ->
            let h = 2.0 / (d + c - b - a)
            let area1 = (b - a) * h / 2.0
            let area2 = (c - b) * h
            let p1 = area1
            let p2 = area1 + area2
            if p <= p1 then
                a + sqrt (p * (b - a) * (d + c - b - a))
            elif p <= p2 then
                (p * (d + c - b - a) + a + b) / 2.0
            else
                d - sqrt ((1.0 - p) * (d - c) * (d + c - b - a))
        | TrapezoidalPlateau(a, b, plateau) ->
            let total_width = b - a
            let slope_width = (total_width - plateau) / 2.0
            let left_slope_end = a + slope_width
            let right_slope_start = b - slope_width
            let h = 2.0 / (total_width + plateau)
            let triangle_area = h * slope_width / 2.0
            let plateau_area = h * plateau
            let p1 = triangle_area
            let p2 = triangle_area + plateau_area
            if p <= p1 then
                a + sqrt (2.0 * p * slope_width / h)
            elif p <= p2 then
                left_slope_end + (p - p1) / h
            else
                b - sqrt (2.0 * (1.0 - p) * slope_width / h)
        | UShape(min, max) ->
            let u = sin (p * math.MathHelpers.pi / 2.0) ** 2.0
            min + u * (max - min)
        | Rayleigh(sigma) ->
            sigma * sqrt (-2.0 * log (1.0 - p))
        | LogNormal(mu, sigma) ->
            exp (mu + sigma * math.MathHelpers.invStandardNormal (p))
        | InvSine(min, max) ->
            let u = sin (p * math.MathHelpers.pi / 2.0) ** 2.0
            min + u * (max - min)
        | Sample(samples) ->
            let n = float (Array.length samples)
            if n = 0.0 then
                failwith "Sample distribution has no samples"
            else
                let sortedSamples = Array.sort samples
                let rank = p * (n - 1.0)
                let lowerIndex = int (floor rank)
                let upperIndex = int (ceil rank)
                if lowerIndex = upperIndex then
                    sortedSamples.[lowerIndex]
                else
                    let weight = rank - float lowerIndex
                    sortedSamples.[lowerIndex] * (1.0 - weight)
                    + sortedSamples.[upperIndex] * weight
        | Bootstrap(n, samples) ->
            let bootstrapSamples = math.BootstrapSamples(n, samples).Sample()
            let n = float (Array.length bootstrapSamples)
            if n = 0.0 then
                failwith "Bootstrap distribution has no samples"
            else
                let sortedSamples = Array.sort bootstrapSamples
                let rank = p * (n - 1.0)
                let lowerIndex = int (floor rank)
                let upperIndex = int (ceil rank)
                if lowerIndex = upperIndex then
                    sortedSamples.[lowerIndex]
                else
                    let weight = rank - float lowerIndex
                    sortedSamples.[lowerIndex] * (1.0 - weight)
                    + sortedSamples.[upperIndex] * weight
        | BootstrapStruct(bs) ->
            let sampled = bs.Sample()
            let n = float (Array.length sampled)
            if n = 0.0 then
                failwith "Bootstrap distribution has no samples"
            else
                let sortedSamples = Array.sort sampled
                let rank = p * (n - 1.0)
                let lowerIndex = int (floor rank)
                let upperIndex = int (ceil rank)
                if lowerIndex = upperIndex then
                    sortedSamples.[lowerIndex]
                else
                    let weight = rank - float lowerIndex
                    sortedSamples.[lowerIndex] * (1.0 - weight)
                    + sortedSamples.[upperIndex] * weight

    // Analytical inverse PDF for each distribution (where meaningful)
    let invPdf dist p =
        match dist with
        | Uniform(a, b) ->
            let pdfValue = 1.0 / (b - a)
            if abs (p - pdfValue) < 1e-10 then
                (a + b) / 2.0 // Return mean value when PDF matches
            else
                failwith "No solution: uniform PDF is constant"
        | Normal(mu, sigma) -> // Normal distribution inverse PDF
            let maxPdf = 1.0 / (sigma * sqrt (2.0 * math.MathHelpers.pi))
            if p > maxPdf then
                failwith "PDF value too high for normal distribution"
            elif abs (p - maxPdf) < 1e-10 then
                mu // At the peak
            else
                // Solve: p = (1/(σ√(2π))) * exp(-0.5 * ((x-μ)/σ)²)
                let z = sqrt (-2.0 * log (p * sigma * sqrt (2.0 * math.MathHelpers.pi)))
                mu + sigma * z // Return positive solution (could also return mu - sigma * z)
        | Triangular(min, mode, max) -> // Triangular distribution inverse PDF
            let maxPdf = 2.0 / (max - min)
            if p > maxPdf then
                failwith "PDF value too high for triangular distribution"
            elif abs (p - maxPdf) < 1e-10 then
                mode // At the mode
            else
                // For ascending part: p = 2(x-min)/((max-min)(mode-min))
                let x1 = min + p * (max - min) * (mode - min) / 2.0
                if x1 <= mode then
                    x1
                else
                    // For descending part: p = 2(max-x)/((max-min)(max-mode))
                    max - p * (max - min) * (max - mode) / 2.0
        | Rayleigh(sigma) -> // Rayleigh distribution inverse PDF
            if p <= 0.0 then
                failwith "PDF value must be positive for Rayleigh distribution"
            else
                // Solve: p = (x/σ²) * exp(-x²/(2σ²))
                // This requires numerical methods for general case, using Newton-Raphson
                let rec solve x_guess iter =
                    if iter > 20 then
                        x_guess
                    else
                        let pdf_val =
                            (x_guess / (sigma * sigma)) * exp (-x_guess * x_guess / (2.0 * sigma * sigma))

                        let pdf_deriv =
                            (1.0 / (sigma * sigma))
                            * exp (-x_guess * x_guess / (2.0 * sigma * sigma))
                            * (1.0 - x_guess * x_guess / (sigma * sigma))

                        let x_new = x_guess - (pdf_val - p) / pdf_deriv

                        if abs (x_new - x_guess) < 1e-10 then
                            x_new
                        else
                            solve x_new (iter + 1)

                solve sigma 0
        | LogNormal(mu, sigma) ->
            if p <= 0.0 then
                failwith "PDF value must be positive for LogNormal distribution"
            else
                // For log-normal, the mode occurs at exp(μ - σ²)
                let mode = exp (mu - sigma * sigma)
                let maxPdf = 1.0 / (mode * sigma * sqrt (2.0 * System.Math.PI))

                if p > maxPdf then
                    failwith "PDF value too high for LogNormal distribution"
                elif abs (p - maxPdf) < 1e-10 then
                    mode
                else
                    // Use Newton-Raphson for general inverse
                    let rec solve x_guess iter =
                        if iter > 20 || x_guess <= 0.0 then
                            max x_guess 1e-10
                        else
                            let pdf_val =
                                (1.0 / (x_guess * sigma * sqrt (2.0 * System.Math.PI)))
                                * exp (-0.5 * ((log (x_guess) - mu) / sigma) ** 2.0)

                            let log_x = log (x_guess)
                            let z = (log_x - mu) / sigma

                            let pdf_deriv =
                                -(1.0 / (x_guess * x_guess * sigma * sqrt (2.0 * System.Math.PI)))
                                * exp (-0.5 * z * z)
                                * (1.0 + z * z)

                            let x_new = x_guess - (pdf_val - p) / pdf_deriv

                            if abs (x_new - x_guess) < 1e-10 then
                                x_new
                            else
                                solve x_new (iter + 1)

                    solve mode 0
        | Sample(samples) ->
            // For empirical distributions, use kernel density estimation to find approximate inverse
            let n = float (Array.length samples)

            if n = 0.0 then
                failwith "Sample distribution has no samples"
            else
                let sampleStdev =
                    sqrt (
                        Array.average (
                            Array.map (fun x -> let mean = Array.average samples in (x - mean) ** 2.0) samples
                        )
                    )

                let bandwidth = 1.06 * sampleStdev * (n ** (-1.0 / 5.0)) // Silverman's rule of thumb

                let kernel u =
                    (1.0 / sqrt (2.0 * System.Math.PI)) * exp (-0.5 * u * u)

                let rec binarySearch low high =
                    if high - low < 1e-6 then
                        (low + high) / 2.0
                    else
                        let mid = (low + high) / 2.0

                        let density =
                            Array.sum (Array.map (fun v -> kernel ((mid - v) / bandwidth)) samples)
                            / (n * bandwidth)

                        if density < p then
                            binarySearch mid high
                        else
                            binarySearch low mid

                let minSample = Array.min samples
                let maxSample = Array.max samples
                binarySearch (minSample - 3.0 * bandwidth) (maxSample + 3.0 * bandwidth)
        | Bootstrap(n, samples) ->
            let bootstrapSamples = math.BootstrapSamples(n, samples).Sample()
            let n = float (Array.length bootstrapSamples)

            if n = 0.0 then
                failwith "Bootstrap distribution has no samples"
            else
                let sampleStdev =
                    sqrt (
                        Array.average (
                            Array.map
                                (fun x -> let mean = Array.average bootstrapSamples in (x - mean) ** 2.0)
                                bootstrapSamples
                        )
                    )

                let bandwidth = 1.06 * sampleStdev * (n ** (-1.0 / 5.0)) // Silverman's rule of thumb

                let kernel u =
                    (1.0 / sqrt (2.0 * math.MathHelpers.pi)) * exp (-0.5 * u * u)

                let rec binarySearch low high =
                    if high - low < 1e-6 then
                        (low + high) / 2.0
                    else
                        let mid = (low + high) / 2.0

                        let density =
                            Array.sum (Array.map (fun v -> kernel ((mid - v) / bandwidth)) bootstrapSamples)
                            / (n * bandwidth)

                        if density < p then
                            binarySearch mid high
                        else
                            binarySearch low mid

                let minSample = Array.min bootstrapSamples
                let maxSample = Array.max bootstrapSamples
                binarySearch (minSample - 3.0 * bandwidth) (maxSample + 3.0 * bandwidth)
        | BootstrapStruct(bs) ->
            let sampled = bs.Sample()
            let n = float (Array.length sampled)

            if n = 0.0 then
                failwith "Bootstrap distribution has no samples"
            else
                let sampleStdev =
                    sqrt (
                        Array.average (
                            Array.map (fun x -> let mean = Array.average sampled in (x - mean) ** 2.0) sampled
                        )
                    )

                let bandwidth = 1.06 * sampleStdev * (n ** (-1.0 / 5.0)) // Silverman's rule of thumb

                let kernel u =
                    (1.0 / sqrt (2.0 * System.Math.PI)) * exp (-0.5 * u * u)
                // Use binary search to find x such that pdf(dist, x) = p
                // This is a numerical method since analytical inverse PDF is not available for empirical distributions
                let rec binarySearch low high =
                    if high - low < 1e-6 then
                        (low + high) / 2.0
                    else
                        let mid = (low + high) / 2.0

                        let density =
                            Array.sum (Array.map (fun v -> kernel ((mid - v) / bandwidth)) sampled)
                            / (n * bandwidth)

                        if density < p then
                            binarySearch mid high
                        else
                            binarySearch low mid

                let minSample = Array.min sampled
                let maxSample = Array.max sampled
                binarySearch (minSample - 3.0 * bandwidth) (maxSample + 3.0 * bandwidth)
        | _ ->
            // For other distributions, use binary search as fallback
            let rec binarySearch low high =
                if high - low < 1e-6 then
                    (low + high) / 2.0
                else
                    let mid = (low + high) / 2.0
                    let pdfMid = pdf dist mid

                    if pdfMid < p then
                        if mid < (mean dist) then
                            binarySearch mid high
                        else
                            binarySearch low mid
                    else if mid < (mean dist) then
                        binarySearch low mid
                    else
                        binarySearch mid high

            let m = mean dist
            let s = stdev dist
            binarySearch (m - 3.0 * s) (m + 3.0 * s)


    // Returns the expanded uncertainty for a given level of confidence p
    let expandedUncertainty dist p =
        let centeredPercentile = (1.0 + p) / 2.0 // For centered coverage interval
        let upperQuantile = invCdf dist centeredPercentile
        let distMean = mean dist
        // let distStdev = stdev dist
        abs (upperQuantile - distMean)

    // Calculate coverage factor kp
    let kp dist p =
        expandedUncertainty dist p / (stdev dist)

    // Calculate quantile
    let quantile dist p = invCdf dist (1.0 - p)

    // Calculate two-sided quantile
    let quantileTwoSided dist p =
        let alpha = p / 2.0
        let lower = invCdf dist alpha
        let upper = invCdf dist (1.0 - alpha)
        (lower, upper)

module measurement =
    open math
    
    // Add a global random number generator
    let mutable private globalRandom = System.Random()   
    let reseed seed =
        globalRandom <- System.Random(seed)

    // Value type - supporting mixed calculations of exact values and distributions
    type Value =
    | Exact of float
    | Distribution of stat.Distribution
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
    | Log of Value * float
    | PowerOf10 of Value

    // Value calculation helper functions
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

        static member (+)(v1: Value, v2: Value) = Addition(v1, v2)
        static member (+)(v: Value, f: float) = Addition(v, Exact(f))
        static member (+)(f: float, v: Value) = Addition(Exact(f), v)

        static member (-)(v1: Value, v2: Value) =
            Addition(v1, Multiplication(Exact(-1.0), v2))

        static member (-)(v: Value, f: float) = Addition(v, Exact(-f))

        static member (-)(f: float, v: Value) =
            Addition(Exact(f), Multiplication(Exact(-1.0), v))

        static member (*)(v1: Value, v2: Value) = Multiplication(v1, v2)
        static member (*)(v: Value, f: float) = Multiplication(v, Exact(f))
        static member (*)(f: float, v: Value) = Multiplication(Exact(f), v)

        static member (/)(v1: Value, v2: Value) = Division(v1, v2)
        static member (/)(v: Value, f: float) = Division(v, Exact(f))
        static member (/)(f: float, v: Value) = Division(Exact(f), v)

        static member Pow(v: Value, p: float) = Power(v, p)
        static member (~-)(v: Value) = Multiplication(Exact(-1.0), v)

    // Advanced mathematical operation functions
    let sum values = List.reduce (+) values
    let product values = List.reduce (*) values

    let average values =
        (sum values) / (Exact(float (List.length values)))

    let meanValue values = average values
    let square v = v * v
    let cube v = v * v * v
    let pow v p = Power(v, p)

    // Random sampling function
    let rec sample (r: System.Random) value =
        match value with
        | Exact v -> v
        | Distribution dist ->
            match dist with
            | stat.Uniform(a, b) ->
                let u = r.NextDouble()
                a + u * (b - a)
            | stat.Normal(mu, sigma) ->
                let u1 = r.NextDouble()
                let u2 = r.NextDouble()
                mu + sigma * sqrt (-2.0 * log (u1)) * cos (2.0 * MathHelpers.pi * u2)
            | stat.Triangular(min, mode, max) ->
                let u = r.NextDouble()
                let fc = (mode - min) / (max - min)
                if u < fc then
                    min + sqrt (u * (max - min) * (mode - min))
                else
                    max - sqrt ((1.0 - u) * (max - min) * (max - mode))
            | stat.Trapezoidal(a, b, c, d) ->
                let u = r.NextDouble()
                let p1 = (b - a) / (2.0 * (d - a))
                let p2 = (c - a) / (d - a)
                let p3 = 1.0 - (d - c) / (2.0 * (d - a))
                if u <= p1 then a + sqrt (2.0 * u * (b - a) * (c - a))
                elif u <= p2 then a + u * (d - a)
                elif u <= p3 then a + u * (d - a)
                else d - sqrt (2.0 * (1.0 - u) * (d - c) * (d - b))
            | stat.TrapezoidalPlateau(a, b, plateau) ->
                let u = r.NextDouble()
                stat.invCdf (stat.TrapezoidalPlateau(a, b, plateau)) u
            | stat.UShape(min, max) ->
                let u = r.NextDouble()
                let v = sin (u * MathHelpers.pi / 2.0) ** 2.0
                min + v * (max - min)
            | stat.Rayleigh(sigma) ->
                let u = r.NextDouble()
                sigma * sqrt (-2.0 * log (1.0 - u))
            | stat.LogNormal(mu, sigma) ->
                let u1 = r.NextDouble()
                let u2 = r.NextDouble()
                let z = sqrt (-2.0 * log (u1)) * cos (2.0 * MathHelpers.pi * u2)
                exp (mu + sigma * z)
            | stat.InvSine(min, max) ->
                let u = r.NextDouble()
                let v = sin (u * MathHelpers.pi / 2.0) ** 2.0
                min + v * (max - min)
            | stat.Sample(samples) ->
                if samples.Length = 0 then
                    failwith "Sample distribution has no samples"
                else
                    let index = r.Next(0, samples.Length)
                    samples.[index]
            | stat.Bootstrap(n, samples) ->
                if samples.Length = 0 then
                    failwith "Bootstrap distribution has no samples"
                else
                    let bootstrapSamples = BootstrapSamples(n, samples).Sample()
                    if bootstrapSamples.Length = 0 then
                        failwith "Bootstrap distribution has no samples"
                    else
                        let index = r.Next(0, bootstrapSamples.Length)
                        bootstrapSamples.[index]
            | stat.BootstrapStruct(bs) ->
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

    // Monte Carlo evaluation function
    let eval (value: Value) numSamples =
        let samples = Array.init numSamples (fun _ -> sample globalRandom value)
        stat.Sample(samples)

    // Value type extension method
    type Value with
    // define a static property for default number of samples
        member this.Eval(numSamples: int) = eval this numSamples

        member this.Mean(?numSamples: int) =
            match this with
            | Exact v -> v
            | Distribution dist -> stat.mean dist
            | _ -> (stat.mean (eval this (defaultArg numSamples 10000)))

        member this.StDev(?numSamples: int) =
            match this with
            | Exact _ -> 0.0
            | Distribution dist -> stat.stdev dist
            | _ -> (stat.stdev (eval this (defaultArg numSamples 10000)))

        member this.ExpandedUncertainty(p: float, ?numSamples: int) =
            match this with
            | Exact _ -> 0.0
            | Distribution dist -> stat.expandedUncertainty dist p
            | _ -> (stat.expandedUncertainty (eval this (defaultArg numSamples 10000)) p)

        member this.Kp(p: float, ?numSamples: int) =
            match this with
            | Exact _ -> 0.0
            | Distribution dist -> stat.kp dist p
            | _ -> (stat.kp (eval this (defaultArg numSamples 10000)) p)

        member this.Quantile(p: float, ?numSamples: int) =
            match this with
            | Exact v -> v
            | Distribution dist -> stat.quantile dist p
            | _ -> (stat.quantile (eval this (defaultArg numSamples 10000)) p)

        member this.QuantileTwoSided(p: float, ?numSamples: int) =
            match this with
            | Exact v -> (v, v)
            | Distribution dist -> stat.quantileTwoSided dist p
            | _ -> (stat.quantileTwoSided (eval this (defaultArg numSamples 10000)) p)

        member this.InvPdf(p: float, ?numSamples: int) =
            match this with
            | Exact v ->
                if abs v < 1e-10 then
                    failwith "PDF is not defined for exact zero value"
                else
                    failwith "PDF is not defined for exact values"
            | Distribution dist -> stat.invPdf dist p
            | _ -> (stat.invPdf (eval this (defaultArg numSamples 10000)) p)

        member this.InvCdf(p: float, ?numSamples: int) =
            match this with
            | Exact v -> v
            | Distribution dist -> stat.invCdf dist p
            | _ -> (stat.invCdf (eval this (defaultArg numSamples 10000)) p)

        member this.Pdf(x: float, ?numSamples: int) =
            match this with
            | Exact v ->
                if abs v < 1e-10 then
                    failwith "PDF is not defined for exact zero value"
                else
                    failwith "PDF is not defined for exact values"
            | Distribution dist -> stat.pdf dist x
            | _ -> (stat.pdf (eval this (defaultArg numSamples 10000)) x)

        member this.Cdf(x: float, ?numSamples: int) =
            match this with
            | Exact v ->
                if abs v < 1e-10 then
                    failwith "CDF is not defined for exact zero value"
                else
                    failwith "CDF is not defined for exact values"
            | Distribution dist -> stat.cdf dist x
            | _ -> (stat.cdf (eval this (defaultArg numSamples 10000)) x)

    let uniform a b = Distribution(stat.Uniform(a, b))
    let norm mu sigma = Distribution(stat.Normal(mu, sigma))

    let triangular min mode max =
        Distribution(stat.Triangular(min, mode, max))

    let trapezoidal a b c d =
        Distribution(stat.Trapezoidal(a, b, c, d))

    let trapezoidalPlateau a b plateau =
        Distribution(stat.TrapezoidalPlateau(a, b, plateau))

    let uShape min max = Distribution(stat.UShape(min, max))
    let rayleigh sigma = Distribution(stat.Rayleigh(sigma))
    let logNormal mu sigma = Distribution(stat.LogNormal(mu, sigma))
    let invSine min max = Distribution(stat.InvSine(min, max))
    let sampleDist samples = Distribution(stat.Sample(samples))

    let bootstrap n samples =
        Distribution(stat.Bootstrap(n, samples))

    let bootstrapStruct bs = Distribution(stat.BootstrapStruct(bs))
