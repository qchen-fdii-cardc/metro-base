// F# 统计计量学库 - 支持多种概率分布和不确定度分析
// F# Statistical Metrology Library - Supporting multiple probability distributions and uncertainty analysis

namespace metro_base

module metro =

    // Bootstrap 抽样结构体 - 用于自举法统计分析
    // Bootstrap sampling structure for statistical resampling analysis
    type BootstrapSamples =
        struct
            val NumSamples: int // 抽样数量 / Number of samples
            val Samples: float array // 原始样本数据 / Original sample data
            val SampledArray: float array // 抽样结果数组 / Sampled result array

            new(numSamples: int, samples: float array) =
                let rand = System.Random()
                let sampledArray = Array.zeroCreate<float> numSamples
                // 进行 Bootstrap 重抽样 / Perform Bootstrap resampling
                for i in 0 .. numSamples - 1 do
                    sampledArray.[i] <- samples.[rand.Next(samples.Length)]

                { NumSamples = numSamples
                  Samples = samples
                  SampledArray = sampledArray }

            // 获取抽样结果 / Get sampling results
            member this.Sample() = this.SampledArray
            // 计算样本均值 / Calculate sample mean
            member this.Mean() = Array.average (this.Sample())
            // 计算样本标准差 / Calculate sample standard deviation
            member this.Stdev() =
                let sampled = this.Sample()
                let mean = this.Mean()
                sqrt (Array.average (Array.map (fun x -> (x - mean) ** 2.0) sampled))

        end

    // 数学辅助函数模块 / Mathematical helper functions module
    module MathHelpers =
        // 误差函数的逆函数 (Abramowitz & Stegun 近似)
        // Inverse error function using Abramowitz & Stegun approximation
        let invErf x =
            let a = 0.147 // 近似参数 / Approximation parameter
            let ln_term = log (1.0 - x * x)
            let first_term = 2.0 / (System.Math.PI * a) + ln_term / 2.0
            let second_term = ln_term / a
            let sqrt_term = sqrt (first_term * first_term - second_term)
            let result = sqrt_term - first_term
            (if x >= 0.0 then 1.0 else -1.0) * sqrt (result)

        // 标准正态分布的逆累积分布函数 / Inverse CDF for standard normal distribution
        let invStandardNormal p =
            if p <= 0.0 then System.Double.NegativeInfinity
            elif p >= 1.0 then System.Double.PositiveInfinity
            else sqrt (2.0) * invErf (2.0 * p - 1.0)

        // 数学常数 π / Mathematical constant π
        let pi = System.Math.PI

    // // Bootstrap辅助函数 / Bootstrap helper functions
    // let createBootstrapSamples n (samples: float array) =
    //     let rand = System.Random()
    //     Array.init n (fun _ -> samples.[rand.Next(samples.Length)])

    // let bootstrapMean n (samples: float array) =
    //     let bootstrapSamples = createBootstrapSamples n samples
    //     Array.average bootstrapSamples

    // let bootstrapStdev n (samples: float array) =
    //     let bootstrapSamples = createBootstrapSamples n samples
    //     let mean = Array.average bootstrapSamples
    //     sqrt(Array.average (Array.map (fun x -> (x - mean) ** 2.0) bootstrapSamples))

    // 概率分布联合类型 - 支持多种统计分布
    // Distribution union type - supporting multiple statistical distributions
    type Distribution =
        | Uniform of float * float // 均匀分布(下界, 上界) / Uniform distribution (lower, upper)
        | Normal of float * float // 正态分布(均值, 标准差) / Normal distribution (mean, std)
        | Triangular of float * float * float // 三角分布(最小值, 众数, 最大值) / Triangular distribution (min, mode, max)
        | Trapezoidal of float * float * float * float // 梯形分布(a, b, c, d) / Trapezoidal distribution
        | TrapezoidalPlateau of float * float * float // 平台梯形分布 / Plateau trapezoidal distribution
        | UShape of float * float // U型分布(最小值, 最大值) / U-shape distribution (min, max)
        | Rayleigh of float // 瑞利分布(尺度参数σ) / Rayleigh distribution (scale parameter σ)
        | LogNormal of float * float // 对数正态分布(μ, σ) / Log-normal distribution (μ, σ)
        | InvSine of float * float // 反正弦分布(最小值, 最大值) / Inverse sine distribution (min, max)
        | Sample of float array // 样本经验分布 / Empirical distribution from samples
        | Bootstrap of int * float array // Bootstrap抽样分布 / Bootstrap sampling distribution
        | BootstrapStruct of BootstrapSamples // Bootstrap样本结构 / Bootstrap samples structure

    // 计算分布的均值 / Calculate distribution mean
    let mean dist =
        match dist with
        | Uniform(a, b) -> (a + b) / 2.0 // 均匀分布均值 / Uniform distribution mean
        | Normal(mu, _) -> mu // 正态分布均值 / Normal distribution mean
        | Triangular(min, mode, max) -> (min + mode + max) / 3.0 // 三角分布均值 / Triangular mean
        | Trapezoidal(a, b, c, d) -> (a + b + c + d) / 4.0 // 梯形分布均值 / Trapezoidal mean
        | TrapezoidalPlateau(a, b, plateau) -> (a + b) / 2.0 // 平台梯形均值 / Plateau trapezoidal mean (symmetric)
        | UShape(min, max) -> (min + max) / 2.0 // U型分布均值 / U-shape distribution mean
        | Rayleigh(sigma) -> sigma * sqrt (MathHelpers.pi / 2.0) // 瑞利分布均值 / Rayleigh distribution mean
        | LogNormal(mu, sigma) -> exp (mu + sigma * sigma / 2.0) // 对数正态分布均值 / Log-normal mean
        | InvSine(min, max) -> (min + max) / 2.0 // 反正弦分布均值 / Inverse sine distribution mean
        | Sample(samples) -> Array.average samples // 样本分布均值 / Sample distribution mean
        | Bootstrap(n, samples) -> // Bootstrap抽样均值 / Bootstrap sampling mean
            BootstrapSamples(n, samples).Mean()
        | BootstrapStruct(bs) -> // Bootstrap样本结构均值 / Bootstrap samples structure mean
            bs.Mean()

    // 计算分布的标准差 / Calculate distribution standard deviation
    let stdev dist =
        match dist with
        | Uniform(a, b) -> (b - a) / (2.0 * sqrt (3.0)) // 均匀分布标准差 / Uniform distribution std
        | Normal(_, sigma) -> sigma // 正态分布标准差 / Normal distribution std
        | Triangular(min, mode, max) ->
            sqrt (
                (min * min + mode * mode + max * max - min * mode - min * max - mode * max)
                / 6.0
            ) // 三角分布标准差 / Triangular std
        | Trapezoidal(a, b, c, d) -> // 梯形分布标准差 / Trapezoidal std
            // Correct formula for trapezoidal distribution variance
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

            sqrt (abs (var)) // Use abs to avoid NaN from numerical errors
        | TrapezoidalPlateau(a, b, plateau) -> // 平台梯形标准差 / Plateau trapezoidal std
            // For trapezoid with plateau length, variance calculation
            let total_width = b - a
            let slope_width = (total_width - plateau) / 2.0
            // Variance for trapezoidal distribution with flat plateau
            let var = (total_width * total_width - plateau * plateau) / 18.0
            sqrt (abs (var))
        | UShape(min, max) -> (max - min) / (2.0 * sqrt (2.0)) // U型分布标准差 / U-shape distribution std
        | Rayleigh(sigma) -> sigma * sqrt (2.0 - MathHelpers.pi / 2.0) // 瑞利分布标准差 / Rayleigh distribution std
        | LogNormal(mu, sigma) -> sqrt ((exp (sigma * sigma) - 1.0) * exp (2.0 * mu + sigma * sigma)) // 对数正态分布标准差 / Log-normal std
        | InvSine(min, max) -> (max - min) / (2.0 * sqrt (2.0)) // 反正弦分布标准差 / Inverse sine std
        | Sample(samples) -> // 样本分布标准差 / Sample distribution std
            let mean = Array.average samples
            sqrt (Array.average (Array.map (fun x -> (x - mean) ** 2.0) samples))
        | Bootstrap(n, samples) -> // Bootstrap抽样标准差 / Bootstrap sampling std
            BootstrapSamples(n, samples).Stdev()
        | BootstrapStruct(bs) -> // Bootstrap样本结构标准差 / Bootstrap samples structure std
            bs.Stdev()
    // 误差函数 (Abramowitz & Stegun 近似) / Error function (Abramowitz & Stegun approximation)
    let erf z =
        let t = 1.0 / (1.0 + 0.5 * abs z)

        // 使用 Horner 法计算 Abramowitz & Stegun 近似中的多项式部分
        let poly =
            let horner (coeffs: float list) = List.foldBack (fun c acc -> c + t * acc) coeffs 0.0
            horner [ 1.00002368;
                     0.37409196;
                     0.09678418;
                    -0.18628806;
                     0.27886807;
                    -1.13520398;
                     1.48851587;
                    -0.82215223;
                     0.17087277 ]

        let tau =
            t * exp (-z * z - 1.26551223 + t * poly)

        if z >= 0.0 then 1.0 - tau else tau - 1.0

    // 计算累积分布函数 (CDF) / Calculate cumulative distribution function
    // 计算累积分布函数 (CDF) / Calculate cumulative distribution function
    let cdf dist x =
        match dist with
        | Uniform(a, b) -> // 均匀分布CDF / Uniform distribution CDF
            if x < a then
                0.0
            elif x > b then
                1.0
            else
                (x - a) / (b - a)
        | Normal(mu, sigma) -> // 正态分布CDF / Normal distribution CDF
            let z = (x - mu) / sigma
            0.5 * (1.0 + erf (z / sqrt (2.0)))
        | Triangular(min, mode, max) -> // 三角分布CDF / Triangular distribution CDF
            if x < min then
                0.0
            elif x > max then
                1.0
            elif x < mode then
                (x - min) * (x - min) / ((max - min) * (mode - min))
            else
                1.0 - (max - x) * (max - x) / ((max - min) * (max - mode))
        | Trapezoidal(a, b, c, d) -> // 梯形分布CDF / Trapezoidal distribution CDF
            if x < a then
                0.0
            elif x > d then
                1.0
            elif x <= b then
                // Rising part: CDF = (x-a)² / ((b-a)(d+c-b-a))
                (x - a) * (x - a) / ((b - a) * (d + c - b - a))
            elif x <= c then
                // Flat part: CDF = (2x - a - b) / (d + c - b - a)
                (2.0 * x - a - b) / (d + c - b - a)
            else
                // Falling part: CDF = 1 - (d-x)² / ((d-c)(d+c-b-a))
                1.0 - (d - x) * (d - x) / ((d - c) * (d + c - b - a))
        | TrapezoidalPlateau(a, b, plateau) -> // 平台梯形分布CDF / Plateau trapezoidal CDF
            if x < a then
                0.0
            elif x > b then
                1.0
            else
                let total_width = b - a
                let slope_width = (total_width - plateau) / 2.0 // Width of each slope
                let left_slope_end = a + slope_width // Where rising slope ends
                let right_slope_start = b - slope_width // Where falling slope starts
                let h = 2.0 / (total_width + plateau) // Height

                if x <= left_slope_end then
                    // Rising part: CDF = integral of triangular rise
                    h * (x - a) * (x - a) / (2.0 * slope_width)
                elif x >= right_slope_start then
                    // Falling part: 1 - remaining triangular area
                    1.0 - h * (b - x) * (b - x) / (2.0 * slope_width)
                else
                    // Flat plateau region: area of triangle + area of rectangle
                    let triangle_area = h * slope_width / 2.0
                    let rect_width = x - left_slope_end
                    triangle_area + h * rect_width
        | UShape(min, max) -> // U型分布CDF / U-shape distribution CDF
            if x < min || x > max then
                (if x < min then 0.0 else 1.0)
            else
                let u = (x - min) / (max - min) in (2.0 / MathHelpers.pi) * asin (sqrt (u))
        | Rayleigh(sigma) -> // 瑞利分布CDF / Rayleigh distribution CDF
            if x < 0.0 then
                0.0
            else
                1.0 - exp (-x * x / (2.0 * sigma * sigma))
        | LogNormal(mu, sigma) -> // 对数正态分布CDF / Log-normal distribution CDF
            if x <= 0.0 then
                0.0
            else
                let z = (log (x) - mu) / sigma in 0.5 * (1.0 + erf (z / sqrt (2.0)))
        | InvSine(min, max) -> // 反正弦分布CDF / Inverse sine distribution CDF
            if x < min || x > max then
                (if x < min then 0.0 else 1.0)
            else
                let u = (x - min) / (max - min) in (2.0 / MathHelpers.pi) * asin (sqrt (u))
        | Sample(samples) -> // 样本分布CDF / Sample distribution CDF
            let n = float (Array.length samples)

            if n = 0.0 then
                failwith "Sample distribution has no samples"
            else
                let count = Array.filter (fun v -> v <= x) samples |> Array.length |> float
                count / n
        | Bootstrap(n, samples) -> // Bootstrap抽样CDF / Bootstrap sampling CDF
            let bootstrapSamples = BootstrapSamples(n, samples).Sample()
            let count = Array.filter (fun v -> v <= x) bootstrapSamples |> Array.length |> float
            count / float bootstrapSamples.Length
        | BootstrapStruct(bs) -> // Bootstrap样本结构CDF / Bootstrap samples structure CDF
            let sampled = bs.Sample()
            let count = Array.filter (fun v -> v <= x) sampled |> Array.length |> float
            count / float sampled.Length

    // 计算概率密度函数 (PDF) / Calculate probability density function
    let pdf dist x =
        match dist with
        | Uniform(a, b) -> // 均匀分布PDF / Uniform distribution PDF
            if x < a || x > b then
                0.0
            else
                1.0 / (b - a)
        | Normal(mu, sigma) -> // 正态分布PDF / Normal distribution PDF
            let z = (x - mu) / sigma
            (1.0 / (sigma * sqrt (2.0 * MathHelpers.pi))) * exp (-0.5 * z * z)
        | Triangular(min, mode, max) -> // 三角分布PDF / Triangular distribution PDF
            if x < min || x > max then
                0.0
            elif x < mode then
                2.0 * (x - min) / ((max - min) * (mode - min))
            else
                2.0 * (max - x) / ((max - min) * (max - mode))
        | Trapezoidal(a, b, c, d) -> // 梯形分布PDF / Trapezoidal distribution PDF
            if x < a || x > d then
                0.0
            elif x <= b then
                // Rising part: PDF = 2(x-a) / ((b-a)(d+c-b-a))
                2.0 * (x - a) / ((b - a) * (d + c - b - a))
            elif x <= c then
                // Flat part: PDF = 2 / (d+c-b-a)
                2.0 / (d + c - b - a)
            else
                // Falling part: PDF = 2(d-x) / ((d-c)(d+c-b-a))
                2.0 * (d - x) / ((d - c) * (d + c - b - a))
        | TrapezoidalPlateau(a, b, plateau) -> // 平台梯形分布PDF / Plateau trapezoidal PDF
            if x < a || x > b then
                0.0
            else
                let total_width = b - a
                let slope_width = (total_width - plateau) / 2.0 // Width of each slope
                let left_slope_end = a + slope_width // Where rising slope ends
                let right_slope_start = b - slope_width // Where falling slope starts
                let h = 2.0 / (total_width + plateau) // Height to normalize area to 1

                if x <= left_slope_end then
                    // Rising part: PDF = h * (x-a) / slope_width
                    h * (x - a) / slope_width
                elif x >= right_slope_start then
                    // Falling part: PDF = h * (b-x) / slope_width
                    h * (b - x) / slope_width
                else
                    // Flat plateau: PDF = h
                    h
        | UShape(min, max) -> // U型分布PDF / U-shape distribution PDF
            if x < min || x > max then
                0.0
            else
                let u = (x - min) / (max - min) in 1.0 / (MathHelpers.pi * sqrt (u * (1.0 - u)) * (max - min))
        | Rayleigh(sigma) -> // 瑞利分布PDF / Rayleigh distribution PDF
            if x < 0.0 then
                0.0
            else
                (x / (sigma * sigma)) * exp (-x * x / (2.0 * sigma * sigma))
        | LogNormal(mu, sigma) -> // 对数正态分布PDF / Log-normal distribution PDF
            if x <= 0.0 then
                0.0
            else
                (1.0 / (x * sigma * sqrt (2.0 * MathHelpers.pi)))
                * exp (-0.5 * ((log (x) - mu) / sigma) ** 2.0)
        | InvSine(min, max) -> // 反正弦分布PDF / Inverse sine distribution PDF
            if x < min || x > max then
                0.0
            else
                let u = (x - min) / (max - min) in 1.0 / (MathHelpers.pi * sqrt (u * (1.0 - u)) * (max - min))
        | Sample(samples) -> // 样本分布PDF (使用核密度估计) / Sample distribution PDF (using kernel density estimation)
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

                let bandwidth = 1.06 * sampleStdev * (n ** (-1.0 / 5.0)) // Silverman经验法则 / Silverman's rule of thumb

                let kernel u =
                    (1.0 / sqrt (2.0 * MathHelpers.pi)) * exp (-0.5 * u * u) // 高斯核 / Gaussian kernel

                let density = Array.sum (Array.map (fun v -> kernel ((x - v) / bandwidth)) samples)
                density / (n * bandwidth)
        | Bootstrap(n, samples) -> // Bootstrap抽样PDF / Bootstrap sampling PDF
            let bootstrapSamples = BootstrapSamples(n, samples).Sample()

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

                let bandwidth = 1.06 * sampleStdev * (float bootstrapSamples.Length ** (-1.0 / 5.0)) // Silverman经验法则 / Silverman's rule of thumb

                let kernel u =
                    (1.0 / sqrt (2.0 * MathHelpers.pi)) * exp (-0.5 * u * u)

                let density =
                    Array.sum (Array.map (fun v -> kernel ((x - v) / bandwidth)) bootstrapSamples)

                density / (float bootstrapSamples.Length * bandwidth)
        | BootstrapStruct(bs) -> // Bootstrap样本结构PDF / Bootstrap samples structure PDF
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

                let bandwidth = 1.06 * sampleStdev * (n ** (-1.0 / 5.0)) // Silverman经验法则 / Silverman's rule of thumb

                let kernel u =
                    (1.0 / sqrt (2.0 * MathHelpers.pi)) * exp (-0.5 * u * u)

                let density = Array.sum (Array.map (fun v -> kernel ((x - v) / bandwidth)) sampled)
                density / (n * bandwidth)

    // 各分布的解析逆累积分布函数 / Analytical inverse CDF for each distribution
    let invCdf dist p =
        match dist with
        | Uniform(a, b) -> // 均匀分布逆CDF / Uniform distribution inverse CDF
            a + p * (b - a) // 线性插值 / Linear interpolation
        | Normal(mu, sigma) -> // 正态分布逆CDF / Normal distribution inverse CDF
            mu + sigma * MathHelpers.invStandardNormal (p)
        | Triangular(min, mode, max) -> // 三角分布逆CDF / Triangular distribution inverse CDF
            let fc = (mode - min) / (max - min)

            if p < fc then
                min + sqrt (p * (max - min) * (mode - min))
            else
                max - sqrt ((1.0 - p) * (max - min) * (max - mode))
        | Trapezoidal(a, b, c, d) -> // 梯形分布逆CDF / Trapezoidal distribution inverse CDF
            let h = 2.0 / (d + c - b - a) // Height of trapezoid
            let area1 = (b - a) * h / 2.0 // Area of rising triangle
            let area2 = (c - b) * h // Area of flat rectangle
            let p1 = area1
            let p2 = area1 + area2

            if p <= p1 then
                // Inverse of rising part
                a + sqrt (p * (b - a) * (d + c - b - a))
            elif p <= p2 then
                // Inverse of flat part
                (p * (d + c - b - a) + a + b) / 2.0
            else
                // Inverse of falling part
                d - sqrt ((1.0 - p) * (d - c) * (d + c - b - a))
        | TrapezoidalPlateau(a, b, plateau) -> // 平台梯形分布逆CDF / Plateau trapezoidal inverse CDF
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
                // Inverse of rising part
                a + sqrt (2.0 * p * slope_width / h)
            elif p <= p2 then
                // Inverse of flat part
                left_slope_end + (p - p1) / h
            else
                // Inverse of falling part
                b - sqrt (2.0 * (1.0 - p) * slope_width / h)
        | UShape(min, max) -> // U型分布逆CDF / U-shape distribution inverse CDF
            let u = sin (p * MathHelpers.pi / 2.0) ** 2.0
            min + u * (max - min)
        | Rayleigh(sigma) -> // 瑞利分布逆CDF / Rayleigh distribution inverse CDF
            sigma * sqrt (-2.0 * log (1.0 - p))
        | LogNormal(mu, sigma) -> // 对数正态分布逆CDF / Log-normal distribution inverse CDF
            exp (mu + sigma * MathHelpers.invStandardNormal (p))
        | InvSine(min, max) -> // 反正弦分布逆CDF / Inverse sine distribution inverse CDF
            let u = sin (p * MathHelpers.pi / 2.0) ** 2.0
            min + u * (max - min)
        | Sample(samples) -> // 样本分布逆CDF (分位数估计) / Sample distribution inverse CDF (quantile estimation)
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
        | Bootstrap(n, samples) -> // Bootstrap抽样逆CDF / Bootstrap sampling inverse CDF
            let bootstrapSamples = BootstrapSamples(n, samples).Sample()
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
        | BootstrapStruct(bs) -> // Bootstrap样本结构逆CDF / Bootstrap samples structure inverse CDF
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

    // 各分布的解析逆概率密度函数 (在有意义的情况下) / Analytical inverse PDF for each distribution (where meaningful)
    let invPdf dist p =
        match dist with
        | Uniform(mu, sigma) -> // 均匀分布逆PDF / Uniform distribution inverse PDF
            let pdfValue = 1.0 / (2.0 * sqrt (3.0) * sigma)

            if abs (p - pdfValue) < 1e-10 then
                mu // 在均值处唯一解 / Only one solution at the mean
            else
                failwith "No solution: uniform PDF is constant"
        | Normal(a, b) -> // 正态分布逆PDF / Normal distribution inverse PDF
            let mu = (a + b) / 2.0
            let sigma = (b - a) / 4.0
            let maxPdf = 1.0 / (sigma * sqrt (2.0 * MathHelpers.pi))

            if p > maxPdf then
                failwith "PDF value too high for normal distribution"
            elif abs (p - maxPdf) < 1e-10 then
                mu // 在峰值处 / At the peak
            else
                // 解方程: p = (1/(σ√(2π))) * exp(-0.5 * ((x-μ)/σ)²)
                // Solve: p = (1/(σ√(2π))) * exp(-0.5 * ((x-μ)/σ)²)
                let z = sqrt (-2.0 * log (p * sigma * sqrt (2.0 * MathHelpers.pi)))
                mu + sigma * z // 返回正解 / Return positive solution (could also return mu - sigma * z)
        | Triangular(min, mode, max) -> // 三角分布逆PDF / Triangular distribution inverse PDF
            let maxPdf = 2.0 / (max - min)

            if p > maxPdf then
                failwith "PDF value too high for triangular distribution"
            elif abs (p - maxPdf) < 1e-10 then
                mode // 在众数处 / At the mode
            else
                // 上升部分: p = 2(x-min)/((max-min)(mode-min))
                // For ascending part: p = 2(x-min)/((max-min)(mode-min))
                let x1 = min + p * (max - min) * (mode - min) / 2.0

                if x1 <= mode then
                    x1
                else
                    // 下降部分: p = 2(max-x)/((max-min)(max-mode))
                    // For descending part: p = 2(max-x)/((max-min)(max-mode))
                    max - p * (max - min) * (max - mode) / 2.0
        | Rayleigh(sigma) -> // 瑞利分布逆PDF / Rayleigh distribution inverse PDF
            if p <= 0.0 then
                failwith "PDF value must be positive for Rayleigh distribution"
            else
                // 解方程: p = (x/σ²) * exp(-x²/(2σ²))
                // Solve: p = (x/σ²) * exp(-x²/(2σ²))
                // 一般情况需要数值方法，使用牛顿-拉夫逊法 / This requires numerical methods for general case, using Newton-Raphson
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
            let bootstrapSamples = BootstrapSamples(n, samples).Sample()
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

                let bandwidth = 1.06 * sampleStdev * (n ** (-1.0 / 5.0)) // Silverman经验法则 / Silverman's rule of thumb

                let kernel u =
                    (1.0 / sqrt (2.0 * MathHelpers.pi)) * exp (-0.5 * u * u)

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


    // 计算给定置信水平p的扩展不确定度 / Returns the expanded uncertainty for a given level of confidence p
    let expandedUncertainty dist p =
        let centeredPercentile = (1.0 + p) / 2.0 // 中心覆盖区间 / For centered coverage interval
        let upperQuantile = invCdf dist centeredPercentile
        let distMean = mean dist
        // let distStdev = stdev dist
        abs (upperQuantile - distMean)

    // 计算覆盖因子kp / Calculate coverage factor kp
    let kp dist p =
        expandedUncertainty dist p / (stdev dist)

    // 计算分位数 / Calculate quantile
    let quantile dist p = invCdf dist (1.0 - p)

    // 计算双侧分位数 / Calculate two-sided quantile
    let quantileTwoSided dist p =
        let alpha = p / 2.0
        let lower = invCdf dist alpha
        let upper = invCdf dist (1.0 - alpha)
        (lower, upper)

    // 数值类型 - 支持精确值和分布的混合计算 / Value type - supporting mixed calculations of exact values and distributions
    type Value =
        | Exact of float // 精确数值 / Exact numerical value
        | Distribution of Distribution // 概率分布 / Probability distribution
        | Addition of Value * Value // 加法运算 / Addition operation
        | Bias of Value * float // 偏差修正 / Bias correction
        | Multiplication of Value * Value // 乘法运算 / Multiplication operation
        | Division of Value * Value // 除法运算 / Division operation
        | Power of Value * float // 幂运算 / Power operation
        | Min of Value * Value // 最小值运算 / Minimum operation
        | Max of Value * Value // 最大值运算 / Maximum operation
        | Abs of Value // 绝对值运算 / Absolute value operation
        | Sqrt of Value // 平方根运算 / Square root operation
        | Ln of Value // 自然对数运算 / Natural logarithm operation
        | Exp of Value // 指数运算 / Exponential operation
        | Sin of Value // 正弦运算 / Sine operation
        | Cos of Value // 余弦运算 / Cosine operation
        | Tan of Value // 正切运算 / Tangent operation
        | ASin of Value // 反正弦运算 / Arcsine operation
        | ACos of Value // 反余弦运算 / Arccosine operation
        | ATan of Value // 反正切运算 / Arctangent operation
        | ATan2 of Value * Value // 二参数反正切运算 / Two-parameter arctangent operation
        | Sinh of Value // 双曲正弦运算 / Hyperbolic sine operation
        | Cosh of Value // 双曲余弦运算 / Hyperbolic cosine operation
        | Tanh of Value // 双曲正切运算 / Hyperbolic tangent operation
        | Floor of Value // 向下取整运算 / Floor operation
        | Ceil of Value // 向上取整运算 / Ceiling operation
        | Round of Value // 四舍五入运算 / Rounding operation
        | Log10 of Value // 常用对数运算 / Common logarithm operation
        | Log2 of Value // 二进制对数运算 / Binary logarithm operation
        | Log of Value * float // 任意底对数运算 / Logarithm with arbitrary base
        | PowerOf10 of Value // 十的幂运算 / Power of 10 operation

    // 数值计算辅助函数 / Value calculation helper functions
    let add v1 v2 = Addition(v1, v2) // 加法 / Addition
    let multiply v1 v2 = Multiplication(v1, v2) // 乘法 / Multiplication
    let divide v1 v2 = Division(v1, v2) // 除法 / Division
    let power v p = Power(v, p) // 幂运算 / Power
    let minVal v1 v2 = Min(v1, v2) // 最小值 / Minimum
    let maxVal v1 v2 = Max(v1, v2) // 最大值 / Maximum
    let absVal v = Abs v // 绝对值 / Absolute value
    let sqrtVal v = Sqrt v // 平方根 / Square root
    let lnVal v = Ln v // 自然对数 / Natural logarithm
    let expVal v = Exp v // 指数 / Exponential
    let sinVal v = Sin v // 正弦 / Sine
    let cosVal v = Cos v // 余弦 / Cosine
    let tanVal v = Tan v // 正切 / Tangent
    let asinVal v = ASin v // 反正弦 / Arcsine
    let acosVal v = ACos v // 反余弦 / Arccosine
    let atanVal v = ATan v // 反正切 / Arctangent
    let atan2Val v1 v2 = ATan2(v1, v2) // 二参数反正切 / Two-parameter arctangent
    let sinhVal v = Sinh v // 双曲正弦 / Hyperbolic sine
    let coshVal v = Cosh v // 双曲余弦 / Hyperbolic cosine
    let tanhVal v = Tanh v // 双曲正切 / Hyperbolic tangent
    let floorVal v = Floor v // 向下取整 / Floor
    let ceilVal v = Ceil v // 向上取整 / Ceiling
    let roundVal v = Round v // 四舍五入 / Rounding
    let log10Val v = Log10 v // 常用对数 / Common logarithm
    let log2Val v = Log2 v // 二进制对数 / Binary logarithm
    let logVal v b = Log(v, b) // 任意底对数 / Logarithm with arbitrary base
    let powerOf10Val v = PowerOf10 v // 十的幂 / Power of 10

    // Value类型的运算符重载 / Operator overloads for Value type
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

    // 高级数学运算函数 / Advanced mathematical operation functions
    let sum values = List.reduce (+) values // 求和 / Summation
    let product values = List.reduce (*) values // 求积 / Product

    let average values =
        (sum values) / (Exact(float (List.length values))) // 平均值 / Average

    let meanValue values = average values // 均值 / Mean value
    let square v = v * v // 平方 / Square
    let cube v = v * v * v // 立方 / Cube
    let pow v p = Power(v, p) // 幂运算辅助函数 / Helper function for power operations

    // 随机抽样函数 / Random sampling function
    let rec sample (r: System.Random) value =
        match value with
        | Exact v -> v // 精确值直接返回 / Return exact value directly
        | Distribution dist -> // 从分布中抽样 / Sample from distribution
            match dist with
            | Uniform(mu, sigma) -> // 均匀分布抽样 / Uniform distribution sampling
                let a = mu - sqrt (3.0) * sigma
                let b = mu + sqrt (3.0) * sigma
                let u = r.NextDouble()
                a + u * (b - a)
            | Normal(a, b) -> // 正态分布抽样 / Normal distribution sampling
                let mu = (a + b) / 2.0
                let sigma = (b - a) / 4.0
                // Box-Muller变换 / Box-Muller transform
                let u1 = r.NextDouble()
                let u2 = r.NextDouble()
                mu + sigma * sqrt (-2.0 * log (u1)) * cos (2.0 * MathHelpers.pi * u2)
            | Triangular(min, mode, max) -> // 三角分布抽样 / Triangular distribution sampling
                let u = r.NextDouble()
                let fc = (mode - min) / (max - min)

                if u < fc then
                    min + sqrt (u * (max - min) * (mode - min))
                else
                    max - sqrt ((1.0 - u) * (max - min) * (max - mode))
            | Trapezoidal(a, b, c, d) -> // 梯形分布抽样 / Trapezoidal distribution sampling
                let u = r.NextDouble()
                let p1 = (b - a) / (2.0 * (d - a))
                let p2 = (c - a) / (d - a)
                let p3 = 1.0 - (d - c) / (2.0 * (d - a))

                if u <= p1 then a + sqrt (2.0 * u * (b - a) * (c - a))
                elif u <= p2 then a + u * (d - a)
                elif u <= p3 then a + u * (d - a)
                else d - sqrt (2.0 * (1.0 - u) * (d - c) * (d - b))
            | TrapezoidalPlateau(a, b, plateau) -> // 平台梯形分布抽样 / Plateau trapezoidal sampling
                let u = r.NextDouble()
                invCdf (TrapezoidalPlateau(a, b, plateau)) u // Use inverse CDF sampling
            | UShape(min, max) -> // U型分布抽样 / U-shape distribution sampling
                let u = r.NextDouble()
                let v = sin (u * MathHelpers.pi / 2.0) ** 2.0
                min + v * (max - min)
            | Rayleigh(sigma) -> // 瑞利分布抽样 / Rayleigh distribution sampling
                let u = r.NextDouble()
                sigma * sqrt (-2.0 * log (1.0 - u))
            | LogNormal(mu, sigma) -> // 对数正态分布抽样 / Log-normal distribution sampling
                let u1 = r.NextDouble()
                let u2 = r.NextDouble()
                let z = sqrt (-2.0 * log (u1)) * cos (2.0 * MathHelpers.pi * u2)
                exp (mu + sigma * z)
            | InvSine(min, max) -> // 反正弦分布抽样 / Inverse sine distribution sampling
                let u = r.NextDouble()
                let v = sin (u * MathHelpers.pi / 2.0) ** 2.0
                min + v * (max - min)
            | Sample(samples) -> // 样本分布抽样 / Sample distribution sampling
                if samples.Length = 0 then
                    failwith "Sample distribution has no samples"
                else
                    let index = r.Next(0, samples.Length)
                    samples.[index]
            | Bootstrap(n, samples) -> // Bootstrap抽样 / Bootstrap sampling
                if samples.Length = 0 then
                    failwith "Bootstrap distribution has no samples"
                else
                    let bootstrapSamples = BootstrapSamples(n, samples).Sample()

                    if bootstrapSamples.Length = 0 then
                        failwith "Bootstrap distribution has no samples"
                    else
                        let index = r.Next(0, bootstrapSamples.Length)
                        bootstrapSamples.[index]
            | BootstrapStruct(bs) -> // Bootstrap样本结构抽样 / Bootstrap samples structure sampling
                let sampled = bs.Sample()
                let index = r.Next(0, sampled.Length)
                sampled.[index]
        // 复合运算的抽样处理 / Sampling handling for composite operations
        | Addition(v1, v2) -> sample r v1 + sample r v2 // 加法抽样 / Addition sampling
        | Bias(v, b) -> sample r v + b // 偏差修正抽样 / Bias correction sampling
        | Multiplication(v1, v2) -> sample r v1 * sample r v2 // 乘法抽样 / Multiplication sampling
        | Division(v1, v2) -> sample r v1 / sample r v2 // 除法抽样 / Division sampling
        | Power(v, p) -> (sample r v) ** p // 幂运算抽样 / Power sampling
        | Min(v1, v2) -> min (sample r v1) (sample r v2) // 最小值抽样 / Minimum sampling
        | Max(v1, v2) -> max (sample r v1) (sample r v2) // 最大值抽样 / Maximum sampling
        | Abs v -> abs (sample r v) // 绝对值抽样 / Absolute value sampling
        | Sqrt v -> sqrt (sample r v) // 平方根抽样 / Square root sampling
        | Ln v -> log (sample r v) // 自然对数抽样 / Natural logarithm sampling
        | Exp v -> exp (sample r v) // 指数抽样 / Exponential sampling
        | Sin v -> sin (sample r v) // 正弦抽样 / Sine sampling
        | Cos v -> cos (sample r v) // 余弦抽样 / Cosine sampling
        | Tan v -> tan (sample r v) // 正切抽样 / Tangent sampling
        | ASin v -> asin (sample r v) // 反正弦抽样 / Arcsine sampling
        | ACos v -> acos (sample r v) // 反余弦抽样 / Arccosine sampling
        | ATan v -> atan (sample r v) // 反正切抽样 / Arctangent sampling
        | ATan2(v1, v2) -> atan2 (sample r v1) (sample r v2) // 二参数反正切抽样 / Two-parameter arctangent sampling
        | Sinh v -> sinh (sample r v) // 双曲正弦抽样 / Hyperbolic sine sampling
        | Cosh v -> cosh (sample r v) // 双曲余弦抽样 / Hyperbolic cosine sampling
        | Tanh v -> tanh (sample r v) // 双曲正切抽样 / Hyperbolic tangent sampling
        | Floor v -> floor (sample r v) // 向下取整抽样 / Floor sampling
        | Ceil v -> ceil (sample r v) // 向上取整抽样 / Ceiling sampling
        | Round v -> round (sample r v) // 四舍五入抽样 / Rounding sampling
        | Log10 v -> log10 (sample r v) // 常用对数抽样 / Common logarithm sampling
        | Log2 v -> log (sample r v) / log 2.0 // 二进制对数抽样 / Binary logarithm sampling
        | Log(v, b) -> log (sample r v) / log b // 任意底对数抽样 / Arbitrary base logarithm sampling
        | PowerOf10 v -> 10.0 ** (sample r v) // 十的幂抽样 / Power of 10 sampling

    // 蒙特卡洛求值函数 / Monte Carlo evaluation function
    let eval (value: Value) numSamples =
        let r = System.Random()
        let samples = Array.init numSamples (fun _ -> sample r value)
        Sample(samples)

    // Value类型扩展方法 / Value type extension method
    type Value with

        member this.Eval(numSamples: int) = eval this numSamples
