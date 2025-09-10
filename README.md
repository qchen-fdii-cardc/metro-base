# Metro-Base: F# 统计计量学库

![F#](https://img.shields.io/badge/language-F%23-178600)
![.NET](https://img.shields.io/badge/.NET-8.0-512bd4)
![License](https://img.shields.io/badge/license-MIT-green)

## 概述

[Nuget Package Page](https://www.nuget.org/packages/metro-base.fsharp.qc/)

Metro-Base 是一个专为统计计量学应用设计的综合性 F# 库，提供先进的概率分布建模、不确定度量化和蒙特卡洛仿真功能。该库为测量科学和工程中常用的各种概率分布实现了解析和数值方法。

### 代码优化特性

模块化设计：通过 `MathHelpers` 模块提取公共数学函数，避免代码重复。

统一数学常数：使用统一的 π 常数定义，提高代码一致性。

优化的逆误差函数：单一实现的高精度逆误差函数，支持所有需要的分布。

简化的 Bootstrap 处理：统一的 Bootstrap 采样逻辑，减少代码冗余。

全面的中文注释：每个函数和重要代码段都提供中英文对照注释。

改进的错误处理：更清晰的错误消息和边界条件处理。

## 主要特性

支持 10 种概率分布：正态、均匀、三角、传统梯形、平台梯形、U 形、瑞利、对数正态、反正弦和经验（基于样本）分布。

解析逆函数：提供逆累积分布函数（CDF）和概率密度函数（PDF）计算的快速闭式解。

自举采样：用于经验分布分析的先进重采样方法。

运算符重载：使用 `+`、`-`、`*`、`/` 运算符的自然数学语法。

覆盖因子：自动计算计量学覆盖因子（k 因子）。

蒙特卡洛仿真：高效的不确定度采样和传播。

扩展不确定度：符合 ISO GUM 的不确定度计算。

## 数学基础

### 误差函数实现

该库实现了 Abramowitz & Stegun 的误差函数有理逼近：

```fsharp
let erf z =
    let t = 1.0 / (1.0 + 0.5 * abs z)
    let tau = t * exp(-z * z - 1.26551223 + t * (1.00002368 + t * (0.37409196 + t * (0.09678418 + t * (-0.18628806 + t * (0.27886807 + t * (-1.13520398 + t * (1.48851587 + t * (-0.82215223 + t * 0.17087277)))))))))
    if z >= 0.0 then 1.0 - tau else tau - 1.0
```

数学定义：误差函数定义为：
$$\text{erf}(z) = \frac{2}{\sqrt{\pi}} \int_0^z e^{-t^2} dt$$

参考文献：Abramowitz, M. and Stegun, I. A. "Error Function and Fresnel Integrals." Ch. 7 in Handbook of Mathematical
Functions with Formulas, Graphs, and Mathematical Tables, 9th printing. New York: Dover, pp. 297-309, 1972.

在线参考：[MathWorld - Error Function](https://mathworld.wolfram.com/Erf.html)

## 支持的分布

### 1. 正态分布

参数：`Normal(μ, σ)` 其中 μ 是均值，σ 是标准差。

均值： μ

标准差： σ

概率密度函数：
f(x) = 1⁄(σ√(2π)) · exp(−½((x−μ)/σ)²)

累积分布函数：
F(x) = ½·(1 + erf((x−μ)/(σ√2)))

应用：测量不确定度、校准误差、自然现象。

### 2. 均匀分布

参数：`Uniform(a, b)` 其中 a 是下界，b 是上界。

均值： μ = (a + b)⁄2

标准差： σ = (b − a)⁄(2√3)

概率密度函数：
f(x) = 1⁄(b−a)，x ∈ [a, b]，否则为 0

累积分布函数：
F(x) = (x − a)⁄(b − a)，x ∈ [a, b]

应用：数字舍入误差、量化噪声、矩形不确定度。

### 3. 三角分布

参数：`Triangular(min, mode, max)`。

均值： μ = (min + mode + max)⁄3

方差： σ² = (min² + mode² + max² − min·mode − min·max − mode·max)⁄18

概率密度函数：
f(x) =
 2(x−min)⁄((max−min)(mode−min))，min ≤ x ≤ mode
 2(max−x)⁄((max−min)(max−mode))，mode < x ≤ max

参考文献：Evans, M.; Hastings, N.; and Peacock, B. "Triangular Distribution." Ch. 40 in Statistical Distributions, 3rd
ed. New York: Wiley, pp. 187-188, 2000.

在线参考：[MathWorld - Triangular Distribution](https://mathworld.wolfram.com/TriangularDistribution.html)

### 4. 梯形分布

该库提供两种梯形分布的实现形式。

#### 4.1 传统梯形分布

参数：`Trapezoidal(a, b, c, d)` 其中 a ≤ b ≤ c ≤ d。

a 表示分布的最小值（左端点）。
b 表示平坦区域的左端点。
c 表示平坦区域的右端点。
d 表示分布的最大值（右端点）。

均值： μ = (a + b + c + d)⁄4

方差： σ² = (1⁄18)·[(d−a)² + (c−b)² + (d−a)(c−b)]

概率密度函数：
f(x) =
 2(x−a)⁄((d−a+c−b)(b−a))，a ≤ x ≤ b
 2⁄(d−a+c−b)，b ≤ x ≤ c
 2(d−x)⁄((d−a+c−b)(d−c))，c ≤ x ≤ d

#### 4.2 平台梯形分布

参数：`TrapezoidalPlateau(a, b, plateau)`。

a 表示分布的最小值。
b 表示分布的最大值。
plateau 表示中央平坦区域的长度（非位置）。

参数关系说明如下。
总宽度： w = b − a
边坡宽度： wₛₗₒₚₑ = (w − plateau)⁄2
上升区间： [a, a + wₛₗₒₚₑ]
平坦区间： [a + wₛₗₒₚₑ, b − wₛₗₒₚₑ]
下降区间： [b − wₛₗₒₚₑ, b]

均值： μ = (a + b)⁄2（对称分布）
标准差： σ = √((b−a)² − plateau²)⁄12

概率密度函数：
f(x) =
 2(x−a)⁄(wₛₗₒₚₑ·w)，上升区间
 2⁄w，平坦区间
 2(b−x)⁄(wₛₗₒₚₑ·w)，下降区间

应用包括工程公差分析、具有已知平坦区域长度的测量范围、质量控制中的规格限制、计量学中的矩形和三角不确定度的组合。

### 5. U 形分布（反正弦分布）

参数：`UShape(min, max)`。

均值： μ = (min + max)⁄2
标准差： σ = (max − min)⁄(2√2)

概率密度函数：
f(x) = 1⁄(π√(u(1−u)(max−min)))，其中 u = (x−min)⁄(max−min)

累积分布函数：
F(x) = 2⁄π · arcsin(√u)

应用：振荡现象、极值场景。

### 6. 瑞利分布

参数：`Rayleigh(σ)` 其中 σ 是尺度参数。

均值： μ = σ√(π⁄2)
标准差： σₛₜd = σ√(2 − π⁄2)

概率密度函数：
f(x) = x⁄σ² · exp(−x²⁄(2σ²))，x ≥ 0

累积分布函数：
F(x) = 1 − exp(−x²⁄(2σ²))

应用：风速建模、波高分析、可靠性工程。

参考文献：Papoulis, A. Probability, Random Variables, and Stochastic Processes, 2nd ed. New York: McGraw-Hill, pp. 104
and 148, 1984。

在线参考：[MathWorld - Rayleigh Distribution](https://mathworld.wolfram.com/RayleighDistribution.html)

### 7. 对数正态分布

参数：`LogNormal(μ, σ)` 其中 μ 和 σ 是对数尺度参数。

均值： E[X] = exp(μ + σ²⁄2)
方差： Var[X] = (exp(σ²) − 1) · exp(2μ + σ²)

概率密度函数：
f(x) = 1⁄(xσ√(2π)) · exp(−½((ln(x)−μ)/σ)²)，x > 0

应用：金融建模、粒度分布、环境数据。

### 8. 反正弦分布

参数：`InvSine(min, max)`，等价于 U 形分布。

应用：相位分布、角度测量。

### 9. 自举分布

参数：`Bootstrap(n, samples)`，其中 n 是自举样本数。

实现：使用蓄水池采样进行高效的自举重采样。

```fsharp
type BootstrapSamples = struct
    val NumSamples: int
    val Samples: float array
    val SampledArray: float array
    // 高效的自举采样实现
end
```

## 覆盖因子（k 因子）

该库使用中心百分位数为计量学应用计算覆盖因子：

```fsharp
let kp dist p =
    expandedUncertainty dist p / (stdev dist)

let expandedUncertainty dist p =
    let centeredPercentile = (1.0 + p) / 2.0
    let upperQuantile = invCdf dist centeredPercentile
    let distMean = mean dist
    abs(upperQuantile - distMean)
```

典型覆盖因子（95% 置信度）：正态分布 k ≈ 1.96。均匀分布 k ≈ 1.65。U 形分布 k ≈ 1.41。瑞利分布 k ≈ 2.23。

## 解析逆函数

该库实现了逆累积分布函数计算的闭式解析解，与数值方法相比提供了优越的性能。

### 正态分布逆累积分布函数

使用逆误差函数的有理逼近：

```fsharp
let invErf z =
    let a = 0.147
    let ln1minusZ2 = log(1.0 - z * z)
    let term1 = 2.0 / (System.Math.PI * a) + ln1minusZ2 / 2.0
    let term2 = ln1minusZ2 / a
    let sign = if z >= 0.0 then 1.0 else -1.0
    sign * sqrt(sqrt(term1 * term1 - term2) - term1)
```

### 瑞利分布逆累积分布函数

直接解析解：

```fsharp
sigma * sqrt(-2.0 * log(1.0 - p))
```

### 三角分布逆累积分布函数

基于众数的分段解析解：

```fsharp
let fc = (mode - min) / (max - min)
if p < fc then
    min + sqrt(p * (max - min) * (mode - min))
else
    max - sqrt((1.0 - p) * (max - min) * (max - mode))
```

## 运算符重载

该库通过运算符重载提供自然的数学语法：

```fsharp
type Value with
    static member (+) (v1: Value, v2: Value) = Addition(v1, v2)
    static member (-) (v1: Value, v2: Value) = Addition(v1, Multiplication(Exact(-1.0), v2))
    static member (*) (v1: Value, v2: Value) = Multiplication(v1, v2)
    static member (/) (v1: Value, v2: Value) = Division(v1, v2)
    static member (~-) (v: Value) = Multiplication(Exact(-1.0), v)
```

使用示例：

```fsharp
let v1 = Distribution(Normal(10.0, 2.0))
let v2 = Distribution(Uniform(5.0, 1.0))
let result = v1 + v2 * 2.0 - v1 / v2  // 自然数学语法
```

## 安装和使用

### 先决条件

需要 .NET 8.0 或更高版本以及 F# 编译器。

### 项目结构

```
metro-base/
├── metro-base.fsproj       # 库项目
├── metro-console.fsproj    # 控制台应用程序
├── metro_base.fs           # 核心库实现
├── Program.fs              # 使用示例和测试
└── README.md               # 本文档
```

### 构建库

```bash
dotnet build metro-base.fsproj
```

### 运行控制台应用程序

```bash
dotnet run --project metro-console.fsproj
```

### 基本使用示例

```fsharp
open metro_base.metro

// 创建分布
let normalDist = Normal(0.0, 10.0)
let uniformDist = Uniform(5.0, 2.0)
let trapDist = Trapezoidal(2.0, 3.0, 5.0, 8.0)          // 传统 4 参数梯形
let trapPlateau = TrapezoidalPlateau(2.0, 8.0, 3.0)     // 平台梯形，高原长度 = 3

// 计算统计属性
let meanVal = mean normalDist        // 5.0
let stdevVal = stdev normalDist      // 2.5
let trapMean = mean trapPlateau      // 5.0 (对称分布)

// 计算覆盖因子
let k95 = kp normalDist 0.95         // ~1.96

// 使用运算符的蒙特卡洛仿真
let v1 = Distribution(normalDist)
let v2 = Distribution(uniformDist)
let combined = v1 + v2 * 2.0

let result = eval combined 100000
let resultMean = mean result
let resultStdev = stdev result
```

## 高级特性

### 蒙特卡洛仿真

使用 Box-Muller 变换进行正态分布的高效采样，其他分布使用解析逆变换：

```fsharp
let rec sample (r: System.Random) value =
    match value with
    | Normal(a, b) ->
        let mu = (a + b) / 2.0
        let sigma = (b - a) / 4.0
        // Box-Muller 变换
        let u1 = r.NextDouble()
        let u2 = r.NextDouble()
        mu + sigma * sqrt(-2.0 * log(u1)) * cos(2.0 * System.Math.PI * u2)
```

### 核密度估计

对于经验分布，该库使用 Silverman 拇指法则进行带宽选择：

```fsharp
let bandwidth = 1.06 * sampleStdev * (n ** (-1.0 / 5.0))
let kernel u = (1.0 / sqrt(2.0 * System.Math.PI)) * exp(-0.5 * u * u)
```

## 参考文献和延伸阅读

主要数学参考文献：Abramowitz, M. and Stegun, I. A. Handbook of Mathematical Functions with Formulas, Graphs, and
Mathematical Tables, 9th printing. New York: Dover, 1972。

主要数学参考文献：Evans, M.; Hastings, N.; and Peacock, B. Statistical Distributions, 3rd ed. New York: Wiley, 2000。

主要数学参考文献：Papoulis, A. Probability, Random Variables, and Stochastic Processes, 2nd ed. New York: McGraw-Hill,
1984。

在线数学资源：[MathWorld - Error Function](https://mathworld.wolfram.com/Erf.html)。

在线数学资源：[MathWorld - Triangular Distribution](https://mathworld.wolfram.com/TriangularDistribution.html)。

在线数学资源：[MathWorld - Rayleigh Distribution](https://mathworld.wolfram.com/RayleighDistribution.html)。

在线数学资源：[MathWorld - Statistical Distributions](https://mathworld.wolfram.com/topics/StatisticalDistributions.html)。

计量学标准：ISO/IEC Guide 98-3:2008 (GUM) - 测量不确定度表示指南。

计量学标准：NIST Technical Note 1297 - NIST 测量结果不确定度评估和表达指南。

## 性能特性

该库优先使用解析解而非数值方法以获得最佳性能。

解析逆累积分布函数具备 O(1) 复杂度（相较于二分搜索的 O(log n)）。

直接采样采用变换方法而非拒绝采样。

内存高效，函数式编程方法，最小状态。

## 许可证

本项目采用 MIT 许可证，详见 LICENSE 文件。

## 贡献

欢迎贡献。请确保所有新分布包括：完整的数学文档；尽可能的解析逆函数；具有已知参考值的单元测试；性能基准。

## 参考和验证

数学公式经 MathWorld (Wolfram Research) 验证。

误差函数实现基于 Abramowitz & Stegun。

自举方法遵循 Efron & Tibshirani。

计量学惯例遵循 ISO GUM 指南。
