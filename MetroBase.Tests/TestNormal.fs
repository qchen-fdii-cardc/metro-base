module MetroBase.TestNormal

open Xunit
open metro_base.stat
open System

[<Fact>]
let ``Normal(0,1) facts`` () =
    let d = Normal(0.0, 1.0)
    Assert.Equal(0.0, mean d, 6)
    Assert.Equal(1.0, stdev d, 6)
    Assert.Equal(2.572466, kp d 0.99, 6)
    Assert.Equal(1.959049, kp d 0.95, 6)
    Assert.Equal(2.572466, expandedUncertainty d 0.99, 6)
    Assert.Equal(1.959049, expandedUncertainty d 0.95, 6)


[<Theory>]
[<InlineData(-3.0, 0.004432)>]
[<InlineData(-2.0, 0.053991)>]
[<InlineData(-1.0, 0.241971)>]
[<InlineData(0.0, 0.398942)>]
[<InlineData(1.0, 0.241971)>]
[<InlineData(2.0, 0.053991)>]
[<InlineData(3.0, 0.004432)>]
let ``Normal PDF x -> p`` x p =
    let d = Normal(0.0, 1.0)
    Assert.Equal(p, pdf d x, 6)

[<Theory>]
[<InlineData(-3.0, 0.0013499)>]
[<InlineData(-2.0, 0.0227501)>]
[<InlineData(-1.0, 0.158655)>]
[<InlineData(0.0, 0.5)>]
[<InlineData(1.0, 0.841345)>]
[<InlineData(2.0, 0.9772499)>]
[<InlineData(3.0, 0.9986501)>]
let ``Normal CDF x -> p`` x p =
    let d = Normal(0.0, 1.0)
    Assert.Equal(p, cdf d x, 6)


[<Theory>]
[<InlineData(0.00135, -3.0)>]
[<InlineData(0.0228, -2.0)>]
[<InlineData(0.1587, -1.0)>]
[<InlineData(0.5, 0.0)>]
[<InlineData(0.8413, 1.0)>]
[<InlineData(0.9772, 2.0)>]
[<InlineData(0.99865, 3.0)>]
let ``Normal InvCDF p -> x`` p x =
    let d = Normal(0.0, 1.0)
    Assert.Equal(x, invCdf d p, 1)

[<Fact>]
let ``Normal extreme parameters`` () =
    let d1 = Normal(1000.0, 0.0001)
    Assert.Equal(1000.0, mean d1, 6)
    Assert.Equal(0.0001, stdev d1, 6)
    Assert.True(pdf d1 1000.0 > 3000.0)
    let d2 = Normal(-1e10, 1e5)
    Assert.Equal(-1e10, mean d2, 6)
    Assert.Equal(1e5, stdev d2, 6)
    Assert.True(pdf d2 -1e10 > 3.9e-6)

[<Fact>]
let ``Normal invalid stdev throws`` () =
    Assert.Throws<ArgumentException>(fun () -> Normal(0.0, -1.0) |> ignore)
    |> ignore

    Assert.Throws<ArgumentException>(fun () -> Normal(0.0, 0.0) |> ignore) |> ignore

    Assert.Throws<ArgumentException>(fun () -> Normal(Double.NaN, 1.0) |> ignore)
    |> ignore

    Assert.Throws<ArgumentException>(fun () -> Normal(0.0, Double.NaN) |> ignore)
    |> ignore

    Assert.Throws<ArgumentException>(fun () -> Normal(0.0, Double.PositiveInfinity) |> ignore)
    |> ignore

    Assert.Throws<ArgumentException>(fun () -> Normal(0.0, Double.NegativeInfinity) |> ignore)
    |> ignore

[<Theory>]
[<InlineData(0.0, 1.0, 0.0, 1.0)>]
[<InlineData(2.0, 3.0, 2.0, 3.0)>]
[<InlineData(-5.0, 2.5, -5.0, 2.5)>]
let ``Normal mean and stdev`` mu sigma expectedMu expectedSigma =
    let d = Normal(mu, sigma)
    Assert.Equal(expectedMu, mean d, 10)
    Assert.Equal(expectedSigma, stdev d, 10)
    Assert.Equal(expectedSigma ** 2.0, metro_base.stat.variance d, 10)

[<Theory>]
[<InlineData(0.0, 1.0, 0.0, 0.3989422804014337)>]
[<InlineData(0.0, 1.0, 1.0, 0.24197072451914337)>]
[<InlineData(0.0, 1.0, -1.0, 0.24197072451914337)>]
[<InlineData(2.0, 3.0, 2.0, 0.1329807601338109)>]
[<InlineData(5.0, 3.0, 5.0, 0.1329807601338109)>]
let ``Normal PDF accuracy`` mu sigma x expected =
    let d = Normal(mu, sigma)
    Assert.Equal(expected, pdf d x, 10)

[<Theory>]
[<InlineData(0.0, 1.0, 0.0, 0.5)>]
[<InlineData(0.0, 1.0, 1.0, 0.8413447460685429)>]
[<InlineData(0.0, 1.0, -1.0, 0.15865525393145707)>]
[<InlineData(2.0, 3.0, 2.0, 0.5)>]
[<InlineData(2.0, 3.0, 5.0, 0.8413447460685429)>]
let ``Normal CDF accuracy`` mu sigma x expected =
    let d = Normal(mu, sigma)
    Assert.Equal(expected, cdf d x, 3)

[<Theory>]
[<InlineData(0.0, 1.0, 0.5, 0.0)>]
[<InlineData(0.0, 1.0, 0.8413447460685429, 1.0)>]
[<InlineData(0.0, 1.0, 0.15865525393145707, -1.0)>]
[<InlineData(2.0, 3.0, 0.5, 2.0)>]
[<InlineData(2.0, 3.0, 0.8413447460685429, 5.0)>]
let ``Normal Inverse CDF (Quantile) accuracy`` mu sigma p expected =
    let d = Normal(mu, sigma)
    Assert.Equal(expected, invCdf d p, 2)

[<Theory>]
[<InlineData(0.0, 1.0, 0.99)>]
[<InlineData(0.0, 1.0, 0.95)>]
[<InlineData(2.0, 1.0, 0.99)>]
let ``Normal expandedUncertainty matches kp`` mu sigma conf =
    let d = Normal(mu, sigma)
    Assert.Equal(kp d conf, expandedUncertainty d conf, 10)

[<Theory>]
[<InlineData(0.0, 1.0, 1.0)>]
[<InlineData(0.0, 1.0, -1.0)>]
[<InlineData(2.0, 3.0, 5.0)>]
[<InlineData(2.0, 3.0, -1.0)>]
let ``Normal PDF symmetry`` mu sigma x =
    let d = Normal(mu, sigma)
    let m = mean d
    let left = pdf d (m - abs x)
    let right = pdf d (m + abs x)
    Assert.Equal(left, right, 10)

[<Fact>]
let ``Normal PDF at mean is maximum`` () =
    let d = Normal(0.0, 1.0)
    let m = mean d
    let v = [ for x in -10 .. 10 -> pdf d (float x) ]
    let maxv = List.max v
    Assert.Equal(pdf d m, maxv, 10)

[<Fact>]
let ``Normal CDF at mean is 0.5`` () =
    let d = Normal(0.0, 1.0)
    Assert.Equal(0.5, cdf d (mean d), 6)

[<Fact>]
let ``Normal CDF at -infinity is 0, at +infinity is 1`` () =
    let d = Normal(0.0, 1.0)
    Assert.Equal(0.0, cdf d Double.NegativeInfinity, 10)
    Assert.Equal(1.0, cdf d Double.PositiveInfinity, 10)

[<Fact>]
let ``Normal PDF at infinity is 0`` () =
    let d = Normal(0.0, 1.0)
    Assert.Equal(0.0, pdf d Double.PositiveInfinity, 10)
    Assert.Equal(0.0, pdf d Double.NegativeInfinity, 10)

[<Fact>]
let ``Normal sample mean and stdev converge`` () =
    let d = Normal(2.0, 3.0)
    let samples = [ for _ in 1..100000 -> metro_base.stat.sample d ]
    let meanSample = samples |> List.average

    let stdevSample =
        let m = meanSample
        samples |> List.averageBy (fun x -> (x - m) ** 2.0) |> sqrt

    Assert.Equal(mean d, meanSample, 1)
    Assert.Equal(stdev d, stdevSample, 1)
