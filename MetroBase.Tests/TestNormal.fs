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
[<InlineData(0.0, 10.0, 0.0)>]
[<InlineData(0.0, 10.0, 10.0)>]
[<InlineData(-2.0, 2.0, 0.0)>]
let ``cdf of Normal is within [0,1]`` (mu: float) (sigma: float) (x: float) =
    let d = Normal(mu, sigma)
    let p = cdf d x
    Assert.InRange(p, 0.0, 1.0)
