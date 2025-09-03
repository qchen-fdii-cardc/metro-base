module MetroBase.TestNormal

open Xunit
open metro_base.metro

[<Fact>]
let ``mean of Normal(0,10) equals midpoint`` () =
    let d = Normal(0.0, 10.0)
    Assert.Equal(5.0, mean d, 6)

[<Theory>]
[<InlineData(0.0, 10.0, 0.0)>]
[<InlineData(0.0, 10.0, 10.0)>]
[<InlineData(-2.0, 2.0, 0.0)>]
let ``cdf of Normal is within [0,1]`` (a: float) (b: float) (x: float) =
    let d = Normal(a, b)
    let p = cdf d x
    Assert.InRange(p, 0.0, 1.0)

[<Theory>]
[<InlineData(0.0, 10.0, -1.0)>]
[<InlineData(0.0, 10.0, 11.0)>]
[<InlineData(-2.0, 2.0, -2.0)>]
let ``pdf of Normal is non-negative`` (a: float) (b: float) (x: float) =
    let d = Normal(a, b)
    let p = pdf d x
    Assert.True(p >= 0.0)