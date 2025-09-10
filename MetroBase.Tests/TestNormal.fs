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
    

