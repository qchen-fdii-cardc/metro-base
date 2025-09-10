module MetroBase.Tests

open Xunit

[<Fact>]
let ``smoke test`` () =
    Assert.Equal(0.0, 0.0, 6)

[<Theory>]
[<InlineData(0.0, 1.0, -1.0)>]
[<InlineData(0.0, 2.0, -2.0)>]
[<InlineData(2.0, 0.0, 2.0)>]
let ``smoke test of subtraction`` (a: float) (b: float) (expected: float) =
    Assert.Equal(expected, a - b, 6)
