object Example { 
  def methodWithNoSmell = {
    println("this is a method with no smell")
    val x = 3 + 3
    println("the value of x is:" + x)
  }

  def methodWithRedundantReturn(n: Int): Int = { 
    n * 4
  }

  def methodWithNullUsage(x: Int) = {
    if(x % 3 == 0)
      x / 3
    else
      null
  }
  def methodWithExplicitReturn: Int = {
    return 5
  }
}