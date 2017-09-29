

object myMain{

  class myClass(var elem:Int)
  {
    def addThat(that:myClass): Int =
    {
      return this.elem + that.elem
    }
  }

  val a = new myClass(3)
  val b = new myClass(10)
  print(a.addThat(b))
}

