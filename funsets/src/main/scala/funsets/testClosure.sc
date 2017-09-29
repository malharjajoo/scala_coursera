object Main{

  type Set = Int=>Boolean
  def singletonset(x:Int):Set = {

    def fun_name(elem:Int):Boolean={
      elem == x
    }

    fun_name
  }

  singletonset(8)(-2)
}