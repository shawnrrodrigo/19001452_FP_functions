object Bank extends App{
  val acc1 = new Account(111,"980227815V",15500.00)
  val acc2 = new Account(222,"970820566V",25500.00)
  val acc3 = new Account(333,"882620137V",-5500.00)
  val acc4 = new Account(444,"899423119V",-10000.00)

  var bank:List[Account]=List(acc1,acc2,acc3,acc4)
  val overdraft = (b:List[Account]) => b.filter(x=>x.Bal.<(0.0))
  val sumBal = (b:List[Account]) => b.reduce((a,b) => new Account(0,"Total",a.Bal+b.Bal))
  val interest = (b:List[Account]) => b.map(x=> if (x.Bal<0) new Account(x.accNum,x.NIC,x.Bal*0.1+x.Bal) else new Account(x.accNum,x.NIC,x.Bal*0.05+x.Bal))
  print("List of Accounts with negative balances=: ")
  overdraft(bank).foreach{
    println()
    x => print(x.accNum)
    println()
  }
  println()
  println()
  println(s"TSum of a;; account balances: "+sumBal(bank).Bal)
  println()
  println("Final balances of all accounts: ")
  interest(bank).foreach{
    x => print(x.accNum)
    print(" = ")
    print(x.Bal)
    println()
  }
}

class Account(n:Int, id:String, b:Double){
  val accNum: Int = n
  val NIC:String = id
  var Bal: Double = b

  override  def toString = s"[$accNum:$NIC:$Bal]"
}
