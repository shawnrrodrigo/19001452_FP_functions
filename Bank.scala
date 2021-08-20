object Bank extends App {

    var bank : List[Account] = List(
       new Account("Shawn" , 111 , -1500) ,
       new Account("Rodrigo" , 222 , 2500)
    )

    val find = ( n : Int , b : List[Account]) => b.filter( x => x.accNumber.equals( n ))

    val overdraft = ( b : List[Account] ) => b.filter( x => x.balance < 0)

    val balance = ( b: List[Account] ) => b.map( x => (x,x.balance) ).reduce( (a , c) => ( c._1 , a._2 + c._2) )

    val interest = ( b : List[Account] ) => b.map( x => {
        x.balance match {
            case a if a >= 0 => x deposit x.balance * 0.05
            case _ => x withdraw Math.abs(x.balance) * 0.01

        }
        x
    })


    println("bank " + bank )
    println()
    find( 222 , bank)
    
    println("overdraft: " + overdraft( bank ) )
    println()
    println("balance " + balance( bank )._2 )
    println()

    bank = interest( bank )

    println("bank " + bank )
    println()
    println("balance " + balance( bank )._2 )
    println()

}


class Account ( x:String , y: Int , z : Double){

    val name : String = x
    val accNumber : Int = y
    var balance : Double = z

    def withdraw( amount : Double ) = this.balance -= amount

    def deposit ( amount : Double ) = this.balance += amount

    def transfer( amount : Double, t : Account ): Unit = {
        this.balance =  this.balance - amount
        t.balance = t.balance + amount
    }


    @Override
    override def toString() : String = "Name : " + this.name + "\n Account Number : " + this.accNumber + "\n Balance : " + this.balance

}
