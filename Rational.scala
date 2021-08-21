object RationalNumbers extends App{
  val x = new Rational(3, 4)
  val y = new Rational(5, 8)
  val z = new Rational(2, 7)

  val j = x.add(y)

  val k = x-y-z

  println(x)
  println(j)
  println(k)

}

class Rational(n:Int, d:Int){
  require(d>0, "d must be grater than 0")
  def numer = n/gcd(Math.abs(n),d)
  def denom = d/gcd(Math.abs(n),d)

  private def gcd(a:Int, b:Int):Int = if(b==0) a else gcd(b,a%b)
  def add(r:Rational)= new Rational(this.denom *r.numer + this.numer *r.denom, this.denom *r.denom )
  def -(r:Rational)= new Rational(this.denom *r.numer - this.numer*r.denom, this.denom *r.denom)
  def neg = new Rational(-this.numer, this.denom)
  override def toString = numer+"/"+denom
}
