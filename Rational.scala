object Rational extends App{
  val w = new Rational(2)
  val x = new Rational(3, 4)
  val y = new Rational(5, 8)
  val z = new Rational(2, 7)
  val p = x+y
  val q = x-y-z

  println(w)
  println(x)
  println(p.neg)
  println(q)
}

class Rational(n:Int, d:Int){
  require(d>0 , "denominator should always be positive")

  def numer = n/gcd(n,d)
  def denom = n/gcd(n,d)
  def this(n:Int) = this(n, 1)

  private def gcd(a:Int, b:Int): Int = if(b==0) a else if(b>a) gcd(b, a) else gcd(b, a%b)

  def neg = new Rational(-this.numer, this.denom)
  def +(r:Rational) = new Rational(this.numer *r.denom + r.numer *this.denom, this.denom *r.denom)
  def -(r:Rational) = this + r.neg
  def *(r:Rational) = new Rational(this.numer * r.numer, this.denom * r.denom)
  def /(r:Rational) = new Rational(this.numer * r.denom, this.denom * r.numer)

  override def toString= numer+"/"+denom
}
