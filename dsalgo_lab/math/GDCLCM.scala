package dsalgo_lab.math

object GCDLCM extends App {

  def gcdEuler(n1: Int, n2: Int): Int = {
    if (n2 == 0) n1
    else gcdEuler(n2, n2 % n1)
  }
  println(gcdEuler(48, 18))
  
  //lcm = n1 * n2 / gcd
}
