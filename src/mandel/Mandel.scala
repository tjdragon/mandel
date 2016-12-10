package mandel

import scala.annotation.tailrec

object Mandel {
  type Pixel = (Int, Int)
  val maxIter = 255
  val size = 1024
  val palette = {
    val rnd = new java.security.SecureRandom
    ((for(n <- 0 until maxIter) yield rnd.nextInt()).toList) ++ List(0)
  }
  
  def mandel():Map[Pixel, Int] = {
    @tailrec
    def compute(p: Pixel, x: Double = 0, y: Double = 0, iter: Int = 0): Int = {
      def x2mx(lx: Int, w: Int) = -2.5 + (lx * 3.5) / w
      def y2my(ly: Int, h: Int) = 1.0 - ly * 2.0 / h
      val xmy = (x2mx(p._1, size), y2my(p._2, size))
    
      if (x*x+y*y > 4 || iter >= maxIter) 
        iter
      else
        compute(p, x*x-y*y+xmy._1, 2*x*y+xmy._2, iter+1)
    }
    
    val area = (for(x <- 0 until size; y <- 0 until size) yield (x, y))
    area.par.map(e => (e, compute(e))).toList.toMap
  }
  
  def persistToFile(pixels: Map[Pixel, Int]) {
    import java.awt.image.{BufferedImage => BI}
    val im = new BI(size, size, BI.TYPE_INT_RGB)
    pixels.par.foreach(e => {im.setRGB(e._1._1, e._1._2, palette(e._2))})
    javax.imageio.ImageIO.write(im, "jpg", new java.io.File("mandel.jpg"))
  }
  
  def main(args: Array[String]):Unit = {
    val t0 = System.currentTimeMillis()
    persistToFile(mandel())
    val tf = (System.currentTimeMillis() - t0)
    println(s"Done in $tf millis")
  }
}