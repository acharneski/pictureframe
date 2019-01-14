package com.simiacryptus

import java.awt.Desktop
import java.awt.image.BufferedImage
import java.io.File
import java.nio.charset.Charset

import javax.imageio.ImageIO
import org.apache.commons.io.FileUtils

case class TilingJob
(
  path: String,
  topPos: Double = -0.9,
  leftPos: Double = 0,
  delta: (Double,Double) = (0.0,0.0),
  height: Double = 2.7,
  width: Double = 1.01,
  aspectCorrection : Boolean = true,
  dedupImages : Boolean = false,
  pixelFn : (Int,Int,Int)=>(Int,Int,Int) = (_,_,_),
  imageFormat: String = "jpg",
  baseDir: File
) {
  def move(x:Double,y:Double) : TilingJob = copy(delta = (delta._1+x,delta._2+y))
  def printHtml(images: Seq[TileInfo], assemble: Boolean, htmlFilename: String, id: String): Unit = {
    val imageHtml = for (image <- images.zipWithIndex) yield {
      val pos = if (this.dedupImages) image._1.viewPos else image._1.layoutPos
      val imageName = List("tile", id, pos._1, pos._2).mkString("_") + "." + imageFormat
      ImageIO.write(image._1.image, imageFormat, new File(baseDir, imageName))
      if (assemble)
        s"""<img src="$imageName" style="position: absolute;left: ${image._1.layoutPos._1}px;top: ${image._1.layoutPos._2}px;"/><br/>"""
      else
        s"""<img src="$imageName" /><br/>"""
    }
    val html =
      s"""<html>
         |<head></head>
         |<body>${imageHtml.mkString("\n")}</body>
         |</html>""".stripMargin
    val htmlFile = new File(baseDir, htmlFilename)
    FileUtils.write(htmlFile, html, Charset.forName("UTF-8"))
    Desktop.getDesktop.browse(htmlFile.toURI)
  }

  def tiles() = {
    val rootImage = ImageIO.read(new File(this.path))
    val cols = (1 until 10).takeWhile(i => (rootImage.getWidth / (1.5 * i)) > 1200).reverse.head
    val tileWidth = (rootImage.getWidth / (cols * 1.5)).toInt
    val tileHeight = (Math.cos(Math.PI / 6) * tileWidth).toInt
    val targetImageHeight = rootImage.getWidth * tileHeight / tileWidth
    val aspectCorrection = if (this.aspectCorrection) (targetImageHeight.doubleValue() / rootImage.getHeight) / 1.5 else 1// WTF?
    val tiles = (-100 until 100).flatMap(i => (-100 until 100).map(j => (i, j)))
      .map(t => (
        (t._1 * tileWidth * 1.5 + (if (0 == (t._2 % 2)) 0 else (tileWidth * 0.5 * (1 + Math.sin(Math.PI / 6))))).toInt,
        (tileHeight * t._2 / 2).toInt,
        t._1,
        t._2
      ))
      .filter(_._1 >= this.leftPos * rootImage.getWidth).filter(_._1 < (this.leftPos + this.width) * rootImage.getWidth)
      .filter(_._2 >= this.topPos * rootImage.getHeight * aspectCorrection).filter(_._2 < (this.topPos + this.height) * rootImage.getHeight * aspectCorrection)

    def toViewXy(tile:(Int,Int),x:Int,y:Int) = {
      var viewX = (tile._1 + x) % rootImage.getWidth
      while (viewX < 0) viewX = viewX + rootImage.getWidth
      var viewY = ((tile._2 + y) / aspectCorrection).toInt % rootImage.getHeight
      while (viewY < 0) viewY = viewY + rootImage.getHeight
      (viewX, viewY)
    }

    (for (tile <- tiles) yield {
      val tileImage = new BufferedImage(tileWidth, tileHeight, BufferedImage.TYPE_INT_ARGB)
      for (y <- (0 until tileImage.getHeight)) {
        val distFromCenter = if (y >= (tileImage.getHeight / 2)) {
          tileImage.getHeight - y
        } else {
          y
        }
        val rowWidth = ((distFromCenter.toDouble / tileImage.getHeight) + .5) * tileImage.getWidth
        val empty = ((tileImage.getWidth - rowWidth) / 2).toInt
        if (empty >= 0) for (x <- (empty until (tileImage.getWidth - empty))) {

          var (viewX: Int, viewY: Int) = toViewXy((tile._1+ (this.delta._1*rootImage.getWidth()).toInt,tile._2+ (this.delta._2*rootImage.getHeight()).toInt),x,y)
          val sourcePixel = rootImage.getRGB(viewX, viewY)
          val alpha = (sourcePixel >> 24) & 0xff
          val red = (sourcePixel >> 16) & 0xff
          val green = (sourcePixel >> 8) & 0xff
          val blue = sourcePixel & 0xff
          val resultPixel = this.pixelFn(red,blue,green)
          tileImage.setRGB(x, y, (resultPixel._1 << 16) | (resultPixel._2) | (resultPixel._3 << 8) | (alpha << 24))
        }
      }
      TileInfo(tileImage,(tile._1,tile._2),toViewXy((tile._1,tile._2),0,0),(tile._3,tile._4))
    })
  }

}
