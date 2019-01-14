package com.simiacryptus

import java.awt.image.BufferedImage

case class TileInfo
(
  image : BufferedImage,
  layoutPos : (Int,Int),
  viewPos : (Int,Int),
  tileId : (Int,Int)
)
