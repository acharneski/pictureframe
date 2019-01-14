package com.simiacryptus

import java.awt.Desktop
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

object ImagePrint {

  def main(args: Array[String]): Unit = {
    val reportDir = new File(new SimpleDateFormat("yyyy_MM_dd_HH_mm").format(new Date()))
    reportDir.mkdirs()
    val jobs = List(
      TilingJob(args.head, baseDir = reportDir)
    )
    for ((job, idx) <- jobs.zipWithIndex) {
      job.printHtml(job.tiles(), assemble = true, htmlFilename = s"index$idx.html", id = idx.toString)
    }
    Desktop.getDesktop.open(reportDir)
    System.exit(0)
  }

}
