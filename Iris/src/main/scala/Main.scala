import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import scala.collection.mutable
import scala.util.Random

object Iris {
  def euclideanDistance(a: Array[Double], b: Array[Double]): Double = {
    math.sqrt(a.zip(b).map { case (x, y) => math.pow(x - y, 2) }.sum)
  }

  def averageCentroid(cluster: Seq[Array[Double]]): Array[Double] = {
    val sum = cluster.reduce((a, b) => a.zip(b).map { case (x, y) => x + y })
    sum.map(_ / cluster.size)
  }

  def kMeans(data: Array[Array[Double]], k: Int, maxIterations: Int = 100): Array[Array[Double]] = {
    val random = new Random
    var centroids = (0 until k).map(_ => data(random.nextInt(data.length))).toArray

    for (_ <- 0 until maxIterations) {
      val clusters = mutable.Map[Int, mutable.Buffer[Array[Double]]]()

      data.foreach { point =>
        val closest = centroids.map(centroid => euclideanDistance(point, centroid)).zipWithIndex.minBy(_._1)._2
        clusters.getOrElseUpdate(closest, mutable.Buffer[Array[Double]]()) += point
      }

      centroids = clusters.values.map(cluster => averageCentroid(cluster.toSeq)).toArray
    }

    centroids
  }

  def readCsv(filePath: String, processLine: Array[String] => Unit): Unit = {
    val bufferedSource = Source.fromFile(filePath)
    try {
      bufferedSource.getLines().drop(1).foreach(line => {
        val cols = line.split(",").map(_.trim)
        processLine(cols)
      })
    } finally {
      bufferedSource.close()
    }
  }

  def pearsonCorrelation(data: Array[Array[Double]], col1: Int, col2: Int): Double = {
    val pairedData = data.map(row => (row(col1), row(col2))).filter { case (x, y) => !x.isNaN && !y.isNaN }
    val meanCol1 = pairedData.map(_._1).sum / pairedData.length
    val meanCol2 = pairedData.map(_._2).sum / pairedData.length

    val numerator = pairedData.map { case (x, y) => (x - meanCol1) * (y - meanCol2) }.sum
    val denominator = math.sqrt(pairedData.map { case (x, _) => math.pow(x - meanCol1, 2) }.sum) *
                      math.sqrt(pairedData.map { case (_, y) => math.pow(y - meanCol2, 2) }.sum)

    if (denominator == 0) 0.0 else numerator / denominator
  }

 
  def predict(dataPoint: Array[Double], centroids: Array[Array[Double]]): Int = {
  centroids.map(centroid => euclideanDistance(dataPoint, centroid))
    .zipWithIndex.minBy(_._1)._2
}

}