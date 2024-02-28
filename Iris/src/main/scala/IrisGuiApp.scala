import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import javax.swing._
import java.awt.event.ActionEvent
import scala.collection.mutable
import java.awt.Image
import javax.imageio.ImageIO
import java.io.File
object IrisUI extends App {

  val frame = new JFrame("Iris Classifier & Statistics Par MAATALLA FAWZIA")
  frame.setSize(700, 600) 
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
  frame.setLayout(null)
  val labels =
    Array("Sepal Length", "Sepal Width", "Petal Length", "Petal Width")
  val labelComponents = new Array[JLabel](4)
  val chooseFileButton = new JButton("Choose CSV File")
  val calculateButton = new JButton("Calculate Stats & Cluster")
  val predictButton = new JButton("Predict Cluster")
  val resultArea = new JTextArea()
  val inputFields = Array.fill(4)(new JTextField())
  val fileChooser = new JFileChooser()
  val scrollPane = new JScrollPane(resultArea)

  chooseFileButton.setBounds(10, 10, 200, 30)
  calculateButton.setBounds(10, 50, 200, 30)
  predictButton.setBounds(10, 90, 200, 30)
  scrollPane.setBounds(
    10,
    350,
    670,
    200
  ) 

  inputFields.zipWithIndex.foreach { case (field, index) =>
   
    field.setBounds(120, 130 + index * 30, 90, 25)
    frame.add(field)

    
    labelComponents(index) = new JLabel(labels(index))
    labelComponents(index).setBounds(10, 130 + index * 30, 100, 25) 
    frame.add(labelComponents(index))
  }

  try {
    //val imageFile = new File("c:\\iris.jpg") // ajoueter cete image au chemain
    
    val imageStream = getClass.getResourceAsStream("/images/iris.jpg")
    val image = ImageIO.read(imageStream)
    val scaledImage = image.getScaledInstance(350, 350, Image.SCALE_SMOOTH)
    val imageLabel = new JLabel(new ImageIcon(scaledImage))
    imageLabel.setBounds(30, 0, 770, 300)
    frame.add(imageLabel)
  } catch {
    case e: Exception =>
      e.printStackTrace()
      JOptionPane.showMessageDialog(
        frame,
        "Failed to load image: " + e.getMessage
      )
  }

  frame.add(chooseFileButton)
  frame.add(calculateButton)
  frame.add(predictButton)
  frame.add(scrollPane)

  resultArea.setEditable(false)

  var centroids: Array[Array[Double]] = Array()
  var numericMatrix: Array[Array[Double]] = Array()

  chooseFileButton.addActionListener((e: ActionEvent) => {
    val result = fileChooser.showOpenDialog(frame)
    if (result == JFileChooser.APPROVE_OPTION) {
      val file = fileChooser.getSelectedFile
      val data = ArrayBuffer[Array[String]]()

      Iris.readCsv(file.getAbsolutePath, cols => data += cols)
      numericMatrix = data.toArray.map(row =>
        row.init.map(s => Try(s.toDouble).getOrElse(0.0))
      )

      JOptionPane.showMessageDialog(frame, "File loaded successfully!")
    }
  })

  calculateButton.addActionListener((e: ActionEvent) => {
    if (numericMatrix.isEmpty) {
      JOptionPane.showMessageDialog(frame, "Please load a CSV file first.")
    } else {
      val statsBuilder = new StringBuilder()
      val k = 3 // Or let the user choose

      // Perform k-means clustering to calculate centroids
      centroids = Iris.kMeans(numericMatrix, k)

      // Append statistics for each column
      for (i <- numericMatrix(0).indices) {
        val column = numericMatrix.map(_(i))
        val mean = column.sum / column.length
        val variance =
          column.map(x => math.pow(x - mean, 2)).sum / column.length
        val stdDev = math.sqrt(variance)

        statsBuilder.append(
          s"${labels(i)}: Mean = $mean, Variance = $variance, StdDev = $stdDev\n"
        )
      }

      // Append Pearson correlation for each pair of columns
      for (i <- 0 until numericMatrix(0).length - 1) {
        for (j <- i + 1 until numericMatrix(0).length) {
          val correlation = Iris.pearsonCorrelation(numericMatrix, i, j)
          statsBuilder.append(s"Correlation ${labels(i)}: & ${labels(j)}:: $correlation\n")
        }
      }

      // Append centroids information
      statsBuilder.append("Centroïdes des clusters :\n")
      centroids.foreach(centroid => {
        val formattedCentroid = centroid.map("%.2f".format(_)).mkString(", ")
        statsBuilder.append(s"[ $formattedCentroid ]\n")
      })

      // Display all the collected information in the result area
      resultArea.setText(statsBuilder.toString())
    }
  })

  predictButton.addActionListener((e: ActionEvent) => {
    try {
      val point = inputFields.map(_.getText.toDouble)
      if (centroids.isEmpty) {
        JOptionPane
          .showMessageDialog(frame, "Please calculate centroids first.")
      } else {
        val cluster = Iris.predict(point, centroids)
        resultArea.setText(
          resultArea.getText + s"\nPredicted cluster for input: $cluster"
        )
      }
    } catch {
      case _: NumberFormatException =>
        JOptionPane.showMessageDialog(frame, "Please enter valid numbers.")
    }
  })

  frame.setVisible(true)
}
