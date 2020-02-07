package avro

import org.apache.avro.specific.SpecificDatumWriter
import org.apache.avro.file.DataFileWriter

/*
© 2019 Nokia.All rights reserved
 */
object MainAvro {

  def main(args: Array[String]): Unit = {

    val user = User("a", "b", "c")
    val userDatumWriter = new SpecificDatumWriter[User](classOf[User])
    val dataFileWriter = new DataFileWriter[User](userDatumWriter)

  }

}
