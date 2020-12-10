// Use the named values (val) below whenever your need to
// read/write inputs and outputs in your program. 
val inputFilePath  = "FULL_PATH_OF_YOUR_INPUT_FILE"
val outputDirPath = "FULL_PATH_OF_YOUR_OUTPUT_DIRECTORY"



    import scala.io.Source
    import scala.math.min
    import scala.math.max
    
    val source =Source.fromFile("inputFilePath")
    

    val lines=source.getLines.toArray

    source.close()

    for(i<-1 until lines.length)
        val lineArray=lines(i).split(",")
       
        val byte;
        if(lineArray[3].endswith(KB)){
           byte=Integer.parseInt(lineArray[3].substring(0,lineArrays[3].length-2))
           byte=byte*1024;
        }
        else if(lineArray[3].endswith(MB)){
            byte=Integer.parseInt(lineArray[3].substring(0,lineArrays[3].length-2))
            byte=byte*1024*1024;
         }
         else if(lineArray[3].endswith(B)){
             byte=Integer.parseInt(lineArray[3].substring(0,lineArrays[3].length-1))
         }
         val paris = lineArray.map(lineArray[0]=>(lineArray[0],byte))      
     }
     
         val min = paris.reduceByKey(min)
         val max = paris.reduceByKey(max)

         val avg = pairs.reduceByKey(mean)
         val var = pairs.reduceBykey(variance)
         
         
          def mean[T: Numeric](xs: Iterable[T]): Double = xs.sum.toDouble / xs.size

          def variance[T: Numeric](xs: Iterable[T]): Double = {
          val avg = mean(xs)
          xs.map(_.toDouble).map(a => math.pow(a - avg, 2)).sum / xs.size
          }


          val result=min+"\t"+max+"\t"+avg+"\t"+var;
          
          result.saveAsTextFile("outputDirPath")










