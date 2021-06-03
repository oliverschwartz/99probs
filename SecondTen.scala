object SecondTen {
    def main(args: Array[String]): Unit = {
        assert (decode(List((1,"a"), (2, "e"))) == List("a", "e", "e"))
        assert (decode(List((5,"a"), (1, "e"))) == List("a", "a", "a", "a", "a", "e"))
        assert (decode2(List((5,"a"), (1, "e"))) == List("a", "a", "a", "a", "a", "e"))
    }

    // P12: Decode a run-length encoded list. 
    def decode[A](enc: List[(Int, A)]) : List[A] = {
        
        // Apply a map to the list which turns every tuple into a list.
        def f (tup: Tuple2[Int,A]): List[A] = {
            tup match {
                case (cnt, el) =>
                    if (cnt == 0) List[A]()
                    else el :: f((cnt-1, el))
            }
        }
        
        // We then want to flatten this list of lists. 
        val mapped = enc.map(f)
        mapped.flatten
    }

    // P12 alternate. 
    def decode2[A](enc: List[(Int, A)]) : List[A] = {
        enc.flatMap(tup => List.fill(tup._1)(tup._2))
    }
}