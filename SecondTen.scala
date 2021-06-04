object SecondTen {
    def main(args: Array[String]): Unit = {
        assert (decode(List((1,"a"), (2, "e"))) == List("a", "e", "e"))
        assert (decode(List((5,"a"), (1, "e"))) == List("a", "a", "a", "a", "a", "e"))
        assert (decode2(List((5,"a"), (1, "e"))) == List("a", "a", "a", "a", "a", "e"))
        
        assert (encodeDirect(List("a","a","a")) == List((3,"a")))
        assert (encodeDirect(List("a","a","b","b")) == List((2,"a"),(2,"b")))
        
        assert (duplicate(List("a","b")) == List("a","a","b","b"))
        assert (duplicateN(2, List("a", "b")) == List("a","a","b","b"))
        
        assert (drop(2, List("a", "b", "c", "d")) == List("a","b","d"))
        assert (drop(0, List("b", "c", "d")) == List("c","d"))

        assert (split(3, List(1,2,3,4,5)) == (List(1,2,3), List(4,5)))

        assert (slice(0,4, List(1,2,3,4,5,6)) == List(1,2,3,4))
        assert (slice(2,4, List(1,2,3,4,5,6)) == List(3,4))

        assert (rotate(2, List(1,2,3,4,5)) == List(3,4,5,1,2))
        assert (rotate(0, List(1,2,3,4,5)) == List(1,2,3,4,5))
        assert (rotate(3, List(1,2,3)) == List(1,2,3))

        assert (removeAt(0, List(1,2,3)) == List(2,3))
        assert (removeAt(2, List(1,2,3)) == List(1,2))
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

    // P13: run length encoding compression (directly).  
    def encodeDirect[A](xs: List[A]): List[(Int, A)] = {
        // Do this with a fold right. 
        xs.foldRight (List[(Int,A)]()) (
            (el: A, acc: List[(Int,A)]) => {
                if (acc.isEmpty || el != acc.head._2) (1, el) :: acc
                else (acc.head._1+1, el) :: acc.tail
            }
        )
    }

    // P14: Duplicate the elements of a list. 
    def duplicate[A](xs: List[A]): List[A] = {
        xs.flatMap((el:A) => List(el,el))
    }

    // P15: Duplicate the elements of a list a given number of times. 
    def duplicateN[A](N: Int, xs: List[A]): List[A] = {
        xs.flatMap((el:A) => List.fill(N)(el))
    }

    // P16: Drop every Nth element from a list. 
    def drop[A](N: Int, xs: List[A]): List[A] = {
        if (N < 0) throw new IllegalArgumentException
        else if (N == 0) xs.tail
        else xs.head :: drop(N-1, xs.tail)
    }

    // P17: Split a list into two parts. 
    def split[A](N: Int, xs: List[A]): Tuple2[List[A], List[A]] = {
        if (N < 0) throw new IllegalArgumentException    
        else xs.foldRight((List[A](), List[A]()))(
            (el: A, tup: Tuple2[List[A], List[A]]) => {
                val (l1, l2) = tup
                if (l2.length < xs.length - N) (l1, el :: l2)
                else (el :: l1, l2)
            }
        )
    }

    // P18: Extract a slice from a list. 
    def slice[A](start: Int, end: Int, xs: List[A]): List[A] = {
        if (end <= 0) Nil
        else if (start <= 0 && end > 0) xs.head :: slice(start-1, end-1, xs.tail)
        else slice(start-1, end-1, xs.tail)
    }

    // P19: Rotate a list N places to the left. 
    def rotate[A](N: Int, xs: List[A]) : List[A] = {
        if (N < 0) throw new IllegalArgumentException
        else if (N == 0) xs
        else rotate(N-1, xs.tail ++: List(xs.head))
    }

    // P20: Remove the Kth element of a list. 
    def removeAt[A](k: Int, xs: List[A]) : List[A] = {
        if (k < 0) throw new IllegalArgumentException
        else if (k == 0) xs.tail
        else xs.head :: removeAt(k-1, xs.tail)
    }
}