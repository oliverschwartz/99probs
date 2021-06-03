object FirstTen { 
    def main(args: Array[String]): Unit = {
        // Testing. 
        assert (first(List(1,2,3)) == 1)
        assert (last(List(1,2,3)) == 3)

        assert (penultimate(List(1,2,3)) == 2)
        assert (penultimate(List(1,2,3,4,5)) == 4)

        assert (kth(0, List(1,2,3,4,5)) == 1)
        assert (kth(2, List(1,2,3,4,5)) == 3)

        assert (length(List(1,2,3,4,5)) == 5)
        assert (length(List()) == 0)

        assert (isPalindrome(List(1,2,2,1)) == true)
        assert (isPalindrome(List(1,2,3,2,1)) == true)
        assert (isPalindrome(List(1,3,3,2,1)) == false)
        assert (isPalindrome(List(1,2)) == false)

        assert (flattenList(List(List(1,List(2,List(3,List(List(4,List(5)))))))) == List(1,2,3,4,5))
        assert (flattenList(List(List(List()))) == List())

        assert (compress(List(1,1,1,1,2,2,2,3,3,3)) == List(1,2,3))
        assert (compress(List(1,2,3,4,3,2,3)) == List(1,2,3,4,3,2,3))
        assert (compressFR(List(1,2,2,3,3,4)) == List(1,2,3,4))
        
        assert (pack(List(1,2,2,3,3,4)) == List(List(1),List(2,2),List(3,3),List(4)))
        assert (pack(List(1,1,1,1)) == List(List(1,1,1,1)))
        assert (pack(List()) == List(List()))
        
        assert (encode(List(1,2,3,4,4,5,5)) == List((1,1), (2,1), (3,1), (4,2), (5,2)))
        assert (encode(List()) == List())
    }

    // Extract first element of list. 
    def first[A](l: List[A]): A = {
        l match { 
            case hd :: tl => hd 
            case _ => throw new NoSuchElementException 
        }
    } 

    // Extract last element of list. 
    def last[A](l: List[A]): A = {
        l match {
            case hd :: Nil => hd
            case _ :: tl => last(tl)
            case _ => throw new NoSuchElementException
        }
    }

    // Extract second last element of list. 
    def penultimate[A](l: List[A]): A = {
        val len = l.length
        if (len < 2) {
            throw new NoSuchElementException
        } else {
            l match {
                case hd :: tl :: Nil => hd
                case hd :: tl => penultimate(tl)
                case _ => throw new RuntimeException 
            }
        }
    }

    // Get the kth element of a list. 
    def kth[A](k: Int, l: List[A]): A = {
        if (k < 0) {
            throw new IllegalArgumentException
        } else if (k == 0) {
            l.head
        } else {
            kth(k-1, l.tail)
        }
    }

    // Find the length of a list. 
    def length[A](l: List[A]): Int = {
        l match {
            case Nil => 0
            case hd :: tl => 1 + length(tl)
        }
    }
    
    // Find the length of a list. 
    def isPalindrome[A](l: List[A]): Boolean = {
        val len = length(l)
        l match {
            case Nil => true 
            case hd :: Nil => true 
            case _ => 
                if (l(0) != l(len-1)) {
                    false
                } else {
                    isPalindrome(l.slice(1,len-1))
                }
        }
    }

    // Flatten a nested list structure. 
    def flattenList(l: List[Any]) : List[Any] = {
        l match { 
            case Nil => List()
            case hd :: tl => 
                val rest = flattenList(tl)
                hd match { 
                    case x: List[Any] => flattenList(x) :++ rest
                    case _ => hd :: rest
                }
        }
    }

    // Remove consecutive elements in a list. 
    def compress[A](xs: List[A]) : List[A] = {

        // Nested helper function. Pass the previous element as option.
        def helper(xs: List[A], prevOpt: Option[A]) : List[A] = {
            xs match {
                case Nil => List()
                case hd :: tl => 
                    val rest = helper(tl, Some(hd))
                    prevOpt match {
                        case Some(prev) => if (hd == prev) rest else hd :: rest
                        case None => hd :: rest
                    }
            }
        }

        helper(xs, None)
    }

    // As above, but with a fold right.
    def compressFR[A](xs: List[A]) : List[A] = {
        xs.foldRight(List[A]())(
            (el: A, acc: List[A]) => 
                if (acc.isEmpty || el != acc.head) el :: acc
                else acc
        )
    }

    // Pack consecutive duplicates of list elements into sublists.
    def pack[A](xs: List[A]): List[List[A]] = {
        xs.foldRight (List(List[A]())) (
            (el: A, acc: List[List[A]]) => {
                val hd = acc.head
                val tl = acc.tail
                if (hd.isEmpty) List(List(el))
                else {
                    if (el == hd.head) {
                        (el :: hd) :: tl
                    } else {
                        List(el) :: hd :: tl
                    }
                }
            }
        )
    }

    // Run length encoding of a list. 
    def encode[A](xs: List[A]): List[(A, Int)] = {
        if (xs.isEmpty) List[(A,Int)]()
        else {
            val packed = pack(xs)
            packed.map(x => (x.head, x.length))
        }
    }
}
