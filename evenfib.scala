def createFibs(a: Int, b: Int, lim: Int): List[Int] = 
	if (b > lim) a :: Nil else a :: createFibs(b, (b+a), lim)



def pickFibs(fibs: List[Int], count: Int): List[Int] =
	fibs match {
	case List() => Nil
	case x :: xs => if (x % 2 == 1) pickFibs(xs, 2)
			else x :: pickFibs(xs, 1)
}

def solve(lim: Int) =
	pickFibs(createFibs(1, 2, lim), 1).foldLeft(0)(_+_)

