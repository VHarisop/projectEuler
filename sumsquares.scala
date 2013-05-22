def gensq(lim: Int) =
	(1 to lim).toList

def sumsq(seq: List[Int]) =
	(seq.map( i => i * i )).foldLeft(0)(_+_)

def sqsum(seq: List[Int]) =
	List(seq.foldLeft(0)(_+_)).map(x => x * x) match {
	case Nil => 0
	case xs :: Nil => xs
	case x :: xs => 0
	}

def solve() =
	sqsum(gensq(100)) - sumsq(gensq(100)) 
