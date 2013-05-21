

def create3s(lim: Int) = 
	(3 to lim-1 by 3).toList

def create5s(lim: Int) = 
	(5 to lim-1 by 5).toList

def exclude5sfrom3s(lim: Int) =
	(create3s(lim)).remove(x => x % 5 == 0)

def summate(a: List[Int], b:Int) =
	a.foldLeft(b)(_+_)

def mult35(lim: Int): Int =
	summate(exclude5sfrom3s(lim), summate(create5s(lim), 0))
	 
