def get2nd(x: Int, y: Int) = y

def unplist(x: List[(Int, Int)]): List[Int] =
	x match {
		case Nil => Nil
		case (y1, y2) :: ys => get2nd(y1, y2) :: unplist(ys)
	}



def sieve(lim: Int) =
	unplist(
		(for {i <- 3 until lim by 2;
	     	      j <- 2*i until lim by i } 
		yield (i, j)).toList
	)
	

def makeSieve(mysieve: List[Int], lim: Int) =
	(for {i <- 3 until lim by 2 if !mysieve.contains(i)} yield i).toList


def primes(lim: Int) = 
	makeSieve(sieve(lim), lim)
	 
