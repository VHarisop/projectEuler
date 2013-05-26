def get2nd(x: Int, y: Int) = y

def unplist(x: List[(Int, Int)]): List[Int] =
	x match {
		case Nil => Nil
		case (y1, y2) :: ys => get2nd(y1, y2) :: unplist(ys)
	}



def sieve(lim: Int, prime_array: Array[Int]) =	
		for {i <- 2 until lim by 1;
	     	      j <- 2*i until lim by i } 
		prime_array(j) = 1


def makesieve(fun: Unit, prime_array: Array[Int]) = 
		(for {i <- 2 until (prime_array.length - 1);
			 if prime_array(i) == 0 }
		yield i).toList
	
def primes(lim: Int, pr_arr: Array[Int]) = 
	makesieve(sieve(lim, pr_arr), pr_arr)
	 

def generate_primes(lim: Int) =
	primes(lim, Array.fill(lim)(0))
