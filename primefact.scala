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


def find_factors(num: Int) =
	(for {i <- generate_primes(math.sqrt(num).toInt);
		  if (num % i) == 0} yield i).toList

