def maketriplets(lim: Int) = 
	for {i <- 1 until lim-2;
		 j <- i until lim-1;
		 k <- j until lim;
		 if (i*i + j*j == k*k);
		 if (i + j + k == lim)} yield (i, j, k) 
	
