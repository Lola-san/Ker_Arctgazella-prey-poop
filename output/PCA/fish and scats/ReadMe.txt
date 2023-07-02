Written on 2023/06/28
We standardised per type (i.e. fish or scat) only for classical PCA, so classical PCA are on relative compositions and not the other ones. 
We can't do it the same way for the other ones (coda methods) because the standardisation results in negative values that can't be ilr transformed afterwards. So I don't know how to do it... 

Update 2023/07/02
Instead of standardizing, I did the PCA (the three : coda robust, coda non-robust and classical) on relative compositions. For the classical, I scaled and centered the table afterwards, and for the two coda I used the coda methods (that use an ilr transformation).