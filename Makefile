cpiwb : 
		ghc -O2 --make cpiwb

clean : 
		rm *.o *.hi cpiwb

.PHONY : clean cpiwb
