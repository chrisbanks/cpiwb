cpiwb : 
		ghc --make cpiwb

clean : 
		rm *.o *.hi cpiwb

.PHONY : clean cpiwb
