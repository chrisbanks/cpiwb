cpiwb : reset
		ghc -O2 --make cpiwb

clean : 
		rm -rf cpiwb *.o *.hi CPi/*.o CPi/*.hi

reset :
		rm -rf cpiwb