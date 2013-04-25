cpiwb : reset
		ghc -O2 --make -W -fno-warn-unused-binds -fno-warn-unused-matches cpiwb

clean : 
		rm -rf cpiwb *.o *.hi CPi/*.o CPi/*.hi

reset :
		rm -rf cpiwb