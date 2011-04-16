HC=~/ghc-head/bin/ghc

all : peeling-core unrolling-core

peeling-core :
	$(HC) -fforce-recomp -O Peeling.hs -fplugin=LoopUnrolling.Plugin

unrolling-core :
	$(HC) -fforce-recomp -O Unrolling.hs -fplugin=LoopUnrolling.Plugin

clean :
	rm -f *.o *.hi *.core Peeling Unrolling

.PHONY : peeling-core unrolling-core all
