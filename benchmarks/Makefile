package := containers
version := $(shell awk '/^version:/{print $$2}' ../$(package).cabal)
lib := ../dist/build/libHS$(package)-$(version).a

programs := bench-Map bench-Set bench-IntMap bench-IntSet bench-Sequence
all: $(programs)
run: $(patsubst %, %.csv, $(programs))

bench-%: %.hs ../Data/%.hs
	ghc -DTESTING -cpp -O2 -fregs-graph -rtsopts --make -fforce-recomp -i.. -o $@ $<

bench-%-Map: %-Map.hs ../Data/Map.hs
	ghc -DTESTING -cpp -O2 -fregs-graph -rtsopts --make -fforce-recomp -i.. -o $@ $<

bench-%-IntMap: %-IntMap.hs ../Data/IntMap.hs
	ghc -DTESTING -cpp -O2 -fregs-graph -rtsopts --make -fforce-recomp -i.. -o $@ $<

bench-%.csv bench-%.html: bench-%
	./bench-$* -u bench-$*.csv -o bench-$*.html +RTS -K10M

bench-MapIntInt: MapIntInt.hs ../Data/Map/Base.hs ../Data/Map/BaseIntInt.hs ../Data/IntMap/BaseInt.hs
	ghc -DTESTING -cpp -O2 -fregs-graph -rtsopts --make -fforce-recomp -i.. -o $@ $<

.PHONY: clean
clean:
	-find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;
	-rm -f $(programs)
