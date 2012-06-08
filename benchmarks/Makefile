all:

bench-%: %.hs force
	ghc -O2 -DTESTING $< -i../$(TOP) -o $@ -outputdir tmp -rtsopts

bench-%-Map: %-Map.hs ../Data/Map.hs
	ghc -DTESTING -cpp -O2 -fregs-graph -rtsopts --make -fforce-recomp -i.. -o $@ $<

bench-%-IntMap: %-IntMap.hs ../Data/IntMap.hs
	ghc -DTESTING -cpp -O2 -fregs-graph -rtsopts --make -fforce-recomp -i.. -o $@ $<

bench-MapIntInt: MapIntInt.hs ../Data/Map/Base.hs ../Data/Map/BaseIntInt.hs ../Data/IntMap/BaseInt.hs
	ghc -DTESTING -cpp -O2 -fregs-graph -rtsopts --make -fforce-recomp -i.. -o $@ $<


bench-%.csv bench-%.html: bench-%
	./bench-$* -v -u bench-$*.csv -o bench-$*.html

.PHONY: force clean veryclean
force:

clean:
	rm -rf tmp $(patsubst %.hs, bench-%, $(wildcard *.hs))

veryclean: clean
	rm -rf *.csv
