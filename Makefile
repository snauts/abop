all:
	cc -c -fPIC -fno-stack-protector machine.c
	ld -lm -lpng -lGL -lSDL -shared -fPIC machine.o -o machine.so

clean:
	rm -f *.fasl *.inc plant.rib point-of-view.rib scene.png scene.rca \
		machine.o machine.so scene.obj *.pnm shadow.png scene.json
