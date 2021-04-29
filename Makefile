CC=gcc

#rwildcard=$(foreach d,$(wildcard $(1:=/*)),$(call rwildcard,$d,$2) $(filter $(subst *,%,$2),$d))
SDLLIBS=-Wl,-Bstatic -L${exec_prefix}/lib  -lmingw32 -lSDL2main -lSDL2 -lSDL2_image -mwindows  -Wl,--no-undefined -Wl,--dynamicbase -Wl,--nxcompat -Wl,--high-entropy-va -lm -ldinput8 -ldxguid -ldxerr8 -luser32 -lgdi32 -lwinmm -limm32 -lole32 -loleaut32 -lshell32 -lsetupapi -lversion -luuid -static-libgcc

main:
	$(CC) -O3 -s -pipe -Wall -o zoomb0i src/main.c src/ppu.c src/cpu.c src/input.c src/utils.c -lmingw32 -lSDL2main -lSDL2

run: main
	./zoomb0i.exe roms/pkmn.gb
