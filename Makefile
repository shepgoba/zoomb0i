CC = gcc

SRC_DIR := src

OBJ_DIR := obj
SRC_FILES := $(wildcard $(SRC_DIR)/*.c)
OBJ_FILES := $(patsubst $(SRC_DIR)/%.c,$(OBJ_DIR)/%.o,$(SRC_FILES))

CXXFLAGS := -O3

ROM_FILE = roms\kirbydl.gb

build\zoomb0i.exe: $(OBJ_FILES)
	$(CC) $(LDFLAGS) -o $@ $^ -lmingw32 -lSDL2main -lSDL2

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<


run: build\zoomb0i.exe
	.\build\zoomb0i.exe $(ROM_FILE)

do: .\build\zoomb0i.exe run

clean:
	@rm -rf obj/*.o
	@rm -rf build/*.exe