#ifndef INPUT_INCLUDE
#define INPUT_INCLUDE
#include <stdio.h>
#include <SDL2/SDL.h>
#include "cpu.h"

void handle_keypress(SDL_KeyboardEvent *e, gb_cpu_t *cpu);

#endif