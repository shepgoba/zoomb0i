#include "input.h"
#include "utils.h"

void handle_keypress(SDL_KeyboardEvent *e, gb_cpu_t *cpu) 
{
    static const SDL_Keycode keys[] = {SDLK_a, SDLK_b, SDLK_o, SDLK_p, SDLK_RIGHT, SDLK_LEFT, SDLK_UP, SDLK_DOWN};

    int key = e->keysym.sym;
    bool is_key_up = e->type == SDL_KEYUP;

    for (int i = 0; i < 8; i++) {
        if (key == keys[i]) {
            set_bit_on(&cpu->key_status, i, is_key_up);
            break;
        } else if ((cpu->key_status >> i) & 1) {
            set_bit_on(&cpu->key_status, i, 1);
        }
    }
}
