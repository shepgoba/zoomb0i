#ifndef ppu_h
#define ppu_h

#define SCALE 4
#define INTERNAL_SCREEN_WIDTH 160
#define INTERNAL_SCREEN_HEIGHT 144
#define VISIBLE_SCREEN_WIDTH (INTERNAL_SCREEN_WIDTH*SCALE)
#define VISIBLE_SCREEN_HEIGHT (INTERNAL_SCREEN_HEIGHT*SCALE)

#include "cpu.h"
#include "utils.h"

struct gb_cpu;

struct SDL_Renderer;

struct gb_ppu {
    uint8_t *vram;
    uint8_t *oam;
    uint32_t *pixel_buf;
    uint8_t color_palette[3][4];
    uint8_t mode;
    uint8_t scanline;
    bool during_hblank;
    struct SDL_Renderer *renderer;
    //struct gb_cpu *cpu;
};

typedef struct gb_ppu gb_ppu_t;

void init_ppu(gb_ppu_t *ppu, struct gb_cpu *cpu);
void draw_screen(gb_ppu_t *ppu);
void perform_scanline(gb_ppu_t *ppu, struct gb_cpu *cpu);
#endif