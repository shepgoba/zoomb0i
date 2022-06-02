#include <SDL2/SDL.h>
#include "cpu.h"
#include "ppu.h"

extern void _halt();

void init_ppu(gb_ppu_t *ppu, gb_cpu_t *cpu) {
    ppu->oam = cpu->address_space + 0xFE00;
    ppu->vram = cpu->vram;

    bool color_type = 0; // 0: grayscale, 1: green
    if (color_type == 0) {
        ppu->color_palette[0][0] = 255;
        ppu->color_palette[0][1] = 192;
        ppu->color_palette[0][2] = 96;
        ppu->color_palette[0][3] = 0;

        ppu->color_palette[1][0] = 255;
        ppu->color_palette[1][1] = 192;
        ppu->color_palette[1][2] = 96;
        ppu->color_palette[1][3] = 0;
        
        ppu->color_palette[2][0] = 255;
        ppu->color_palette[2][1] = 192;
        ppu->color_palette[2][2] = 96;
        ppu->color_palette[2][3] = 0;
    } else if (color_type == 1) {
        ppu->color_palette[0][0] = 155;
        ppu->color_palette[0][1] = 139;
        ppu->color_palette[0][2] = 48;
        ppu->color_palette[0][3] = 15;

        ppu->color_palette[1][0] = 188;
        ppu->color_palette[1][1] = 172;
        ppu->color_palette[1][2] = 98;
        ppu->color_palette[1][3] = 56;

        ppu->color_palette[2][0] = 15;
        ppu->color_palette[2][1] = 48;
        ppu->color_palette[2][2] = 15;
        ppu->color_palette[2][3] = 15;
    }
}



static inline uint8_t translate_palette_bg(gb_cpu_t *cpu, uint8_t col) {
    return (*cpu->bgp >> col * 2) & 0b00000011;
}

static uint8_t translate_palette_sprite(gb_cpu_t *cpu, uint8_t col, uint8_t palette) {
    if (col == 0)
        return 0;

    if (palette)
        return (*cpu->obp1 >> col * 2) & 0b00000011;
    else
        return (*cpu->obp0 >> col * 2) & 0b00000011;
}

uint8_t reverse(uint8_t num) {
    uint8_t pog = ((num & 0x01) << 7)
                | ((num & 0x02) << 5)
                | ((num & 0x04) << 3)
                | ((num & 0x08) << 1)
                | ((num & 0x10) >> 1)
                | ((num & 0x20) >> 3)
                | ((num & 0x40) >> 5)
                | ((num & 0x80) >> 7);
    return pog;
}


void draw_window_scanline(gb_ppu_t *ppu, gb_cpu_t *cpu, uint8_t *tile_data) {
    uint8_t *window_tile_map = (*cpu->lcdc >> 6) & 1 ? (uint8_t *)(ppu->vram + 0x1C00) : (uint8_t *)(ppu->vram + 0x1800);
    if (ppu->scanline >= *cpu->wy) {
        for (int i = 0; i < 0x14; i++) {
            uint8_t offset = *(window_tile_map + (((ppu->scanline + *cpu->wy) / 8) % 32) * 32 + ((((*cpu->wx) - 7) / 8) + i) % 32);

            int16_t base = 0;
            if (!((*cpu->lcdc >> 4) & 1)) {
                if (offset > 128) {
                    base = -0x800;
                } else {
                    base = 0x800;
                }
            }

            uint16_t finalOffset = base + offset * 16 + (((ppu->scanline + *cpu->wy) % 8) * 2);
            uint8_t top = tile_data[finalOffset];
            uint8_t bottom = tile_data[finalOffset + 1];

            for (int x = 0; x < 8; x++) {
                uint8_t pixelCol = (((bottom >> (7 - x)) & 1) << 1) | ((top >> (7 - x)) & 1);
                uint8_t actualCol = translate_palette_bg(cpu, pixelCol);
                uint8_t pixelOffset = i * 8 + x;

                ppu->pixel_buf[ppu->scanline * 160 + pixelOffset] = ((0xFF << 24) | (ppu->color_palette[0][actualCol] << 16) | (ppu->color_palette[1][actualCol] << 8) | ppu->color_palette[2][actualCol]);
            }
        }
   }
}

static inline bool should_draw_window(gb_cpu_t *cpu) 
{
    return (*cpu->lcdc >> 5) & 1;
}

void perform_scanline(gb_ppu_t *ppu, gb_cpu_t *cpu) 
{

    ppu->during_hblank = false;

    uint8_t *tile_data = (*cpu->lcdc >> 4) & 1 
                         ? (uint8_t *)(ppu->vram) : (uint8_t *)(ppu->vram + 0x800);
    uint8_t *bg_tile_map = (*cpu->lcdc >> 3) & 1 
                         ? (uint8_t *)(ppu->vram + 0x1C00) : (uint8_t *)(ppu->vram + 0x1800);

    for (int i = 0; i < 0x15; i++) {
        uint16_t offset = *(bg_tile_map + (((ppu->scanline + *cpu->scy) / 8) % 32) * 32 + (((*cpu->scx / 8) + i) % 32));
        //printf("offset: %x\n", offset);
 
        int16_t base = 0;

        if (!((*cpu->lcdc >> 4) & 1)) {
            if (offset > 128) {
                base = -0x800;
            } else {
                base = 0x800;
            }
        }
        
        uint16_t finalOffset = base + offset * 16 + (((ppu->scanline + *cpu->scy) % 8) * 2);
        uint8_t top = tile_data[finalOffset];
        uint8_t bottom = tile_data[finalOffset + 1];

        for (int x = 0; x < 8; x++) {
            uint8_t pixelCol = (((bottom >> (7 - x)) & 1) << 1) | ((top >> (7 - x)) & 1);
            uint8_t actualCol = translate_palette_bg(cpu, pixelCol);

            //uint8_t pixelOffset = i * 8 + x + (8 - *cpu->scx % 8) - 8;
            uint8_t pixelOffset = i * 8 + x - *cpu->scx % 8;
            if (pixelOffset >= 0 && pixelOffset < 160) {
                ppu->pixel_buf[ppu->scanline * 160 + pixelOffset] = ((0xFF << 24) | (ppu->color_palette[0][actualCol] << 16) | (ppu->color_palette[1][actualCol] << 8) | ppu->color_palette[2][actualCol]);
            }
        }
    }
    if (should_draw_window(cpu)) {
        draw_window_scanline(ppu, cpu, tile_data);
    }

    for (uint8_t oam_entry = 0; oam_entry < 0x9F; oam_entry += 4) {
        uint8_t spriteY = ppu->oam[oam_entry];
        uint8_t spriteX = ppu->oam[oam_entry + 1];
        
        // dont bother rendering unless visible
        if (!spriteY && !spriteX)
            continue;

        uint8_t tileOffset = ppu->oam[oam_entry + 2];
        uint8_t attributes = ppu->oam[oam_entry + 3];
        
        int spriteSize = 8;

        // get current sprite size to render. 0 = 8x8, 1 = 8x16
        if ((*cpu->lcdc >> 2) & 1) {
            spriteSize = 16;
            if (ppu->scanline >= spriteY - 8) {
                tileOffset |= 0x1;
            } else  {
                tileOffset &= 0xfe;
            }
        }

        int sliceOffset;
        if ((attributes >> 6) & 1) {
            sliceOffset = 2 * ((ppu->scanline % 8) - (spriteY % 8)) % 16;
        } else {
            sliceOffset = 2 * ((ppu->scanline % 8) + (8 - spriteY % 8)) % 16;
        }

        int finalOffset = tileOffset * 16 + sliceOffset;

        uint8_t top = ppu->vram[finalOffset];
        uint8_t bottom = ppu->vram[finalOffset + 1];
        if ((attributes >> 5) & 1) {
            top = reverse(top);
            bottom = reverse(bottom);
        }

        if (ppu->scanline >= spriteY - 16 && ppu->scanline < (spriteY - 16 + spriteSize)) {
            for (int x = 7; x >= 0; x--) {
                uint8_t transparent = false;
                uint8_t pixelCol = (((bottom >> x) & 1) << 1) | ((top >> x) & 1);
                if (pixelCol == 0) {
                    transparent = true;
                }
                
                uint8_t actualCol = translate_palette_sprite(cpu, pixelCol, (attributes >> 4) & 1);

                if (!transparent) {
                    uint8_t val = spriteX - x;
                    if (val < 160)
                        ppu->pixel_buf[ppu->scanline * 160 + val] = ((0x00 << 24) | (ppu->color_palette[0][actualCol] << 16) | (ppu->color_palette[1][actualCol] << 8) | ppu->color_palette[2][actualCol]);
                }
            }
        }
    }
    // next draw window
    ppu->during_hblank = true; 
}
