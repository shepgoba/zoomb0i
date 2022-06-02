#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <SDL2/SDL.h>
#include "rom.h"
#include "cpu.h"
#include "ppu.h"
#include "utils.h"
#include "input.h"
#include "constants.h"

static void check_interrupts(gb_cpu_t *cpu) 
{
	for (int b = 0; b < 5; b++) {
		uint8_t int_enabled = (*cpu->ie >> b) & 1;
		if (int_enabled) {
			uint8_t bit = (*cpu->ifl >> b) & 1;
			if (bit) {
				if (cpu->ime) {
					_push(cpu, &cpu->pc);
					cpu->pc = 0x40 + (b * 0x8);
					*cpu->ifl &= ~(1UL << b);
					cpu->ime = 0;
					cpu->instruction_wait_cycles = 8;
					cpu->shouldHalt = false;
					break;
				} else {
					cpu->shouldHalt = false;
				}
			}
		}
	}
}


static void update_display(gb_cpu_t *cpu, SDL_Texture *display_texture, uint32_t *pixels) 
{
	SDL_UnlockTexture(display_texture);

	static SDL_Rect main_text_rect = {0, 0, INTERNAL_SCREEN_WIDTH * SCALE, INTERNAL_SCREEN_HEIGHT * SCALE};
	SDL_RenderCopy(cpu->ppu->renderer, display_texture, NULL, &main_text_rect);
	SDL_RenderPresent(cpu->ppu->renderer);

	int texture_pitch;
	SDL_LockTexture(display_texture, NULL, (void **)&pixels, &texture_pitch);
}


#define BACKTRACE_SIZE 0x1000000

int main(int argc, char *argv[]) {
	const bool load_bootstrap = false;
	const bool max_speed = false;
	const double target_frame_rate = 59.7275;
	const double target_delay_time = 1000 / target_frame_rate;


	SDL_Init(SDL_INIT_VIDEO);

	if (argc < 2) {
		SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, "Error", "Usage: zoomb0i <rom file>", NULL);
		return 1;
	}

	FILE *rom = fopen(argv[1], "rb");
	if (!rom) {
		SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, "Error", "Failed to open rom file.", NULL);
		return 1;
	}

	char window_title[256];
	char *game_name = malloc(strlen(argv[1]) + 1);

	last_path_component(argv[1], game_name);
	snprintf(window_title, 256, "%s - %s", "zoomb0i 0.1~beta", game_name);
	free(game_name);

	SDL_Window *main_window = SDL_CreateWindow(window_title, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, VISIBLE_SCREEN_WIDTH, VISIBLE_SCREEN_HEIGHT, 0);

	if (!main_window) {
		err("Failed to create window.\n");
		return 1;
	}

	uint32_t *pixels = 0;

	gb_cpu_t cpu = {0};
	gb_ppu_t ppu = {0};

	init_cpu(&cpu, &ppu, load_bootstrap);
	init_ppu(&ppu, &cpu);

	// 64 MB of instr history
	cpu.backtrace = calloc(BACKTRACE_SIZE, sizeof(uint64_t));
	if (!cpu.backtrace) {
		printf("couldn't initialize backtrace memory\n");
		return -1;
	}

	ppu.renderer = SDL_CreateRenderer(main_window, -1, SDL_RENDERER_ACCELERATED);

	struct rom_header *header = read_bytes(rom, 0x100, sizeof(struct rom_header));
	if (header->old_license_code == 0x33) {
		if (header->sgb_flag == 0x03)
			cpu.run_mode = 2;
		else
			cpu.run_mode = 1;
	}
	
	if (header->cartridge_type >= 0x1 && header->cartridge_type <= 0x3) {
		printf("mbc1 enabled\n");
		cpu.mbc1 = true;
	} else if (header->cartridge_type == 0x5 || header->cartridge_type == 0x6) {
		printf("mbc2 enabled\n");
		cpu.mbc2 = true;
	} else if (header->cartridge_type >= 0xF && header->cartridge_type <= 0x13) {
		printf("mbc3 enabled\n");
		cpu.mbc3 = true;
	}

	/*uint32_t rom_size = 0;
	if (header->rom_size <= 0x8)
		rom_size = 32768 << header->rom_size;
	else if (header->rom_size == 0x52)
		rom_size = 72 * 32768;
	else if (header->rom_size == 0x53)
		rom_size = 80 * 32768;
	else
		rom_size = 96 * 32768;*/
		
	free(header);
	fseek(rom, 0L, SEEK_END);

	size_t rom_file_size = ftell(rom); 
	fseek(rom, 0L, SEEK_SET);

	uint8_t *rom_data = malloc(rom_file_size);
	if (!rom_data) {
		printf("could not allocate rom\n");
		return -1;
	}

	fread(rom_data, sizeof(char), rom_file_size, rom);
	cpu.romptr = rom_data;
	fclose(rom);

	memcpy(&cpu.address_space, rom_data, cap(rom_file_size, 0x8000));

	if (load_bootstrap) {
		FILE *bootloader = fopen("bootloader.bin", "rb");
		if (bootloader)
			fread(cpu.address_space, 1, 0x100, bootloader);
		fclose(bootloader);
	}

	#ifdef DEBUG
	FILE *disasmDump = fopen("debug/asmdump.txt", "w+");
	if (disasmDump)
		printf("opened folder");
	#endif

	SDL_Texture *texture = SDL_CreateTexture(ppu.renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, INTERNAL_SCREEN_WIDTH, INTERNAL_SCREEN_HEIGHT);
	int texture_pitch;
	SDL_LockTexture(texture, NULL, (void **)&pixels, &texture_pitch);


	ppu.pixel_buf = pixels;


	bool running = true;


	uint32_t frame_start = 0, 
	         frame_time = 0, 
			 dot_clock_wait = 0;



	int timer_counter = 0;
	int div_counter = 0;
	while (running) {
		frame_start = SDL_GetTicks();
		SDL_Event e;
		while (SDL_PollEvent(&e)) {
			if (e.type == SDL_QUIT) {
				running = false;
			} else if (e.type == SDL_KEYDOWN || e.type == SDL_KEYUP) {
				handle_keypress(&e.key, &cpu);
				if (e.key.keysym.sym == SDLK_d) {
					printf("dumping ram\n");
					ram_dump(&cpu);
					//print_backtrace(&cpu, 100000);
					return 0;
				}
				
			}
		}

		bool screen_on = ((*cpu.lcdc) >> 7) & 1;
		// 70224 instructions per frame (59.7275 frames per second on DMG)
		for (int i = 0; i < 70224; i++) {
			update_input_registers(&cpu);		

			if (div_counter == 255) {
				(*cpu.div)++;
				div_counter = 0;
			}
			uint8_t interval = (*cpu.tac) & 0b00000011;
			uint16_t moduloInterval = 1024;
			if (interval > 0)
				moduloInterval >>= (8 - interval * 2);

			if ((i % moduloInterval) == 0 && ((*cpu.tac >> 2) & 1)) {
				uint16_t val = *cpu.tima;
				if (val > 0 && val + 1 == 256) {
					*cpu.tima = *cpu.tma;
					set_interrupt_flag(&cpu, TIMER_IFLAG, 1);
				} else {
					(*cpu.tima)++;
				}
				timer_counter = 0;
			}
			
			if (cpu.instruction_wait_cycles <= 0) {
				if (!cpu.shouldHalt) {
					if (cpu.queued_tasks) {
						for (unsigned char b = 0; b < 4; b++) {
							bool bit = (cpu.queued_tasks >> b) & 1;
							if (bit) {
								struct task_data *task_data = &cpu.queued_tasks_data[cpu.queued_tasks_data_ptr];
								
								if (task_data->cycles_until_execution == 0) {
									if (b == QUEUE_DMA_TRANSFER) {
										dma_transfer(&cpu, task_data->data);
									} else if (b == QUEUE_INTERRUPT_ENABLE) {
										cpu.ime = true;
									} else if (b == QUEUE_DMA_FINISHED)  {
										cpu.in_dma = false;
									}
									cpu.queued_tasks &= ~(1UL << b);
									cpu.queued_tasks_data_ptr--;
								} else {
									task_data->cycles_until_execution--;
								}
							}
						}
					}

					uint32_t instruction = fetch_instruction(&cpu);
					execute_instruction(&cpu, instruction, NULL);
				}
			}

			check_interrupts(&cpu);
			cpu.instruction_wait_cycles--;
			if (dot_clock_wait > 0) {
				dot_clock_wait--;
				continue;
			}

			uint8_t vmode = *cpu.stat & 0b11;
			switch (vmode) {
				case 2: {
					set_interrupt_flag(&cpu, LCD_STAT_IFLAG, 1);
					dot_clock_wait = 80;
					vmode = 3;
					break;
				}
				case 3: {
					set_interrupt_flag(&cpu, LCD_STAT_IFLAG, 1);
					dot_clock_wait = 172;
					vmode = 0;
					break;
				}
				case 0: {
					set_interrupt_flag(&cpu, LCD_STAT_IFLAG, 1);
					perform_scanline(&ppu, &cpu);
					dot_clock_wait = 204;
					vmode = 2;

					if (ppu.scanline == 143) {
						set_interrupt_flag(&cpu, VBLANK_IFLAG, 1);

						if (screen_on) {
							update_display(&cpu, texture, pixels);
						}

						vmode = 1;
						break;
					}
					ppu.scanline++;
					break;
				}
				case 1: {
					dot_clock_wait = 456;
					ppu.scanline++;
					if (ppu.scanline > 153) {
						vmode = 2;
						ppu.scanline = 0;
					}
					break;
				}
				
			}

			*cpu.stat &= 0b11111100;
			*cpu.stat |= vmode & 0b11;
			ppu.mode = vmode;
			*cpu.ly = ppu.scanline;

			div_counter++;
			timer_counter++;
		}

		frame_time = SDL_GetTicks() - frame_start;

		if (target_delay_time > frame_time) {
			if (!max_speed)
				SDL_Delay(target_delay_time - frame_time);
		}
	}

	free(rom_data);
	free(cpu.backtrace);

	#ifdef DEBUG
	fclose(disasmDump);
	#endif

	SDL_DestroyTexture(texture);
	SDL_DestroyRenderer(ppu.renderer);
	SDL_DestroyWindow(main_window);
	SDL_Quit();

	return 0;
}
