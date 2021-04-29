#ifndef cpu_h
#define cpu_h



#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "ppu.h"
#include "utils.h"
#include "rom.h"

#define __noinline __attribute__((noinline))

#define C_FLAG 4
#define H_FLAG 5
#define N_FLAG 6
#define Z_FLAG 7

#define VBLANK_IFLAG 0
#define LCD_STAT_IFLAG 1
#define TIMER_IFLAG 2
#define SERIAL_IFLAG 3
#define JOYPAD_IFLAG 4

#define QUEUE_DMA_TRANSFER 0
#define QUEUE_INTERRUPT_ENABLE 1
#define QUEUE_TIMER_INTERRUPT_REQUEST 2
#define QUEUE_DMA_FINISHED 3

typedef gb_ppu_t _gb_ppu_t;

struct task_data {
	uint8_t data;
	uint8_t cycles_until_execution;
};
__attribute__((packed))
struct gb_cpu {
	bool load_bootstrap;
	uint8_t shouldHalt;
	int instruction_wait_cycles;
	uint8_t run_mode;
	uint8_t mode;
	uint32_t queued_tasks;
	uint8_t queued_tasks_data_ptr;
	uint8_t last_bank;
	struct task_data queued_tasks_data[32];

	uint16_t *af;
	uint16_t *bc;
	uint16_t *de;
	uint16_t *hl;

	uint8_t *registers[8];

	uint8_t f;
	uint8_t a;

	uint8_t c;
	uint8_t b;

	uint8_t e;
	uint8_t d;

	uint8_t l;
	uint8_t h;


	uint16_t sp;
	uint16_t pc;

	// all the memory
	uint8_t address_space[0x10000];

	uint8_t *lcdc;
	uint8_t *stat;
	uint8_t *scx;
	uint8_t *scy;
	uint8_t *wx;
	uint8_t *wy;
	uint8_t *ly;
	uint8_t *lyc;
	uint8_t *bgp;
	uint8_t *obp0;
	uint8_t *obp1;
	uint8_t *div;
	uint8_t *tima;
	uint8_t *tma;
	uint8_t *tac;
	uint8_t *ifl;
	uint8_t *sb;
	uint8_t *sc;
	uint8_t *joyp;
	uint8_t *ie;
	uint8_t *dma;
	uint8_t key_status;

	uint8_t *oam;
	uint8_t *vram;
	uint8_t *ram;

	_gb_ppu_t *ppu;
	bool mbc1;
	bool mbc2;
	bool mbc3;
	bool ime; // Interrupt master enable
	bool halt_bug;
	bool service_interrupts;
	bool in_dma;

	uint8_t *romptr;

	uint64_t *backtrace;
	uint64_t backtrace_idx;
};

typedef struct gb_cpu gb_cpu_t;

void set_interrupt_flag(gb_cpu_t *cpu, uint8_t bit, uint8_t status);

void init_cpu(gb_cpu_t *cpu, _gb_ppu_t *ppu, bool load_bootstrap);


uint32_t fetch_instruction(gb_cpu_t *cpu);
void execute_instruction(gb_cpu_t *cpu, uint32_t instruction, FILE *assembly_dump);

void dma_transfer(gb_cpu_t *cpu, uint8_t addr);
void _push(gb_cpu_t *cpu, uint16_t *wreg);
uint8_t gb_read(gb_cpu_t *cpu, uint16_t address);
void print_backtrace(gb_cpu_t *cpu, int num_items);
void update_input_registers(gb_cpu_t *cpu);

#endif
