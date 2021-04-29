#include "cpu.h"
#include "utils.h"

#define INSTR_TYPE_NOTHING 0
#define INSTR_TYPE_IMM8 1
#define INSTR_TYPE_IMM16 2

static char *assembly_translation[256] = {
	"nop", "ld bc, d16", "ld (bc), a", "inc bc", "inc b", "dec b", "ld b, d8", "rlca", "ld (a16), sp", "add hl, bc", "ld a, (bc)", "dec bc", "inc c", "dec c", "ld c, d8", "rrca",
	"stop", "ld de, d16", "ld (de), a", "inc de", "inc d", "dec d", "ld d, d8", "rla", "jr r8", "add hl, de", "ld a, (de)", "dec de", "inc e", "dec e", "ld e, d8", "rra",
	"jr nz, r8", "ld hl, d16", "ld (hl+), a", "inc hl", "inc h", "dec h", "ld h, d8", "daa", "jr z, r8", "add hl, hl", "ld a, (hl+)", "dec hl", "inc l", "dec l", "ld l, d8", "cpl",
	"jr nc, r8", "ld sp, d16", "ld (hl-), a", "inc sp", "inc (hl)", "dec (hl)", "ld (hl), d8", "scf", "jr c, r8", "add hl, sp", "ld a, (hl-)", "dec sp", "inc a", "dec a", "ld a, d8", "ccf",
	"ld b, b", "ld b, c", "ld b, d", "ld b, e", "ld b, h", "ld b, l", "ld b, (hl)", "ld b, a", "ld c, b", "ld c, c", "ld c, d", "ld c, e", "ld c, h", "ld c, l", "ld c, (hl)", "ld b, a",
	"ld d, b", "ld d, c", "ld d, d", "ld d, e", "ld d, h", "ld d, l", "ld d, (hl)", "ld d, a", "ld e, b", "ld e, c", "ld e, d", "ld e, e", "ld e, h", "ld e, l", "ld e, (hl)", "ld e, a",
	"ld h, b", "ld h, c", "ld h, d", "ld h, e", "ld h, h", "ld h, l", "ld h, (hl)", "ld h, a", "ld l, b", "ld l, c", "ld l, d", "ld l, e", "ld l, h", "ld l, l", "ld l, (hl)", "ld l, a",
	"ld (hl), b", "ld (hl), c", "ld (hl), d", "ld (hl), e", "ld (hl), h", "ld (hl), l", "halt", "ld (hl), a", "ld a, b", "ld a, c", "ld a, d", "ld a, e", "ld a, h", "ld a, l", "ld a, (hl)", "ld a, a",
	"add a, b", "add a, c", "add a, d", "add a, e", "add a, h", "add a, l", "add a, (hl)", "add a, a", "adc a, b", "adc a, c", "adc a, d", "adc a, e", "adc h", "adc a, l", "adc (hl)", "adc a, a",
	"sub b", "sub c", "sub d", "sub e", "sub h", "sub l", "sub (hl)", "sub a", "sbc b", "sbc c", "sbc d", "sbc e", "sbc h", "sbc l", "sbc (hl)", "sbc a",
	"and b", "and c", "and d", "and e", "and h", "and l", "and (hl)", "and a", "xor b", "xor c", "xor d", "xor e", "xor h", "xor l", "xor (hl)", "xor a",
	"or b", "or c", "or d", "or e", "or h", "or l", "or (hl)", "or a", "cp b", "cp c", "cp d", "cp e", "cp h", "cp l", "cp (hl)", "cp a",
	"ret nz", "pop bc", "jp nz, a16", "jp a16", "call nz, a16", "push bc", "add a, d8", "rst 0x0", "ret z", "ret", "jp z, a16", "(cb prefix)", "call z, a16", "call a16", "adc a, d8", "rst 0x8",
	"ret nc", "pop de", "jp nc, a16", "(null)", "call nc, a16", "push de", ", sub d8", ", rst 0x10", "ret c", "reti", "jp c, a16", "(null)", "call c, a16", "(null)", "sbc a, d8", "rst 0x18",
	"ldh (a8), a", "pop hl", "ld (c), a", "(null)", "(null)", "push hl", "and d8", "rst 0x20", "add sp, r8", "jp (hl)", "ld (a16), a", "(null)", "(null)", "(null)", "xor d8", "rst 0x28",
	"ldh a, (a8)", "pop af", "ld a, (c)", "di", "(null)", "push af", "or d8", "rst 0x30", "ld hl, sp+r8", "ld sp, hp", "ld a, (a16)", "ei", "(null)", "(null)", "cp d8", "rst 0x38"
};
/*
static int instr_types[256] = {
	INSTR_TYPE_NOTHING, INSTR_TYPE_IMM16, INSTR_TYPE_NOTHING, INSTR_TYPENOTHING,
};
*/
static uint8_t instruction_cycle_count[256] = {
	//0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
	4, 12, 8, 8, 4, 4, 8, 4, 20, 8, 8, 8, 4, 4, 8, 4,
	4, 12, 8, 8, 4, 4, 8, 4, 12, 8, 8, 8, 4, 4, 8, 4,
	0, 12, 8, 8, 4, 4, 8, 4, 0, 8, 8, 8, 4, 4, 8, 4,
	0, 12, 8, 8, 12, 12, 12, 4, 0, 8, 8, 8, 4, 4, 8, 4,
	4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4,
	4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4,
	4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4,
	8, 8, 8, 8, 8, 8, 4, 8, 4, 4, 4, 4, 4, 4, 8, 4,
	4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4,
	4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4,
	4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4,
	4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4,
	0, 12, 0, 16, 0, 16, 8, 16, 0, 16, 0, 4, 0, 24, 8, 16,
	0, 12, 0, 0, 0, 16, 8, 16, 0, 16, 0, 0, 0, 0, 8, 16,
	12, 12, 8, 0, 0, 16, 8, 16, 16, 4, 16, 0, 0, 0, 8, 16,
	12, 12, 8, 4, 0, 16, 8, 16, 12, 8, 16, 4, 0, 0, 8, 16
};

static uint8_t instruction_byte_size[256] = {
	//0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
	1, 3, 1, 1, 1, 1, 2, 1, 3, 1, 1, 1, 1, 1, 2, 1,
	1, 3, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1,
	2, 3, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1,
	2, 3, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 3, 3, 3, 1, 2, 1, 1, 1, 3, 2, 3, 3, 2, 1,
	1, 1, 3, 0, 3, 1, 2, 1, 1, 1, 3, 0, 3, 0, 2, 1,
	2, 1, 1, 0, 0, 1, 2, 1, 2, 1, 3, 0, 0, 0, 2, 1,
	2, 1, 1, 1, 0, 1, 2, 1, 2, 1, 3, 1, 0, 0, 2, 1
};

void _halt(gb_cpu_t *cpu) {
	if (cpu->ime) {
		cpu->shouldHalt = true;
	} else {
		if ((*cpu->ie & *cpu->ifl) != 0) {
			cpu->halt_bug = true;
		} else {
			cpu->shouldHalt = true;
		}
	}
}

void print_backtrace(gb_cpu_t *cpu, int num_items)
{
	printf("items: %i\n", num_items);
	for (int i = 0; i < num_items; i++) {
		int curr_offset = cpu->backtrace_idx - num_items + i;

		// don't go beyond cpu->backtrace if requested (prevent segfaults)
		if (cpu->backtrace + curr_offset < cpu->backtrace)
			continue;

		uint64_t data = cpu->backtrace[curr_offset];
		uint32_t addr = (data & 0xffffffff00000000) >> 32;
		uint32_t opcode = data & 0xffffffff;
		printf("0x%04x: %s ; %06x\n", addr, assembly_translation[(opcode & 0xff000000) >> 24], opcode >> 8);
	}
}

void bad_instruction(gb_cpu_t *cpu, uint8_t opcode) {
	printf("a bad instruction (%02x) was executed (we shouldn't have gotten here!)\n", opcode);
	printf("backtrace:\n\n");
	print_backtrace(cpu, 30);
	exit(-1);
}

void queue_task(gb_cpu_t *cpu, uint8_t bit, uint8_t val, uint8_t cycles) {
	if (cpu->queued_tasks_data_ptr < 31) {
		cpu->queued_tasks_data_ptr++;
	} else {
		printf("queued tasks full monkaS\n");
		return;
	}
		
	struct task_data data;
	data.data = val;
	data.cycles_until_execution = cycles;
	cpu->queued_tasks_data[cpu->queued_tasks_data_ptr] = data;

	cpu->queued_tasks |= 1UL << bit;
}

static void set_flag(gb_cpu_t *cpu, uint8_t bit, uint8_t status) {
	cpu->f ^= (-status ^ cpu->f) & (1UL << bit);
}

static inline uint8_t get_flag_on(gb_cpu_t *cpu, uint8_t bit) {
	return (cpu->f >> bit) & 1;
}

static void set_bit_on(uint8_t *value, uint8_t bit, uint8_t status) {
	*value ^= (-status ^ *value) & (1UL << bit);
}

static uint8_t get_bit_on(uint8_t value, uint8_t bit) {
	return (value >> bit) & 1;
}
//9405528206335364659256
void set_interrupt_flag(gb_cpu_t *cpu, uint8_t bit, uint8_t status) {
	*cpu->ifl ^= ((-status ^ *cpu->ifl) & (1UL << bit));
}

	
uint8_t gb_read(gb_cpu_t *cpu, uint16_t address) {
	uint8_t value = 0;

	//only HRAM access allowed in DMA
	if (cpu->in_dma) {
		if (address < 0xFF80 && address > 0xFFFE)
			return 0;
	}

	switch (address & 0xF000) {
		case 0x0000:
		case 0x1000:
		case 0x2000:
		case 0x3000: {
			value = cpu->address_space[address];
			break;
		}

		case 0x4000:
		case 0x5000:
		case 0x6000:
		case 0x7000: {
			if (cpu->mbc1) {
				uint8_t offset = cpu->last_bank;
				return cpu->romptr[offset * 0x4000 + address - 0x4000];
			}
	
			value = cpu->address_space[address];
			break;
		}

		case 0x8000:
		case 0x9000: {
			value = cpu->address_space[address];
			break;
		}

		case 0xA000:
		case 0xB000: {
			value = cpu->address_space[address];
			break;
		}

		case 0xC000:
		case 0xD000: {
			value = cpu->address_space[address];
			break;
		}

		case 0xF000: {
			switch (address & 0x0F00) {
				case 0x0E00: {
					if (address < 0xFEA0) {
						value = cpu->address_space[address];
					} else {
						value = 0;
					}
					break;
				}
				case 0x0F00: {
					if (address < 0xFF7F) {
						value = cpu->address_space[address];
					} else if (address < 0xFFFF) {
						value = cpu->address_space[address];
					} else {
						value = *cpu->ie;
					}
					break;
				}
			}
			break;
		}
	}

	return value;
}

void gb_write_8(gb_cpu_t *cpu, uint16_t address, uint8_t data, uint8_t size) {
	if (cpu->in_dma) {
		if (address < 0xFF80 && address > 0xFFFE)
			return;
	}

	switch (address & 0xF000) {
		case 0x0000:
		case 0x1000: {
			// do nothing as writing to bank0 isnt allowed
			//printf("write enable!\n");
			break;
		}
		case 0x2000:
		case 0x3000: {

			//switch bank1
			if (cpu->mbc1) {
				if (data == 0) {
					cpu->last_bank = 1;
				} else if (data == 20) {
					cpu->last_bank = 21;
				} else if (data == 40) {
					cpu->last_bank = 41; 
				} else if (data == 40) {
					cpu->last_bank = 61;
				} else {
					cpu->last_bank = data;
				}
			}
			break;	
		}

		case 0x4000:
		case 0x5000: {
			break;
		}

		case 0x6000:
		case 0x7000: {
			//do nothing as writing to ROM isnt allowed
			//printf("mode write!\n");
			break;
		}

		case 0x8000:
		case 0x9000: {

			/*if (cpu->ppu->mode == 3)
				return;*/
			cpu->address_space[address] = data;
			break;
		}

		case 0xA000:
		case 0xB000: {
			cpu->address_space[address] = data;
			break;
		}

		case 0xC000:
		case 0xD000: {
			// vram
			cpu->address_space[address] = data;
			break;
		}

		case 0xF000: {
			switch (address & 0x0F00) {
				case 0x0E00: {
					// OAM
					if (address < 0xFFA0) {
						cpu->address_space[address] = data;
					}
					break;
				}
				case 0x0F00: {
					if (address < 0xFF7F) {
						switch (address) {
							case 0xFF04: {
								*cpu->div = 0;
								break;
							}
							case 0xFF41: {
								uint8_t lower = *cpu->stat;
								*cpu->stat = data & 0b11111000;
								*cpu->stat |= lower & 0b00000111;
								break;
							}
							case 0xFF42:
							case 0xFF43: {
								if (!(cpu->mode == 3))
									cpu->address_space[address] = data;
								break;
							}
							case 0xFF44: {
								break;
							}
							case 0xFF46: {
								//printf("[DEBUG]: Queueing dma transfer with base address: 0x%04x\n", data * 0x100);
								queue_task(cpu, QUEUE_DMA_TRANSFER, data, 1);
								break;
							}

							default:
								cpu->address_space[address] = data;
						}
					} else if (address < 0xFFFF) {
						cpu->address_space[address] = data;
					} else {
						*cpu->ie = data;
					}
					break;
				}
			}
			break;
		}
	}
}


inline void _push(gb_cpu_t *cpu, uint16_t *wreg) {
	cpu->sp -= 2;
	uint8_t top = (*wreg >> 8) & 0xff;
	uint8_t bottom = *wreg & 0xff;

	gb_write_8(cpu, cpu->sp, bottom, 1);
	gb_write_8(cpu, cpu->sp + 1, top, 1);
}

static inline void _pop(gb_cpu_t *cpu, uint16_t *wreg) {
	uint16_t newt = 0;
	newt = gb_read(cpu, cpu->sp + 1);
	newt <<= 8;
	newt |= gb_read(cpu, cpu->sp);
	if (wreg == cpu->af) {
		*wreg = newt & 0xFFF0;
	} else {
		*wreg = newt;
	}
	cpu->sp += 2;
}

static inline void _ret(gb_cpu_t *cpu) {
	_pop(cpu, &cpu->pc);
}

void dma_transfer(gb_cpu_t *cpu, uint8_t addr) {
	//Check for vblank
	if (cpu->ppu->mode == 1) {
		uint8_t *finalAddr = cpu->address_space + addr * 0x100;
		memcpy(cpu->address_space + 0xFE00, finalAddr, 0x9E);
		queue_task(cpu, QUEUE_DMA_FINISHED, 0, 160);
		cpu->in_dma = true;
	}
}

void init_cpu(gb_cpu_t *cpu, gb_ppu_t *ppu, bool load_bootstrap) {

	cpu->queued_tasks_data_ptr = 0;

	cpu->load_bootstrap = load_bootstrap;
	cpu->ppu = ppu;
	cpu->joyp = cpu->address_space + 0xFF00;
	cpu->sb = cpu->address_space +  0xFF01;
	cpu->sc = cpu->address_space + 0xFF02;
	cpu->div = cpu->address_space + 0xFF04;
	cpu->tima = cpu->address_space + 0xFF05;
	cpu->tma = cpu->address_space + 0xFF06;
	cpu->tac = cpu->address_space + 0xFF07;
	cpu->lcdc = cpu->address_space + 0xFF40;
	cpu->stat = cpu->address_space + 0xFF41;
	cpu->scy = cpu->address_space + 0xFF42;
	cpu->scx = cpu->address_space + 0xFF43;
	cpu->ly = cpu->address_space + 0xFF44;
	cpu->lyc = cpu->address_space + 0xFF45;
	cpu->dma = cpu->address_space + 0xFF46;
	cpu->bgp = cpu->address_space + 0xFF47;
	cpu->obp0 = cpu->address_space + 0xFF48;
	cpu->obp1 = cpu->address_space + 0xFF49;
	cpu->wy = cpu->address_space + 0xFF4A;
	cpu->wx = cpu->address_space + 0xFF4B;
	cpu->ifl = cpu->address_space + 0xFF0F;
	cpu->ie = cpu->address_space + 0xFFFF;
	cpu->oam = cpu->address_space + 0xFE00;
	cpu->vram = cpu->address_space + 0x8000;
	cpu->ram = cpu->address_space + 0xC000;
	*cpu->stat = 2;

	uint8_t *registers[] = {&cpu->a, &cpu->f, &cpu->b, &cpu->c, &cpu->d, &cpu->e, &cpu->h, &cpu->l};
	cpu->af = (uint16_t *)&cpu->f;
	cpu->bc = (uint16_t *)&cpu->c;
	cpu->de = (uint16_t *)&cpu->e;
	cpu->hl = (uint16_t *)&cpu->l;
	uint8_t *cbreg[] = {&cpu->b, &cpu->c, &cpu->d, &cpu->e, &cpu->h, &cpu->l, NULL, &cpu->a};
	for (int i = 0; i < 8; i++) {
		*registers[i] = 0;
		cpu->registers[i] = cbreg[i];
	}

	// Final values after bootstrap
	if (!cpu->load_bootstrap) {
		cpu->sp = 0xfffe;
		*cpu->af = 0x01b0;
		*cpu->bc = 0x0013;
		*cpu->de = 0x00d8;
		*cpu->hl = 0x014d;
		cpu->pc = 0x100;
	}

	cpu->last_bank = 1;
	cpu->halt_bug = false;
	cpu->service_interrupts = true;
}

uint32_t fetch_opcode(gb_cpu_t *cpu) 
{
	uint16_t loc = cpu->pc;
	uint8_t *addr = cpu->address_space;

	if (cpu->mbc1 && loc >= 0x4000 && loc < 0x8000) {
		addr = cpu->romptr + cpu->last_bank * 0x4000 + loc - 0x4000;
		loc = 0;
	}

	uint8_t opcode = addr[loc];

	uint8_t instruction_size = instruction_byte_size[opcode];
	cpu->instruction_wait_cycles = instruction_cycle_count[opcode];

	uint32_t final = opcode << 24;
	if (instruction_size > 1) {
		if (instruction_size == 3)
			final |= (addr[loc + 2] << 16 | addr[loc + 1] << 8);
		else
			final |= addr[loc + 1] << 16;
	}

	if (!cpu->halt_bug)
		cpu->pc += instruction_size;
	else
		cpu->halt_bug = false;

	return final;
}


static void execute_prefix_instruction(gb_cpu_t *cpu, uint8_t opcode);
static void execute_main_instruction(gb_cpu_t *cpu, uint32_t instruction);

void execute_instruction(gb_cpu_t *cpu, uint32_t instruction, FILE *assembly_dump) {
	#ifdef DEBUG
	fprintf(assembly_dump, "pc: 0x%x, sp:%04x, instruction(0x%06x): %s\naf: 0x%04x, bc: 0x%04x, de: 0x%04x, hl: 0x%04x, ime: 0x%02x, if: 0x%02x, ie: 0x%02x\n\n", cpu->pc, cpu->sp, instruction >> 8, assembly_translation[(instruction & 0xff000000) >> 24], *cpu->af, *cpu->bc, *cpu->de, *cpu->hl, cpu->ime, *cpu->ifl, *cpu->ie);
	#endif
	if ((instruction & 0xff000000) == 0xcb000000)
		execute_prefix_instruction(cpu, (instruction & 0x00ff0000) >> 16);
	else
		execute_main_instruction(cpu, instruction);
}

/*
big thanks to this

https://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
*/
/*
__attribute__((fastcall))
static void (*opcode_table[256])(gb_cpu_t *, uint32_t) = {
	NULL, ld_bc_imm16,
};

static void ld_bc_imm16(gb_cpu_t *cpu, uint32_t instruction) {
	uint16_t immw = ((instruction & 0x00ffff00) >> 8);
	*cpu->bc = immw;
}*/

static void execute_main_instruction(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t n1 = (instruction & 0xf0000000) >> 28;
	uint8_t n2 = (instruction & 0x0f000000) >> 24;

	switch (n1) {
		case 0x0: {
			switch (n2) {
				case 0x0: {
					// nop
					break;
				}
				case 0x1: {
					//confirmed good
					//*cpu->bc = immw;
					uint16_t immw = ((instruction & 0x00ffff00) >> 8);
					*cpu->bc = immw;
					break;
				}
				case 0x2: {
					//confirmed good
					//queue_task
					//attempt_write(cpu, cpu->address_space +  *cpu->bc, &cpu->a);
					gb_write_8(cpu, *cpu->bc, cpu->a, 1);
					break;
				}
				case 0x3: {
					//confirmed good
					(*cpu->bc)++;
					
					break;
				}
				case 0x4: {
					set_flag(cpu, H_FLAG, (cpu->b++ & 0xf) == 0xf);
					
					set_flag(cpu, N_FLAG, 0);
					set_flag(cpu, Z_FLAG, cpu->b == 0);
					break;
				}
				case 0x5: {
					set_flag(cpu, H_FLAG, (--cpu->b & 0xf) == 0xf);
	
					set_flag(cpu, N_FLAG, 1);
					set_flag(cpu, Z_FLAG, cpu->b == 0);
					break;
				}
				case 0x6: {
					uint8_t immb = ((instruction & 0x00ff0000) >> 16);
					cpu->b = immb;
					break;
				}
				case 0x7: {
					//confirmed good
					bool bit = get_bit_on(cpu->a, 7);
					set_flag(cpu, C_FLAG, bit);
					set_flag(cpu, N_FLAG, 0);
					set_flag(cpu, H_FLAG, 0);
					cpu->a <<= 1;
					cpu->a |= bit;
					set_flag(cpu, Z_FLAG, 0);
					break;
				}
				case 0x8: {
					//confirmed good (possible endianness)
					uint8_t b1 = (cpu->sp >> 8) & 0xff;
					uint8_t b2 = cpu->sp & 0xff;

					uint16_t immw = ((instruction & 0x00ffff00) >> 8);

					gb_write_8(cpu, immw + 1, b1, 1);
					gb_write_8(cpu, immw, b2, 1);
					break;
				}
				case 0x9: {
					//probably good
					uint32_t val = (*cpu->hl + *cpu->bc);
					set_flag(cpu, C_FLAG, (val & 0x10000) == 0x10000);
					set_flag(cpu, H_FLAG, (((*cpu->hl & 0xfff) + (*cpu->bc & 0xfff)) & 0x1000) == 0x1000);

					*cpu->hl = val & 0xffff;

					set_flag(cpu, N_FLAG, 0);
					set_flag(cpu, N_FLAG, 0);
					break;
				}
				case 0xA: {
					//confirmed good
					//cpu->a = cpu->addressSpace[*cpu->bc];
					cpu->a = gb_read(cpu, *cpu->bc);

					break;
				}
				case 0xB: {
					//confirmed good
					(*cpu->bc)--;
					break;
				}
				case 0xC: {
					//confirmed good (possible h flag)
					set_flag(cpu, H_FLAG, (cpu->c++ & 0xf) == 0xf);

					set_flag(cpu, N_FLAG, 0);
					set_flag(cpu, Z_FLAG, cpu->c == 0);
					break;
				}
				case 0xD: {
					//confirmed good (possible h flag)
					set_flag(cpu, H_FLAG, (--cpu->c & 0xf) == 0xf);

					set_flag(cpu, N_FLAG, 1);
					set_flag(cpu, Z_FLAG, cpu->c == 0);
					break;
				}
				case 0xE: {
					uint8_t immb = ((instruction & 0x00ff0000) >> 16);
					cpu->c = immb;
					break;
				}
				case 0xF: {
					//confirmed good
					bool bit = get_bit_on(cpu->a, 0);
					set_flag(cpu, C_FLAG, bit);
					set_flag(cpu, H_FLAG, 0);
					set_flag(cpu, N_FLAG, 0);
					cpu->a >>= 1;
					cpu->a |= bit << 7;
					set_flag(cpu, Z_FLAG, 0);
					break;
					
				}
			}
			break;
		}
		case 0x1: {
			switch (n2) {
				case 0x0: {
					//confirmed good
					cpu->mode = 2;
					break;
				}
				case 0x1: {
					//confirmed good
					uint16_t immw = ((instruction & 0x00ffff00) >> 8);
					*cpu->de = immw;
					break;
				}
				case 0x2: {
					//confirmed good

					gb_write_8(cpu, *cpu->de, cpu->a, 1);
					break;
				}
				case 0x3: {
					//confirmed good
					(*cpu->de)++;
					break;
				}
				case 0x4: {
					//confirmed good (possible h flag)
					if ((cpu->d++ & 0xf) == 0xf)
						set_flag(cpu, H_FLAG, 1);
					else
						set_flag(cpu, H_FLAG, 0);
					
					set_flag(cpu, N_FLAG, 0);
					set_flag(cpu, Z_FLAG, cpu->d == 0);
					break;
				}
				case 0x5: {
					//confirmed good (possible h flag)
					if ((--cpu->d & 0xf) == 0xf)
						set_flag(cpu, H_FLAG, 1);
					else
						set_flag(cpu, H_FLAG, 0);
					
					set_flag(cpu, N_FLAG, 1);
					set_flag(cpu, Z_FLAG, cpu->d == 0);
					break;
				}
				case 0x6: {
					//confirmed good
					uint8_t immb = ((instruction & 0x00ff0000) >> 16);
					cpu->d = immb;
					break;
				}
				case 0x7: {
					//confirmed good
					bool bit = get_bit_on(cpu->a, 7);
					bool newBit = get_flag_on(cpu, C_FLAG);
					set_flag(cpu, C_FLAG, bit);
					set_flag(cpu, N_FLAG, 0);
					set_flag(cpu, H_FLAG, 0);
					cpu->a <<= 1;
					cpu->a |= newBit;
					set_flag(cpu, Z_FLAG, 0);
					break;
				}
				case 0x8: {
					//confirmed good
					int8_t simmb = ((instruction & 0x00ff0000) >> 16);
					cpu->pc += simmb;
					break;
				}
				case 0x9: {
					//confirmed good (possible h flag)
					uint32_t val = (*cpu->hl + *cpu->de);
					set_flag(cpu, C_FLAG, (val & 0x10000) == 0x10000);
					set_flag(cpu, H_FLAG, (((*cpu->hl & 0xfff) + (*cpu->de & 0xfff)) & 0x1000) == 0x1000);

					*cpu->hl = val & 0xffff;

					set_flag(cpu, N_FLAG, 0);
					break;
				}
				case 0xA: {
					//confirmed good
					cpu->a = gb_read(cpu, *cpu->de);//*(cpu->address_space +  *cpu->de);
					break;
				}
				case 0xB: {
					//confirmed good
					(*cpu->de)--;
					break;
				}
				case 0xC: {
					//confirmed good (possible h flag)
					if ((cpu->e++ & 0xf) == 0xf)
						set_flag(cpu, H_FLAG, 1);
					else
						set_flag(cpu, H_FLAG, 0);
					
					set_flag(cpu, N_FLAG, 0);
					set_flag(cpu, Z_FLAG, cpu->e == 0);
					
					break;
				}
				case 0xD: {
					//confirmed good (possible h flag)
					if ((--cpu->e & 0xf) == 0xf)
						set_flag(cpu, H_FLAG, 1);
					else
						set_flag(cpu, H_FLAG, 0);
					
					set_flag(cpu, N_FLAG, 1);
					set_flag(cpu, Z_FLAG, cpu->e == 0);
					break;
				}
				case 0xE: {
					//confirmed good
					uint8_t immb = ((instruction & 0x00ff0000) >> 16);
					cpu->e = immb;
					break;
				}
				case 0xF: {
					//confirmed good
					bool bit = get_flag_on(cpu, C_FLAG);
					set_flag(cpu, C_FLAG, get_bit_on(cpu->a, 0));
					set_flag(cpu, H_FLAG, 0);
					set_flag(cpu, N_FLAG, 0);
					cpu->a >>= 1;
					cpu->a |= bit << 7;
					set_flag(cpu, Z_FLAG, 0);
				}
			}
			break;
		}
		case 0x2: {
			switch (n2) {
				case 0x0: {
					//confirmed good
					if (!get_flag_on(cpu, Z_FLAG)) {
						//printf("simm1 is %i\n", simm1);
						//exit(-1);
						int8_t simmb = ((instruction & 0x00ff0000) >> 16);
						cpu->pc += simmb;
						cpu->instruction_wait_cycles = 12;
					} else {
						cpu->instruction_wait_cycles = 8;
					}

					break;
				}
				case 0x1: {
					//confirmed good
					uint16_t immw = ((instruction & 0x00ffff00) >> 8);
					*cpu->hl = immw;
					break;
				}
				case 0x2: {
					//confirmed good
					gb_write_8(cpu, *cpu->hl, cpu->a, 1);
					(*cpu->hl)++;
					break;
				}
				case 0x3: {
					//confirmed good
					(*cpu->hl)++;
					break;
				}
				case 0x4: {
					//confirmed good (h)
					if ((cpu->h++ & 0xf) == 0xf)
						set_flag(cpu, H_FLAG, 1);
					else
						set_flag(cpu, H_FLAG, 0);
					
					set_flag(cpu, N_FLAG, 0);
					set_flag(cpu, Z_FLAG, cpu->h == 0);
					break;
				}
				case 0x5: {
					//confirmed good (h)
					if ((--cpu->h & 0xf) == 0xf)
						set_flag(cpu, H_FLAG, 1);
					else
						set_flag(cpu, H_FLAG, 0);
					
					set_flag(cpu, N_FLAG, 1);
					set_flag(cpu, Z_FLAG, cpu->h == 0);
					break;
				}
				case 0x6: {
					//confirmed good
					uint8_t immb = ((instruction & 0x00ff0000) >> 16);
					cpu->h = immb;
					break;
				}
				case 0x7: {
					if (get_flag_on(cpu, N_FLAG)) {
						// subtraction
						if (get_flag_on(cpu, C_FLAG)) {
							cpu->a -= 0x60;
						}
						if (get_flag_on(cpu, H_FLAG)) {
							cpu->a -= 0x6;
						}
					} else {
						// addition
						if (get_flag_on(cpu, C_FLAG) || cpu->a > 0x99) {

							cpu->a += 0x60;
							set_flag(cpu, C_FLAG, 1);
						}
						if (get_flag_on(cpu, H_FLAG) || (cpu->a & 0xf) > 0x9) {
							cpu->a += 0x6;
						}
					}
					set_flag(cpu, Z_FLAG, cpu->a == 0);
					set_flag(cpu, H_FLAG, 0);
					break;
				}
				case 0x8: {
					//confirmed good
					if (get_flag_on(cpu, Z_FLAG)) {
						int8_t simmb = ((instruction & 0x00ff0000) >> 16);
						cpu->pc += simmb;
						cpu->instruction_wait_cycles = 12;
					} else {
						cpu->instruction_wait_cycles = 8;
					}
						
					break;
				}
				case 0x9: {
					//confirmed good probably (flags)
					uint32_t val = (*cpu->hl + *cpu->hl);
					set_flag(cpu, C_FLAG, (val & 0x10000) == 0x10000);
					set_flag(cpu, H_FLAG, (((*cpu->hl & 0xfff) + (*cpu->hl & 0xfff)) & 0x1000) == 0x1000);

					*cpu->hl = val & 0xffff;

					set_flag(cpu, N_FLAG, 0);
					break;
				}//0000 1000 0000 0000
				case 0xA: {
					//confirmed good
					cpu->a = gb_read(cpu, *cpu->hl);//cpu->address_space[*cpu->hl];
					(*cpu->hl)++;
					break;
				}
				case 0xB: {
					//confirmed good
					(*cpu->hl)--;
					break;
				}
				case 0xC: {
					//confirmed good (h)
					if ((cpu->l++ & 0xf) == 0xf)
						set_flag(cpu, H_FLAG, 1);
					else
						set_flag(cpu, H_FLAG, 0);
					
					set_flag(cpu, N_FLAG, 0);
					set_flag(cpu, Z_FLAG, cpu->l == 0);
					break;
				}
				case 0xD: {
					//confirmed good (h)
					if ((--cpu->l & 0xf) == 0xf)
						set_flag(cpu, H_FLAG, 1);
					else
						set_flag(cpu, H_FLAG, 0);
					
					set_flag(cpu, N_FLAG, 1);
					set_flag(cpu, Z_FLAG, cpu->l == 0);
					break;
				}
				case 0xE: {
					//confirmed good
					uint8_t immb = ((instruction & 0x00ff0000) >> 16);
					cpu->l = immb;
					break;
				}
				case 0xF: {
					//confirmed good
					cpu->a = ~cpu->a;
					set_flag(cpu, N_FLAG, 1);
					set_flag(cpu, H_FLAG, 1);
					break;
				}
			}
			break;
		}
		case 0x3: {
			switch (n2) {
				case 0x0: {
					//confirmed good
					if (!get_flag_on(cpu, C_FLAG)) {
						int8_t simmb = ((instruction & 0x00ff0000) >> 16);
						cpu->pc += simmb;
						cpu->instruction_wait_cycles = 12;
					} else {
						cpu->instruction_wait_cycles = 8;
					}

					break;
				}
				case 0x1: {
					//confirmed good
					uint16_t immw = ((instruction & 0x00ffff00) >> 8);
					cpu->sp = immw;
					break;
				}
				case 0x2: {
					//confirmed good
					//attempt_write(cpu, cpu->address_space +  *cpu->hl, &cpu->a);
					gb_write_8(cpu, *cpu->hl, cpu->a, 1);
					(*cpu->hl)--;
					break;
				}
				case 0x3: {
					//confirmed good
					cpu->sp++;
					break;
				}
				case 0x4: {
					//confirmed good (h)
					if ((( cpu->address_space[*cpu->hl] )++ & 0xf) == 0xf)
						set_flag(cpu, H_FLAG, 1);
					else
						set_flag(cpu, H_FLAG, 0);

					set_flag(cpu, N_FLAG, 0);
					set_flag(cpu, Z_FLAG, gb_read(cpu, *cpu->hl) == 0);
					break;
				}
				case 0x5: {
					//confirmed good (h)
					if ((--(cpu->address_space[*cpu->hl]) & 0xf) == 0xf)
						set_flag(cpu, H_FLAG, 1);
					else
						set_flag(cpu, H_FLAG, 0);

					set_flag(cpu, N_FLAG, 1);
					set_flag(cpu, Z_FLAG, gb_read(cpu, *cpu->hl) == 0);
					break;
				}
				case 0x6: {
					//confirmed good
					uint8_t immb = ((instruction & 0x00ff0000) >> 16);
					gb_write_8(cpu, *cpu->hl, immb, 1);
					break;
				}
				case 0x7: {
					//confirmed good
					set_flag(cpu, C_FLAG, 1);
					set_flag(cpu, N_FLAG, 0);
					set_flag(cpu, H_FLAG, 0);
					break;
				}
				case 0x8: {
					//confirmed good
					if (get_flag_on(cpu, C_FLAG)) {
						int8_t simmb = ((instruction & 0x00ff0000) >> 16);
						cpu->pc += simmb;
						cpu->instruction_wait_cycles = 12;
					} else {
						cpu->instruction_wait_cycles = 8;
					}
						
					break;
				}
				case 0x9: {
					//confirmed good (flags maybe)
					uint32_t val = (*cpu->hl + cpu->sp);
					set_flag(cpu, C_FLAG, (val & 0x10000) == 0x10000);
					set_flag(cpu, H_FLAG, (((*cpu->hl & 0xfff) + (cpu->sp & 0xfff)) & 0x1000) == 0x1000);

					*cpu->hl = val & 0xffff;

					set_flag(cpu, N_FLAG, 0);
					break;
				}
				case 0xA: {
					//confirmed good
					cpu->a = gb_read(cpu, *cpu->hl);//cpu->addressSpace[*cpu->hl];
					(*cpu->hl)--;
					break;
				}
				case 0xB: {
					//confirmed good
					cpu->sp--;

					break;
				}
				case 0xC: {
					//confirmed good (flags)
					if ((cpu->a++ & 0xf) == 0xf)
						set_flag(cpu, H_FLAG, 1);
					else
						set_flag(cpu, H_FLAG, 0);

					set_flag(cpu, N_FLAG, 0);
					set_flag(cpu, Z_FLAG, cpu->a == 0);
					break;
				}
				case 0xD: {
					//confirmed good (FLAGS)
					if ((--cpu->a & 0xf) == 0xf)
						set_flag(cpu, H_FLAG, 1);
					else
						set_flag(cpu, H_FLAG, 0);

					set_flag(cpu, N_FLAG, 1);
					set_flag(cpu, Z_FLAG, cpu->a == 0);
					break;
				}
				case 0xE: {
					//confirmed good
					uint8_t immb = ((instruction & 0x00ff0000) >> 16);
					cpu->a = immb;
					break;
				}
				case 0xF: {
					//confirmed good
					set_flag(cpu, C_FLAG, !get_flag_on(cpu, C_FLAG));
					set_flag(cpu, N_FLAG, 0);
					set_flag(cpu, H_FLAG, 0);
					break;
				}
			}
			break;
		}
		case 0x4:
		case 0x5:
		case 0x6:
		case 0x7: {
			//confirmed good
			if (n1 == 0x7 && n2 == 0x6) {
				_halt(cpu);
				break;
			}
			uint8_t reg1index = (n1 - 0x4) * 2;
			if (n2 > 0x7)
				reg1index++;

			uint8_t *reg1, *reg2;
			if (reg1index == 6) {
				reg1 = NULL;
			} else {
				reg1 = cpu->registers[reg1index];
			}

			uint8_t reg2index = n2 % 8;
			if (reg2index == 6) {
				reg2 = NULL;
			} else {
				reg2 = cpu->registers[reg2index];
			}

			if (reg1index == 6) {
				gb_write_8(cpu, *cpu->hl, *reg2, 1);
				break;
			}
			if (reg2index == 6) {
				*reg1 = gb_read(cpu, *cpu->hl);
				break;
			}
			*reg1 = *reg2;

			break;
		}
		
		case 0x8: {
			//confirmed good possibly flags
			uint8_t *reg = cpu->registers[n2 % 8];
			
			if (n2 < 0x8) {
				uint16_t val;
				if (n2 % 8 == 6) {
					uint8_t memVal = gb_read(cpu, *cpu->hl);
					val = cpu->a + memVal;
					set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
					set_flag(cpu, H_FLAG, (((cpu->a & 0xf) + (memVal & 0xf)) & 0x10) == 0x10);
				} else {
					uint8_t memVal = *reg;
					val = cpu->a + memVal;
					set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
					set_flag(cpu, H_FLAG, (((cpu->a & 0xf) + (memVal & 0xf)) & 0x10) == 0x10);
				}
				cpu->a = val & 0xff;
			} else {
				uint16_t val;
				uint8_t memVal;
				if (n2 % 8 == 6) {
					memVal = gb_read(cpu, *cpu->hl);
				} else {
					memVal = *reg;
				}
				val = cpu->a + memVal + get_flag_on(cpu, C_FLAG);
				uint8_t low_nibble_a = cpu->a & 0xf;
				uint8_t low_nibble_nc = memVal & 0xf;
				set_flag(cpu, H_FLAG, (((low_nibble_a + low_nibble_nc + get_flag_on(cpu, C_FLAG)) & 0x10)) == 0x10);
				set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
				cpu->a = val & 0xff;
			}
			set_flag(cpu, N_FLAG, 0);
			set_flag(cpu, Z_FLAG, cpu->a == 0);
			break;
		}

		case 0x9: {

			//confirmed good possibly flags
			uint8_t *reg = cpu->registers[n2 % 8];
			
			if (n2 < 0x8) {
				uint16_t val;
				if (n2 % 8 == 6) {
					uint8_t memVal = gb_read(cpu, *cpu->hl);
					val = cpu->a - memVal;
					set_flag(cpu, C_FLAG, cpu->a < memVal);
					set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (memVal & 0xf)) < 0);
				} else {
					val = cpu->a - *reg;
					set_flag(cpu, C_FLAG, cpu->a < *reg);
					set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (*reg & 0xf)) < 0);
				}
				cpu->a = val & 0xff;
			} else {
				uint16_t nc;
				if (n2 % 8 == 6) {
					nc = gb_read(cpu, *cpu->hl) + get_flag_on(cpu, C_FLAG);
				} else {
					nc = *reg + get_flag_on(cpu, C_FLAG);
				}

				uint16_t val = cpu->a - nc;
				set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (val & 0xf) - get_flag_on(cpu, C_FLAG)) < 0);
				set_flag(cpu, C_FLAG, cpu->a < nc);
				cpu->a = val & 0xff;
			}

			set_flag(cpu, N_FLAG, 1);
			set_flag(cpu, Z_FLAG, cpu->a == 0);
			break;
		}
		case 0xA: {
			//confirmed good
			uint8_t *reg = cpu->registers[n2 % 8];
			
			if (n2 < 0x8) {
				set_flag(cpu, H_FLAG, 1);
				if (n2 % 8 == 0x6) {
					cpu->a &= gb_read(cpu, *cpu->hl);
				} else {
					cpu->a &= *reg;
				}
			} else {
				set_flag(cpu, H_FLAG, 0);
				if (n2 % 8 == 0x6) {
					cpu->a ^= gb_read(cpu, *cpu->hl);
				} else {
					cpu->a ^= *reg;
				}
			}
			
			set_flag(cpu, Z_FLAG, cpu->a == 0);
			set_flag(cpu, N_FLAG, 0);
			set_flag(cpu, C_FLAG, 0);
			break;
		}
		case 0xB: {
			//confirmed good
			uint8_t *reg = cpu->registers[n2 % 8];
			
			if (n2 < 0x8) {
				set_flag(cpu, N_FLAG, 0);
				set_flag(cpu, H_FLAG, 0);
				set_flag(cpu, C_FLAG, 0);

				uint8_t val;
				if (n2 % 8 == 6)
					val = gb_read(cpu, *cpu->hl);
				else
					val = *reg;

				cpu->a |= val;
				set_flag(cpu, Z_FLAG, cpu->a == 0);
			} else {
				uint8_t val;
				if (n2 % 8 == 6)
					val = gb_read(cpu, *cpu->hl);
				else
					val = *reg;

				if ((cpu->a & 0xf) < (val & 0xf))
					set_flag(cpu, H_FLAG, 1);
				else
					set_flag(cpu, H_FLAG, 0);

				if (cpu->a < val)
					set_flag(cpu, C_FLAG, 1);
				else
					set_flag(cpu, C_FLAG, 0);

				set_flag(cpu, Z_FLAG, cpu->a == val);
				set_flag(cpu, N_FLAG, 1);
			}
			break;
		}
		case 0xC: {
			
			switch (n2) {
				case 0x0: {
					//confirmed good
					if (!get_flag_on(cpu, Z_FLAG)) {
						_ret(cpu);
						cpu->instruction_wait_cycles = 20;
					} else {
						cpu->instruction_wait_cycles = 8;
					}

					break;
				}
				case 0x1: {
					//confirmed good
					_pop(cpu, cpu->bc);
					break;
				}
				case 0x2: {
					//confirmed good
					if (!get_flag_on(cpu, Z_FLAG)) {
						uint16_t immw = ((instruction & 0x00ffff00) >> 8);
						cpu->pc = immw;
						cpu->instruction_wait_cycles = 16;
					} else {
						cpu->instruction_wait_cycles = 12;
					}
						
					break;
				}
				case 0x3: {
					//confirmed good
					uint16_t immw = ((instruction & 0x00ffff00) >> 8);
					cpu->pc = immw;
					break;
				}
				case 0x4: {
					//confirmed good
					if (!get_flag_on(cpu, Z_FLAG)) {
						_push(cpu, &cpu->pc);
						uint16_t immw = ((instruction & 0x00ffff00) >> 8);
						cpu->pc = immw;
						cpu->instruction_wait_cycles = 24;
					} else {
						cpu->instruction_wait_cycles = 12;
					}

					break;
				}
				case 0x5: {
					//confirmed good
					_push(cpu, cpu->bc);
					break;
				}
				case 0x6: {
					//confirmed good (possible flags)
					//uint16_t val = cpu->a + imm1;
					uint8_t immb = ((instruction & 0x00ff0000) >> 16);
					uint16_t val = cpu->a + immb;
					set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
					set_flag(cpu, H_FLAG, (((cpu->a & 0xf) + (immb & 0xf)) & 0x10) == 0x10);
					cpu->a = val & 0xff;
					set_flag(cpu, N_FLAG, 0);
					set_flag(cpu, Z_FLAG, cpu->a == 0);
					break;
				}
				case 0x7: {
					//confirmed good
					_push(cpu, &cpu->pc);
					cpu->pc = 0;
					break;
				}
				case 0x8: {
					//confirmed good
					if (get_flag_on(cpu, Z_FLAG)) {
						_ret(cpu);
						cpu->instruction_wait_cycles = 20;
					} else {
						cpu->instruction_wait_cycles = 8;
					}
					break;
				}
				case 0x9: {
					//confirmed good
					_ret(cpu);
					break;
				}
				case 0xA: {
					//confirmed good
					if (get_flag_on(cpu, Z_FLAG)) {
						uint16_t immw = ((instruction & 0x00ffff00) >> 8);
						cpu->pc = immw;
						cpu->instruction_wait_cycles = 16;
					} else {
						cpu->instruction_wait_cycles = 12;
					}
						
					break;
				}
				case 0xB: {
					// cb, should never get here
					printf("we shouldn't have gotten here (CB)\n");
					break;
				}
				case 0xC: {
					//confirmed good
					if (get_flag_on(cpu, Z_FLAG)) {
						_push(cpu, &cpu->pc);
						uint16_t immw = ((instruction & 0x00ffff00) >> 8);
						cpu->pc = immw;
						cpu->instruction_wait_cycles = 24;
					} else {
						cpu->instruction_wait_cycles = 12;
					}

					break;
				}
				case 0xD: {
					//confirmed good
					_push(cpu, &cpu->pc);
					uint16_t immw = ((instruction & 0x00ffff00) >> 8);
					cpu->pc = immw;
					break;
				}
				case 0xE: {
					uint8_t immb = ((instruction & 0x00ff0000) >> 16);

					uint16_t val = cpu->a + immb + get_flag_on(cpu, C_FLAG);
					uint8_t low_nibble_a = cpu->a & 0xf;
					uint8_t low_nibble_nc = immb & 0xf;
					set_flag(cpu, H_FLAG, (((low_nibble_a + low_nibble_nc + get_flag_on(cpu, C_FLAG)) & 0x10)) == 0x10);
					set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
					cpu->a = val & 0xff;
					set_flag(cpu, Z_FLAG, cpu->a == 0);
					set_flag(cpu, N_FLAG, 0);
					break;
				}
				case 0xF: {
					//confirmed good
					_push(cpu, &cpu->pc);
					cpu->pc = 0x8;
					break;
				}
			}
			break;
		}
		case 0xD: {
			switch (n2) {
				case 0x0: {
					//confirmed good
					if (!get_flag_on(cpu, C_FLAG)) {
						_ret(cpu);
						cpu->instruction_wait_cycles = 20;
					} else {
						cpu->instruction_wait_cycles = 8;
					}
					break;
				}
				case 0x1: {
					//confirmed good
					_pop(cpu, cpu->de);
					break;
				}
				case 0x2: {
					//confirmed good
					if (!get_flag_on(cpu, C_FLAG)) {
						uint16_t immw = ((instruction & 0x00ffff00) >> 8);
						cpu->pc = immw;
						cpu->instruction_wait_cycles = 16;
					} else {
						cpu->instruction_wait_cycles = 12;
					}

					break;
				}
				case 0x3: {
					// nothing
					bad_instruction(cpu, instruction >> 24);
					break;
				}
				case 0x4: {
					//confirmed good
					if (!get_flag_on(cpu, C_FLAG)) {
						_push(cpu, &cpu->pc);
						uint16_t immw = ((instruction & 0x00ffff00) >> 8);
						cpu->pc = immw;
						cpu->instruction_wait_cycles = 24;
					} else {
						cpu->instruction_wait_cycles = 12;
					}
					break;
				}
				case 0x5: {
					//confirmed good
					_push(cpu, cpu->de);
					break;
				}
				case 0x6: {
					//confirmed good (flags)
					uint8_t immb = ((instruction & 0x00ff0000) >> 16);
					uint16_t val = cpu->a - immb;

					set_flag(cpu, C_FLAG, cpu->a < immb);
					set_flag(cpu, H_FLAG, (((cpu->a & 0xf) - (immb & 0xf)) & 0x10) == 0x10);

					cpu->a = val;

					set_flag(cpu, N_FLAG, 1);
					set_flag(cpu, Z_FLAG, cpu->a == 0);
					break;
				}
				case 0x7: {
					//confirmed good
					_push(cpu, &cpu->pc);
					cpu->pc = 0x10;
					break;
				}
				case 0x8: {
					//confirmed good
					if (get_flag_on(cpu, C_FLAG)) {
						_ret(cpu);
						cpu->instruction_wait_cycles = 20;
					} else {
						cpu->instruction_wait_cycles = 8;
					}

					break;
				}
				case 0x9: {
					//confirmed good
					cpu->ime = true;
					_ret(cpu);
					break;
				}
				case 0xA: {
					//confirmed good
					if (get_flag_on(cpu, C_FLAG)) {
						uint16_t immw = ((instruction & 0x00ffff00) >> 8);
						cpu->pc = immw;
						cpu->instruction_wait_cycles = 16;
					} else {
						cpu->instruction_wait_cycles = 12;
					}
					break;
				}
				case 0xB: {
					// nothing
					break;
				}
				case 0xC: {
					//confirmed good
					if (get_flag_on(cpu, C_FLAG)) {
						_push(cpu, &cpu->pc);
						uint16_t immw = ((instruction & 0x00ffff00) >> 8);
						cpu->pc = immw;
						cpu->instruction_wait_cycles = 24;
					} else {
						cpu->instruction_wait_cycles = 12;
					}

					break;
				}
				case 0xD: {
					// nothing
					bad_instruction(cpu, instruction >> 24);
					break;
				}
				case 0xE: {
				//confirmed good (flags)
					uint8_t immb = ((instruction & 0x00ff0000) >> 16);
					uint16_t nc = immb + get_flag_on(cpu, C_FLAG);
					uint16_t val = cpu->a - nc;
					set_flag(cpu, H_FLAG, (((cpu->a & 0xf) - (immb & 0xf) - get_flag_on(cpu, C_FLAG)) & 0x10) == 0x10);
					set_flag(cpu, C_FLAG, cpu->a < nc);
					cpu->a = val & 0xff;

					set_flag(cpu, Z_FLAG, cpu->a == 0);
					set_flag(cpu, N_FLAG, 1);
					break;
				}
				case 0xF: {
					//confirmed good
					_push(cpu, &cpu->pc);
					cpu->pc = 0x18;
					break;
				}
			}
			break;
		}
		case 0xE: {
			switch (n2) {
				case 0x0: {
					//confirmed good
					//attempt_write(cpu, cpu->address_space +  0xFF00 + imm1, &cpu->a);
					uint8_t immb = ((instruction & 0x00ff0000) >> 16);
					gb_write_8(cpu, 0xFF00 + immb, cpu->a, 1);
					break;
				}
				case 0x1: {
					//confirmed good
					_pop(cpu, cpu->hl);
					break;
				}
				case 0x2: {
					//confirmed good
					//attempt_write(cpu, cpu->address_space +  0xFF00 + cpu->c, &cpu->a);
					gb_write_8(cpu, 0xFF00 + cpu->c, cpu->a, 1);
					break;
				}
				case 0x3: {
					// nothing
					bad_instruction(cpu, instruction >> 24);
					break;
				}
				case 0x4: {
					// nothing
					bad_instruction(cpu, instruction >> 24);
					break;
				}
				case 0x5: {
					//confirmed good
					_push(cpu, cpu->hl);
					break;
				}

				case 0x6: {
					// and d8
					//exit(-1);
					uint8_t immb = ((instruction & 0x00ff0000) >> 16);

					set_flag(cpu, H_FLAG, 1);
					set_flag(cpu, N_FLAG, 0);
					set_flag(cpu, C_FLAG, 0);
					cpu->a &= immb;
					set_flag(cpu, Z_FLAG, cpu->a == 0);
					break;
				}
				case 0x7: {
					_push(cpu, &cpu->pc);
					cpu->pc = 0x20;
					break;
				}
				case 0x8: {
					//confirmed good (flags)
					int8_t simmb = ((instruction & 0x00ff0000) >> 16);
					uint32_t val = cpu->sp + simmb;
					if (simmb > 0) {
						set_flag(cpu, C_FLAG, (((cpu->sp & 0xff) + simmb) & 0x100) == 0x100);
						set_flag(cpu, H_FLAG, (((cpu->sp & 0xf) + (simmb & 0xf)) & 0x10) == 0x10); 
					} else {
						set_flag(cpu, C_FLAG, (val & 0xff) < (cpu->sp & 0xff));
						set_flag(cpu, H_FLAG, (val & 0xf) < (cpu->sp & 0xf));
					}

					cpu->sp = val & 0xffff;
					set_flag(cpu, Z_FLAG, 0);
					set_flag(cpu, N_FLAG, 0);
					break;
				}
				case 0x9: {
					//confirmed good
					cpu->pc = *cpu->hl;
					break;
				}
				case 0xA: {
					//confirmed good
					//attempt_write(cpu, (uint8_t *)cpu->address_space +  immw, &cpu->a);
					uint16_t immw = ((instruction & 0x00ffff00) >> 8);
					gb_write_8(cpu, immw, cpu->a, 1);
					break;
				}
				case 0xB: {
					// nothing
					bad_instruction(cpu, instruction >> 24);
					break;
				}
				case 0xC: {
					// nothing
					bad_instruction(cpu, instruction >> 24);
					break;
				}
				case 0xD: {
					// nothing
					bad_instruction(cpu, instruction >> 24);
					break;
				}
				case 0xE: {
					//confirmed good
					set_flag(cpu, N_FLAG, 0);
					set_flag(cpu, H_FLAG, 0);
					set_flag(cpu, C_FLAG, 0);

					uint8_t immb = ((instruction & 0x00ff0000) >> 16);
					cpu->a ^= immb;
					set_flag(cpu, Z_FLAG, cpu->a == 0);
					break;
				}
				case 0xF: {
					//confirmed good
					_push(cpu, &cpu->pc);
					cpu->pc = 0x28;
					break;
				}
			}
			break;
		}
		case 0xF: {
			switch (n2) {
				case 0x0: {
					//confirmed good
					uint8_t immb = ((instruction & 0x00ff0000) >> 16);
					cpu->a = gb_read(cpu, 0xFF00 + immb);
					break;

				}
				case 0x1: {
					//confirmed good
					_pop(cpu, cpu->af);
					break;
				}
				case 0x2: {
					cpu->a = gb_read(cpu, 0xFF00 + cpu->c);
					break;
				}
				case 0x3: {
					//confirmed good
					cpu->ime = false;
					break;
				}
				case 0x4: {
					// nothing
					bad_instruction(cpu, instruction >> 24);
					break;
				}
				case 0x5: {
					//confirmed good
					_push(cpu, cpu->af);
					break;
				}
				case 0x6: {
					// or d8
					set_flag(cpu, N_FLAG, 0);
					set_flag(cpu, H_FLAG, 0);
					set_flag(cpu, C_FLAG, 0);
					uint8_t immb = ((instruction & 0x00ff0000) >> 16);
					cpu->a |= immb;
					set_flag(cpu, Z_FLAG, cpu->a == 0);
					break;
				}
				case 0x7: {
					//confirmed good
					_push(cpu, &cpu->pc);
					cpu->pc = 0x30;
					break;
				}
				case 0x8: {
					int8_t simmb = ((instruction & 0x00ff0000) >> 16);
					uint32_t val = cpu->sp + simmb;

					if (simmb > 0) {
						set_flag(cpu, C_FLAG, (((cpu->sp & 0xff) + simmb) & 0x100) == 0x100);
						set_flag(cpu, H_FLAG, (((cpu->sp & 0xf) + (simmb & 0xf)) & 0x10) == 0x10); 
					} else {
						set_flag(cpu, C_FLAG, (val & 0xff) < (cpu->sp & 0xff));
						set_flag(cpu, H_FLAG, (val & 0xf) < (cpu->sp & 0xf));
					}

					*cpu->hl = val & 0xffff;

					set_flag(cpu, Z_FLAG, 0);
					set_flag(cpu, N_FLAG, 0);
					break;
				}
				case 0x9: {
					//confirmed good
					cpu->sp = *cpu->hl;
					break;
				}
				case 0xA: {
					//confirmed good
					uint16_t immw = ((instruction & 0x00ffff00) >> 8);
					cpu->a = gb_read(cpu, immw);
					//cpu->a = cpu->addressSpace[immw];
					break;
				}
				case 0xB: {
					//confirmed good
					//cpu->ime = true;
					queue_task(cpu, QUEUE_INTERRUPT_ENABLE, 0, 1);
					break;
				}
				case 0xC: {
					// nothing
					bad_instruction(cpu, instruction >> 24);
					break;
				}
				case 0xD: {
					// nothing
					bad_instruction(cpu, instruction >> 24);
					break;
				}
				case 0xE: {
					uint8_t immb = ((instruction & 0x00ff0000) >> 16);
					set_flag(cpu, H_FLAG, (cpu->a & 0xf) < (immb & 0xf));
					set_flag(cpu, C_FLAG, cpu->a < immb);
					set_flag(cpu, Z_FLAG, cpu->a == immb);
					set_flag(cpu, N_FLAG, 1);
					break;
				}
				case 0xF: {
					_push(cpu, &cpu->pc);
					cpu->pc = 0x38;
					break;
				}
			}
			break;
		}
	}
}


static void execute_prefix_instruction(gb_cpu_t *cpu, uint8_t opcode) {
	uint8_t n1 = (opcode & 0xf0) >> 4;
	uint8_t n2 = opcode & 0x0f;

	uint8_t *reg = cpu->registers[n2 % 8];

	bool memory_access = false;
	if ((n2 % 8) == 6) {
		//reg = cpu->address_space + *cpu->hl;
		memory_access = true;
		cpu->instruction_wait_cycles = 16;
	} else {
		cpu->instruction_wait_cycles = 8;
	}


	switch (n1) {
		case 0x0: {
			//confirmed good
			set_flag(cpu, N_FLAG, 0);
			set_flag(cpu, H_FLAG, 0);

			if (n2 < 0x8) {
				if (memory_access) {
					uint8_t byte = gb_read(cpu, *cpu->hl);
					bool bit = get_bit_on(byte, 7);
					set_flag(cpu, C_FLAG, bit);

					byte <<= 1;
					byte |= bit;
					gb_write_8(cpu, *cpu->hl, byte, 1);
					set_flag(cpu, Z_FLAG, byte == 0);

				} else {
					bool bit = get_bit_on(*reg, 7);
					set_flag(cpu, C_FLAG, bit);
					*reg <<= 1;
					*reg |= bit;
					set_flag(cpu, Z_FLAG, *reg == 0);

				}
			} else {
				if (memory_access) {
					uint8_t byte = gb_read(cpu, *cpu->hl);
					bool bit = get_bit_on(byte, 0);
					set_flag(cpu, C_FLAG, bit);

					byte >>= 1;
					byte |= bit << 7;
					gb_write_8(cpu, *cpu->hl, byte, 1);
					set_flag(cpu, Z_FLAG, byte == 0);
				} else { 
					bool bit = get_bit_on(*reg, 0);
					set_flag(cpu, C_FLAG, bit);
					*reg >>= 1;
					*reg |= bit << 7;
					set_flag(cpu, Z_FLAG, *reg == 0);

				}
			}
			
			break;
		}
		case 0x1: {
			//confirmed good
			set_flag(cpu, N_FLAG, 0);
			set_flag(cpu, H_FLAG, 0);
			if (n2 < 0x8) {
				if (memory_access) {
					uint8_t byte = gb_read(cpu, *cpu->hl);
					bool bit = get_flag_on(cpu, C_FLAG);
					set_flag(cpu, C_FLAG, get_bit_on(byte, 7));

					byte <<= 1;
					byte |= bit;
					gb_write_8(cpu, *cpu->hl, byte, 1);
					set_flag(cpu, Z_FLAG, byte == 0);
				} else {
					bool bit = get_flag_on(cpu, C_FLAG);
					set_flag(cpu, C_FLAG, get_bit_on(*reg, 7));
					*reg <<= 1;
					*reg |= bit;
					set_flag(cpu, Z_FLAG, *reg == 0);
				}
			} else {
				if (memory_access) {
					uint8_t byte = gb_read(cpu, *cpu->hl);
					bool bit = get_bit_on(byte, 0);

					byte >>= 1;
					byte |= get_flag_on(cpu, C_FLAG) << 7;

					gb_write_8(cpu, *cpu->hl, byte, 1);
					set_flag(cpu, C_FLAG, bit);
					set_flag(cpu, Z_FLAG, byte == 0);
				} else {
					bool bit = get_bit_on(*reg, 0);
					*reg >>= 1;
					*reg |= get_flag_on(cpu, C_FLAG) << 7;
					set_flag(cpu, C_FLAG, bit);
					set_flag(cpu, Z_FLAG, *reg == 0);
				}
			}


			break;
		}
		case 0x2: {
			//confirmed good
			set_flag(cpu, N_FLAG, 0);
			set_flag(cpu, H_FLAG, 0);
			if (n2 < 0x8) {
				if (memory_access) {
					uint8_t byte = gb_read(cpu, *cpu->hl);
					bool bit = get_bit_on(byte, 7);
					
					set_flag(cpu, C_FLAG, bit);

					byte <<= 1;
					set_bit_on(&byte, 0, 0);
					gb_write_8(cpu, *cpu->hl, byte, 1);
					set_flag(cpu, Z_FLAG, byte == 0);
				} else {
					set_flag(cpu, C_FLAG, get_bit_on(*reg, 7));
					*reg <<= 1;
					set_bit_on(reg, 0, 0);
					set_flag(cpu, Z_FLAG, *reg == 0);
				}

			} else {
				if (memory_access) {
					uint8_t byte = gb_read(cpu, *cpu->hl);
					bool bit = get_bit_on(byte, 7);
					set_flag(cpu, C_FLAG, get_bit_on(byte, 0));

					byte >>= 1;
					set_bit_on(&byte, 7, bit);
					gb_write_8(cpu, *cpu->hl, byte, 1);
					set_flag(cpu, Z_FLAG, byte == 0);
				} else {
					bool bit = get_bit_on(*reg, 7);
					set_flag(cpu, C_FLAG, get_bit_on(*reg, 0));
					*reg >>= 1;
					set_bit_on(reg, 7, bit);
					set_flag(cpu, Z_FLAG, *reg == 0);
				}
			}

			break;
		}
		case 0x3: {
			//confirmed good
			set_flag(cpu, N_FLAG, 0);
			set_flag(cpu, H_FLAG, 0);
			if (n2 < 0x8) {
				if (memory_access) {
					uint8_t byte = gb_read(cpu, *cpu->hl);

					set_flag(cpu, C_FLAG, 0);
					uint8_t newValue = ( (byte & 0x0F) << 4 | (byte & 0xF0) >> 4 );
					byte = newValue;

					gb_write_8(cpu, *cpu->hl, byte, 1);
					set_flag(cpu, Z_FLAG, byte == 0);
				} else {
					set_flag(cpu, C_FLAG, 0);
					uint8_t newValue = ( (*reg & 0x0F) << 4 | (*reg & 0xF0) >> 4 );
					*reg = newValue;
					set_flag(cpu, Z_FLAG, *reg == 0);
				}
			} else {
				if (memory_access) {
					uint8_t byte = gb_read(cpu, *cpu->hl);
					bool bit = get_bit_on(byte, 0);
					set_flag(cpu, C_FLAG, bit);

					byte >>= 1;
					set_bit_on(&byte, 7, 0);
					gb_write_8(cpu, *cpu->hl, byte, 1);
					set_flag(cpu, Z_FLAG, byte == 0);
				} else {
					bool bit = get_bit_on(*reg, 0);
					set_flag(cpu, C_FLAG, bit);
					*reg >>= 1;
					set_bit_on(reg, 7, 0);
					set_flag(cpu, Z_FLAG, *reg == 0);
				}
			}

			break;
		}
		case 0x4:
		case 0x5:
		case 0x6:
		case 0x7: {
			//confirmed good
			if ((n2 % 8) == 6 && (n2 == 0x6 || n2 == 0xE)) {
				cpu->instruction_wait_cycles = 12;
			}
			uint8_t bit = (n1 - 0x4) * 2;
			if (n2 > 0x7)
				bit++;

			bool status;

			if (memory_access) {
				uint8_t byte = gb_read(cpu, *cpu->hl);
				status = get_bit_on(byte, bit);
			} else {
				status = get_bit_on(*reg, bit);
			}
			set_flag(cpu, N_FLAG, 0);
			set_flag(cpu, H_FLAG, 1);
			set_flag(cpu, Z_FLAG, status == 0);
			break;
		}
		case 0x8:
		case 0x9:
		case 0xA:
		case 0xB: {
			//confirmed good
			uint8_t bit = (n1 - 0x8) * 2;
			if (n2 > 0x7)
				bit++;
			if (memory_access) {
				uint8_t byte = gb_read(cpu, *cpu->hl);
				set_bit_on(&byte, bit, 0);

				gb_write_8(cpu, *cpu->hl, byte, 1);
			} else {
				set_bit_on(reg, bit, 0);
			}
			break;
		}

		case 0xC:
		case 0xD:
		case 0xE:
		case 0xF: {
			//confirmed good
			uint8_t bit = (n1 - 0xC) * 2;
			if (n2 > 0x7)
				bit++;
			if (memory_access) {
				uint8_t byte = gb_read(cpu, *cpu->hl);
				set_bit_on(&byte, bit, 1);

				gb_write_8(cpu, *cpu->hl, byte, 1);
			} else {
				set_bit_on(reg, bit, 1);
			}
			break;
		}
	}
}