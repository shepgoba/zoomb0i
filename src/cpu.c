#include "cpu.h"
#include "utils.h"


#define ___fastcall __attribute__((fastcall))

static inline void execute_main_instruction(gb_cpu_t *cpu, uint32_t instruction);
extern void executeee_main_instruction(gb_cpu_t *cpu, uint32_t instruction);


/* 
    0 is either a nonexistant opcode or it indicates 
	an opcode with a variable length cycle count, 
	which is handled in the instruction implementation instead 
*/

void unmap_bootrom_if_necessary(gb_cpu_t *cpu)
{
	static bool unmapped_bootrom = false;
	
	if (cpu->load_bootstrap) {
		if (!unmapped_bootrom) {
			memcpy(cpu->address_space, cpu->romptr, 0x100);
			unmapped_bootrom = true;
		}
	}
}

uint8_t instruction_cycle_count[16][16] = 
{
    //0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
    {4, 12, 8, 8, 4, 4, 8, 4, 20, 8, 8, 8, 4, 4, 8, 4},
    {4, 12, 8, 8, 4, 4, 8, 4, 12, 8, 8, 8, 4, 4, 8, 4},
    {0, 12, 8, 8, 4, 4, 8, 4, 0, 8, 8, 8, 4, 4, 8, 4},
    {0, 12, 8, 8, 12, 12, 12, 4, 0, 8, 8, 8, 4, 4, 8, 4},
    {4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4},
    {4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4},
    {4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4},
    {8, 8, 8, 8, 8, 8, 4, 8, 4, 4, 4, 4, 4, 4, 8, 4},
    {4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4},
    {4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4},
    {4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4},
    {4, 4, 4, 4, 4, 4, 8, 4, 4, 4, 4, 4, 4, 4, 8, 4},
    {0, 12, 0, 16, 0, 16, 8, 16, 0, 16, 0, 4, 0, 24, 8, 16},
    {0, 12, 0, 0, 0, 16, 8, 16, 0, 16, 0, 0, 0, 0, 8, 16},
    {12, 12, 8, 0, 0, 16, 8, 16, 16, 4, 16, 0, 0, 0, 8, 16},
    {12, 12, 8, 4, 0, 16, 8, 16, 12, 8, 16, 4, 0, 0, 8, 16}
};

uint8_t instruction_byte_size[16][16] = 
{
    //0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
    {1, 3, 1, 1, 1, 1, 2, 1, 3, 1, 1, 1, 1, 1, 2, 1},
    {1, 3, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1},
    {2, 3, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1},
    {2, 3, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1},
    {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
    {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
    {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
    {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
    {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
    {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
    {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
    {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
    {1, 1, 3, 3, 3, 1, 2, 1, 1, 1, 3, 2, 3, 3, 2, 1},
    {1, 1, 3, 0, 3, 1, 2, 1, 1, 1, 3, 0, 3, 0, 2, 1},
    {2, 1, 1, 0, 0, 1, 2, 1, 2, 1, 3, 0, 0, 0, 2, 1},
    {2, 1, 1, 1, 0, 1, 2, 1, 2, 1, 3, 1, 0, 0, 2, 1}
};

void _halt(gb_cpu_t *cpu, uint32_t instruction) 
{
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


 void update_input_registers(gb_cpu_t *cpu)
{
	if ((*cpu->joyp >> 4) & 1) {
		*cpu->joyp &= 0xf0;
		*cpu->joyp |= cpu->key_status & 0xf;
	} else if ((*cpu->joyp >> 5) & 1) {
		*cpu->joyp &= 0xf0;
		*cpu->joyp |= (cpu->key_status & 0xf0) >> 4;
	}
}


void bad_instruction(gb_cpu_t *cpu, uint8_t opcode) 
{
	printf("a bad instruction (%02x) was executed (we shouldn't have gotten here!!)\n", opcode);
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

/* convenience function to get a cpu flag's value */
static uint8_t get_flag_on(gb_cpu_t *cpu, uint8_t bit) {
	return (cpu->f >> bit) & 1;
}

/* convenience function to toggle an interrupt flag */
inline void set_interrupt_flag(gb_cpu_t *cpu, uint8_t bit, uint8_t status) 
{
	*cpu->ifl ^= ((-status ^ *cpu->ifl) & (1UL << bit));
}


__noinline uint8_t gb_read(gb_cpu_t *cpu, uint16_t address) {
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
			if (cpu->mbc1 || cpu->mbc3) {
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

__noinline void gb_write(gb_cpu_t *cpu, uint16_t address, uint8_t data, uint8_t size) {
	if (cpu->in_dma) {
		if ((address < 0xFF80 && address > 0xFFFE))
			return;
	}
	
	switch (address & 0xF000) {
		case 0x0000:
		case 0x1000: {
			// do nothing as writing to bank0 isnt allowed
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
			} else if (cpu->mbc3) {
				cpu->last_bank = data;
			}
			break;	
		}

		case 0x4000:
		case 0x5000:
		case 0x6000:
		case 0x7000: {
			//do nothing as writing to ROM isnt allowed

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
							case 0xFF50: {
								unmap_bootrom_if_necessary(cpu);
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

void _push(gb_cpu_t *cpu, uint16_t *wreg) {
	cpu->sp -= 2;
	uint8_t top = (*wreg >> 8) & 0xff;
	uint8_t bottom = *wreg & 0xff;

	gb_write(cpu, cpu->sp, bottom, 1);
	gb_write(cpu, cpu->sp + 1, top, 1);
}

static void _pop(gb_cpu_t *cpu, uint16_t *wreg) {
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
	cpu->load_bootstrap = load_bootstrap;
	cpu->ppu = ppu;
	cpu->key_status = 0xff;
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

uint32_t fetch_instruction(gb_cpu_t *cpu) 
{
	uint16_t loc = cpu->pc;
	uint8_t *addr = cpu->address_space;

	if (cpu->mbc1 || cpu->mbc3) {
		if (loc >= 0x4000 && loc < 0x8000) {
			addr = cpu->romptr + cpu->last_bank * 0x4000 + loc - 0x4000;
			loc = 0;
		}
	}

	uint8_t opcode = addr[loc];

	uint8_t instruction_size = *((uint8_t *)&instruction_byte_size + opcode);
	cpu->instruction_wait_cycles = *((uint8_t *)&instruction_cycle_count + opcode);

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


/*
big thanks to this

https://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
*/


static void nothing(gb_cpu_t *cpu, uint32_t instruction) 
{

}

static void ___fastcall ld_bc_d16(gb_cpu_t *cpu, uint32_t instruction) {
	uint16_t immw = ((instruction & 0x00ffff00) >> 8);
	*cpu->bc = immw;
}

static void ___fastcall ld_addr_bc_a(gb_cpu_t *cpu, uint32_t instruction) {
	gb_write(cpu, *cpu->bc, cpu->a, 1);
}

static void ___fastcall inc_bc(gb_cpu_t *cpu, uint32_t instruction) {
	(*cpu->bc)++;
}

static void ___fastcall inc_b(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, (cpu->b++ & 0xf) == 0xf);
					
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->b == 0);
}

static void ___fastcall dec_b(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, (--cpu->b & 0xf) == 0xf);

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->b == 0);
}

static void ___fastcall ld_b_d8(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t immb = ((instruction & 0x00ff0000) >> 16);
	cpu->b = immb;
}

static void ___fastcall rlca(gb_cpu_t *cpu, uint32_t instruction) {
	bool bit = get_bit_on(cpu->a, 7);
	set_flag(cpu, C_FLAG, bit);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, H_FLAG, 0);
	cpu->a <<= 1;
	cpu->a |= bit;
	set_flag(cpu, Z_FLAG, 0);
}

static void ___fastcall ld_addr_a16_sp(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t b1 = (cpu->sp >> 8) & 0xff;
	uint8_t b2 = cpu->sp & 0xff;

	uint16_t immw = ((instruction & 0x00ffff00) >> 8);

	gb_write(cpu, immw + 1, b1, 1);
	gb_write(cpu, immw, b2, 1);
}

static void ___fastcall add_hl_bc(gb_cpu_t *cpu, uint32_t instruction) {
	uint32_t val = (*cpu->hl + *cpu->bc);
	set_flag(cpu, C_FLAG, (val & 0x10000) == 0x10000);
	set_flag(cpu, H_FLAG, (((*cpu->hl & 0xfff) + (*cpu->bc & 0xfff)) & 0x1000) == 0x1000);

	*cpu->hl = val & 0xffff;

	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, N_FLAG, 0);
}

static void ___fastcall ld_a_addr_bc(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->a = gb_read(cpu, *cpu->bc);
}

static void ___fastcall dec_bc(gb_cpu_t *cpu, uint32_t instruction) {
	(*cpu->bc)--;
}

static void ___fastcall inc_c(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, (cpu->c++ & 0xf) == 0xf);
					
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->c == 0);
}

static void ___fastcall dec_c(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, (--cpu->c & 0xf) == 0xf);

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->c == 0);
}

static void ___fastcall ld_c_d8(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t immb = ((instruction & 0x00ff0000) >> 16);
	cpu->c = immb;
}

static void ___fastcall rrca(gb_cpu_t *cpu, uint32_t instruction) {
	bool bit = get_bit_on(cpu->a, 0);
	set_flag(cpu, C_FLAG, bit);
	set_flag(cpu, H_FLAG, 0);
	set_flag(cpu, N_FLAG, 0);
	cpu->a >>= 1;
	cpu->a |= bit << 7;
	set_flag(cpu, Z_FLAG, 0);
}

static void ___fastcall stop(gb_cpu_t *cpu, uint32_t instruction) {

}

static void ___fastcall ld_de_d16(gb_cpu_t *cpu, uint32_t instruction) {
	uint16_t immw = ((instruction & 0x00ffff00) >> 8);
	*cpu->de = immw;
}

static void ___fastcall ld_addr_de_a(gb_cpu_t *cpu, uint32_t instruction) {
	gb_write(cpu, *cpu->de, cpu->a, 1);
}

static void ___fastcall inc_de(gb_cpu_t *cpu, uint32_t instruction) {
	(*cpu->de)++;
	
}

static void ___fastcall inc_d(gb_cpu_t *cpu, uint32_t instruction) {
	if ((cpu->d++ & 0xf) == 0xf)
		set_flag(cpu, H_FLAG, 1);
	else
		set_flag(cpu, H_FLAG, 0);
	
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->d == 0);
}

static void ___fastcall dec_d(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, (--cpu->d & 0xf) == 0xf);
	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->d == 0);
}

static void ___fastcall ld_d_d8(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t immb = ((instruction & 0x00ff0000) >> 16);
	cpu->d = immb;
}

static void ___fastcall rla(gb_cpu_t *cpu, uint32_t instruction) {
	bool bit = get_bit_on(cpu->a, 7);
	bool newBit = get_flag_on(cpu, C_FLAG);
	set_flag(cpu, C_FLAG, bit);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, H_FLAG, 0);
	cpu->a <<= 1;
	cpu->a |= newBit;
	set_flag(cpu, Z_FLAG, 0);
}

static void ___fastcall jr_r8(gb_cpu_t *cpu, uint32_t instruction) {
	int8_t simmb = ((instruction & 0x00ff0000) >> 16);
	cpu->pc += simmb;
}

static void ___fastcall add_hl_de(gb_cpu_t *cpu, uint32_t instruction) {
	uint32_t val = (*cpu->hl + *cpu->de);
	set_flag(cpu, C_FLAG, (val & 0x10000) == 0x10000);
	set_flag(cpu, H_FLAG, (((*cpu->hl & 0xfff) + (*cpu->de & 0xfff)) & 0x1000) == 0x1000);

	*cpu->hl = val & 0xffff;

	set_flag(cpu, N_FLAG, 0);
}

static void ___fastcall ld_a_addr_de(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->a = gb_read(cpu, *cpu->de);
}

static void ___fastcall dec_de(gb_cpu_t *cpu, uint32_t instruction) {
	(*cpu->de)--;
}

static void ___fastcall inc_e(gb_cpu_t *cpu, uint32_t instruction) {
	if ((cpu->e++ & 0xf) == 0xf)
		set_flag(cpu, H_FLAG, 1);
	else
		set_flag(cpu, H_FLAG, 0);
	
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->e == 0);
}

static void ___fastcall dec_e(gb_cpu_t *cpu, uint32_t instruction) {
	if ((--cpu->e & 0xf) == 0xf)
		set_flag(cpu, H_FLAG, 1);
	else
		set_flag(cpu, H_FLAG, 0);
	
	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->e == 0);
}

static void ___fastcall ld_e_d8(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t immb = ((instruction & 0x00ff0000) >> 16);
	cpu->e = immb;
}

static void ___fastcall rra(gb_cpu_t *cpu, uint32_t instruction) {
	bool bit = get_flag_on(cpu, C_FLAG);
	set_flag(cpu, C_FLAG, get_bit_on(cpu->a, 0));
	set_flag(cpu, H_FLAG, 0);
	set_flag(cpu, N_FLAG, 0);
	cpu->a >>= 1;
	cpu->a |= bit << 7;
	set_flag(cpu, Z_FLAG, 0);
}

static void ___fastcall jr_nz_r8(gb_cpu_t *cpu, uint32_t instruction) {
	if (!get_flag_on(cpu, Z_FLAG)) {
		int8_t simmb = ((instruction & 0x00ff0000) >> 16);
		cpu->pc += simmb;
		cpu->instruction_wait_cycles = 12;
	} else {
		cpu->instruction_wait_cycles = 8;
	}
}

static void ___fastcall ld_hl_d16(gb_cpu_t *cpu, uint32_t instruction) {
	uint16_t immw = ((instruction & 0x00ffff00) >> 8);
	*cpu->hl = immw;
}

static void ___fastcall ld_addr_hli_a(gb_cpu_t *cpu, uint32_t instruction) {
	gb_write(cpu, *cpu->hl, cpu->a, 1);
	(*cpu->hl)++;
}

static void ___fastcall inc_hl(gb_cpu_t *cpu, uint32_t instruction) {
	(*cpu->hl)++;
}

static void ___fastcall inc_h(gb_cpu_t *cpu, uint32_t instruction) {
	if ((cpu->h++ & 0xf) == 0xf)
		set_flag(cpu, H_FLAG, 1);
	else
		set_flag(cpu, H_FLAG, 0);
	
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->h == 0);
}

static void ___fastcall dec_h(gb_cpu_t *cpu, uint32_t instruction) {
	if ((--cpu->h & 0xf) == 0xf)
		set_flag(cpu, H_FLAG, 1);
	else
		set_flag(cpu, H_FLAG, 0);
	
	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->h == 0);
}

static void ___fastcall ld_h_d8(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t immb = ((instruction & 0x00ff0000) >> 16);
	cpu->h = immb;
}

static void ___fastcall daa(gb_cpu_t *cpu, uint32_t instruction) {
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
}

static void ___fastcall jr_z_r8(gb_cpu_t *cpu, uint32_t instruction) {
	if (get_flag_on(cpu, Z_FLAG)) {
		int8_t simmb = ((instruction & 0x00ff0000) >> 16);
		cpu->pc += simmb;
		cpu->instruction_wait_cycles = 12;
	} else {
		cpu->instruction_wait_cycles = 8;
	}
}

static void ___fastcall add_hl_hl(gb_cpu_t *cpu, uint32_t instruction) {
	uint32_t val = *cpu->hl + *cpu->hl;
	set_flag(cpu, C_FLAG, (val & 0x10000) == 0x10000);
	set_flag(cpu, H_FLAG, (((*cpu->hl & 0xfff) + (*cpu->hl & 0xfff)) & 0x1000) == 0x1000);

	*cpu->hl = val & 0xffff;

	set_flag(cpu, N_FLAG, 0);
}

static void ___fastcall ld_a_addr_hli(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->a = gb_read(cpu, *cpu->hl);
	(*cpu->hl)++;
}

static void ___fastcall dec_hl(gb_cpu_t *cpu, uint32_t instruction) {
	(*cpu->hl)--;
}

static void ___fastcall inc_l(gb_cpu_t *cpu, uint32_t instruction) {
	if ((cpu->l++ & 0xf) == 0xf)
		set_flag(cpu, H_FLAG, 1);
	else
		set_flag(cpu, H_FLAG, 0);

	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->l == 0);
}

static void ___fastcall dec_l(gb_cpu_t *cpu, uint32_t instruction) {
	if ((--cpu->l & 0xf) == 0xf)
		set_flag(cpu, H_FLAG, 1);
	else
		set_flag(cpu, H_FLAG, 0);
	
	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->l == 0);
}

static void ___fastcall ld_l_d8(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t immb = ((instruction & 0x00ff0000) >> 16);
	cpu->l = immb;
}

static void ___fastcall cpl(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->a = ~cpu->a;
	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, H_FLAG, 1);
}

static void ___fastcall jr_nc_r8(gb_cpu_t *cpu, uint32_t instruction) {
	if (!get_flag_on(cpu, C_FLAG)) {
		int8_t simmb = ((instruction & 0x00ff0000) >> 16);
		cpu->pc += simmb;
		cpu->instruction_wait_cycles = 12;
	} else {
		cpu->instruction_wait_cycles = 8;
	}
}

static void ___fastcall ld_sp_d16(gb_cpu_t *cpu, uint32_t instruction) {
	uint16_t immw = ((instruction & 0x00ffff00) >> 8);
	cpu->sp = immw;
}

static void ___fastcall ld_addr_hld_a(gb_cpu_t *cpu, uint32_t instruction) {
	gb_write(cpu, *cpu->hl, cpu->a, 1);
	(*cpu->hl)--;
}

static void ___fastcall inc_sp(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->sp++;
}

static void ___fastcall inc_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t byte = gb_read(cpu, *cpu->hl);
	if ((byte & 0xf) == 0xf)
		set_flag(cpu, H_FLAG, 1);
	else
		set_flag(cpu, H_FLAG, 0);

	byte++;
	gb_write(cpu, *cpu->hl, byte, 1);

	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, gb_read(cpu, *cpu->hl) == 0);
}

static void ___fastcall dec_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t byte = gb_read(cpu, *cpu->hl);
	
	if ((--byte & 0xf) == 0xf)
		set_flag(cpu, H_FLAG, 1);
	else
		set_flag(cpu, H_FLAG, 0);

	gb_write(cpu, *cpu->hl, byte, 1);

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, gb_read(cpu, *cpu->hl) == 0);
}

static void ___fastcall ld_addr_hl_d8(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t immb = ((instruction & 0x00ff0000) >> 16);
	gb_write(cpu, *cpu->hl, immb, 1);
}

static void ___fastcall scf(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, C_FLAG, 1);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, H_FLAG, 0);
}

static void ___fastcall jr_c_r8(gb_cpu_t *cpu, uint32_t instruction) {
	if (get_flag_on(cpu, C_FLAG)) {
		int8_t simmb = ((instruction & 0x00ff0000) >> 16);
		cpu->pc += simmb;
		cpu->instruction_wait_cycles = 12;
	} else {
		cpu->instruction_wait_cycles = 8;
	}
}

static void ___fastcall add_hl_sp(gb_cpu_t *cpu, uint32_t instruction) {
	uint32_t val = *cpu->hl + cpu->sp;
	set_flag(cpu, C_FLAG, (val & 0x10000) == 0x10000);
	set_flag(cpu, H_FLAG, (((*cpu->hl & 0xfff) + (cpu->sp & 0xfff)) & 0x1000) == 0x1000);

	*cpu->hl = val & 0xffff;

	set_flag(cpu, N_FLAG, 0);
}

static void ___fastcall ld_a_addr_hld(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->a = gb_read(cpu, *cpu->hl);
	(*cpu->hl)--;
}

static void ___fastcall dec_sp(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->sp--;
}

static void ___fastcall inc_a(gb_cpu_t *cpu, uint32_t instruction) {
	if ((cpu->a++ & 0xf) == 0xf)
		set_flag(cpu, H_FLAG, 1);
	else
		set_flag(cpu, H_FLAG, 0);

	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall dec_a(gb_cpu_t *cpu, uint32_t instruction) {
	if ((--cpu->a & 0xf) == 0xf)
		set_flag(cpu, H_FLAG, 1);
	else
		set_flag(cpu, H_FLAG, 0);

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall ld_a_d8(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t immb = ((instruction & 0x00ff0000) >> 16);
	cpu->a = immb;
}

static void ___fastcall ccf(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, C_FLAG, !get_flag_on(cpu, C_FLAG));
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, H_FLAG, 0);
}

static void ___fastcall ld_b_b(gb_cpu_t *cpu, uint32_t instruction) {

}

static void ___fastcall ld_b_c(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->b = cpu->c;
}

static void ___fastcall ld_b_d(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->b = cpu->d;
}

static void ___fastcall ld_b_e(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->b = cpu->e;
}

static void ___fastcall ld_b_h(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->b = cpu->h;
}

static void ___fastcall ld_b_l(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->b = cpu->l;
}

static void ___fastcall ld_b_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->b = gb_read(cpu, *cpu->hl);
}

static void ___fastcall ld_b_a(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->b = cpu->a;
}

static void ___fastcall ld_c_b(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->c = cpu->b;
}

static void ___fastcall ld_c_c(gb_cpu_t *cpu, uint32_t instruction) {
	
}

static void ___fastcall ld_c_d(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->c = cpu->d;
}

static void ___fastcall ld_c_e(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->c = cpu->e;
}

static void ___fastcall ld_c_h(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->c = cpu->h;
}

static void ___fastcall ld_c_l(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->c = cpu->l;
}

static void ___fastcall ld_c_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->c = gb_read(cpu, *cpu->hl);
}

static void ___fastcall ld_c_a(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->c = cpu->a;
}

static void ___fastcall ld_d_b(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->d = cpu->b;
}

static void ___fastcall ld_d_c(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->d = cpu->c;
}

static void ___fastcall ld_d_d(gb_cpu_t *cpu, uint32_t instruction) {

}

static void ___fastcall ld_d_e(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->d = cpu->e;
}

static void ___fastcall ld_d_h(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->d = cpu->h;
}

static void ___fastcall ld_d_l(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->d = cpu->l;
}

static void ___fastcall ld_d_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->d = gb_read(cpu, *cpu->hl);
}

static void ___fastcall ld_d_a(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->d = cpu->a;
}

static void ___fastcall ld_e_b(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->e = cpu->b;
}

static void ___fastcall ld_e_c(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->e = cpu->c;
}

static void ___fastcall ld_e_d(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->e = cpu->d;
}

static void ___fastcall ld_e_e(gb_cpu_t *cpu, uint32_t instruction) {

}

static void ___fastcall ld_e_h(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->e = cpu->h;
}

static void ___fastcall ld_e_l(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->e = cpu->l;
}

static void ___fastcall ld_e_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->e = gb_read(cpu, *cpu->hl);
}

static void ___fastcall ld_e_a(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->e = cpu->a;
}

static void ___fastcall ld_h_b(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->h = cpu->b;
}

static void ___fastcall ld_h_c(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->h = cpu->c;
}

static void ___fastcall ld_h_d(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->h = cpu->d;
}

static void ___fastcall ld_h_e(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->h = cpu->e;
}

static void ___fastcall ld_h_h(gb_cpu_t *cpu, uint32_t instruction) {

}

static void ___fastcall ld_h_l(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->h = cpu->l;
}

static void ___fastcall ld_h_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->h = gb_read(cpu, *cpu->hl);
}

static void ___fastcall ld_h_a(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->h = cpu->a;
}

static void ___fastcall ld_l_b(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->l = cpu->b;
}

static void ___fastcall ld_l_c(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->l = cpu->c;
}

static void ___fastcall ld_l_d(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->l = cpu->d;
}

static void ___fastcall ld_l_e(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->l = cpu->e;
}

static void ___fastcall ld_l_h(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->l = cpu->h;
}

static void ___fastcall ld_l_l(gb_cpu_t *cpu, uint32_t instruction) {

}

static void ___fastcall ld_l_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->l = gb_read(cpu, *cpu->hl);
}

static void ___fastcall ld_l_a(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->l = cpu->a;
}

static void ___fastcall ld_addr_hl_b(gb_cpu_t *cpu, uint32_t instruction) {
	gb_write(cpu, *cpu->hl, cpu->b, 1);
}

static void ___fastcall ld_addr_hl_c(gb_cpu_t *cpu, uint32_t instruction) {
	gb_write(cpu, *cpu->hl, cpu->c, 1);
}

static void ___fastcall ld_addr_hl_d(gb_cpu_t *cpu, uint32_t instruction) {
	gb_write(cpu, *cpu->hl, cpu->d, 1);
}

static void ___fastcall ld_addr_hl_e(gb_cpu_t *cpu, uint32_t instruction) {
	gb_write(cpu, *cpu->hl, cpu->e, 1);
}

static void ___fastcall ld_addr_hl_h(gb_cpu_t *cpu, uint32_t instruction) {
	gb_write(cpu, *cpu->hl, cpu->h, 1);
}

static void ___fastcall ld_addr_hl_l(gb_cpu_t *cpu, uint32_t instruction) {
	gb_write(cpu, *cpu->hl, cpu->l, 1);
}

static void ___fastcall ld_addr_hl_a(gb_cpu_t *cpu, uint32_t instruction) {
	gb_write(cpu, *cpu->hl, cpu->a, 1);
}

static void ___fastcall ld_a_b(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->a = cpu->b;
}

static void ___fastcall ld_a_c(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->a = cpu->c;
}

static void ___fastcall ld_a_d(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->a = cpu->d;
}

static void ___fastcall ld_a_e(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->a = cpu->e;
}

static void ___fastcall ld_a_h(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->a = cpu->h;
}

static void ___fastcall ld_a_l(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->a = cpu->l;
}

static void ___fastcall ld_a_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->a = gb_read(cpu, *cpu->hl);
}

static void ___fastcall ld_a_a(gb_cpu_t *cpu, uint32_t instruction) {

}

static void ___fastcall add_a_b(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->b;
	uint16_t val = cpu->a + reg;

	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	set_flag(cpu, H_FLAG, (((cpu->a & 0xf) + (reg & 0xf)) & 0x10) == 0x10);

	cpu->a = val & 0xff;
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall add_a_c(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->c;
	uint16_t val = cpu->a + reg;
	
	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	set_flag(cpu, H_FLAG, (((cpu->a & 0xf) + (reg & 0xf)) & 0x10) == 0x10);

	cpu->a = val & 0xff;
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall add_a_d(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->d;
	uint16_t val = cpu->a + reg;
	
	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	set_flag(cpu, H_FLAG, (((cpu->a & 0xf) + (reg & 0xf)) & 0x10) == 0x10);

	cpu->a = val & 0xff;
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall add_a_e(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->e;
	uint16_t val = cpu->a + reg;
	
	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	set_flag(cpu, H_FLAG, (((cpu->a & 0xf) + (reg & 0xf)) & 0x10) == 0x10);

	cpu->a = val & 0xff;
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall add_a_h(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->h;
	uint16_t val = cpu->a + reg;
	
	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	set_flag(cpu, H_FLAG, (((cpu->a & 0xf) + (reg & 0xf)) & 0x10) == 0x10);

	cpu->a = val & 0xff;
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall add_a_l(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->l;
	uint16_t val = cpu->a + reg;
	
	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	set_flag(cpu, H_FLAG, (((cpu->a & 0xf) + (reg & 0xf)) & 0x10) == 0x10);

	cpu->a = val & 0xff;
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall add_a_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = gb_read(cpu, *cpu->hl);
	uint16_t val = cpu->a + reg;
	
	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	set_flag(cpu, H_FLAG, (((cpu->a & 0xf) + (reg & 0xf)) & 0x10) == 0x10);

	cpu->a = val & 0xff;
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall add_a_a(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->a;
	uint16_t val = cpu->a + reg;
	
	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	set_flag(cpu, H_FLAG, (((cpu->a & 0xf) + (reg & 0xf)) & 0x10) == 0x10);

	cpu->a = val & 0xff;
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall adc_a_b(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->b;
	uint16_t val = cpu->a + reg + get_flag_on(cpu, C_FLAG);
	uint8_t low_nibble_a = cpu->a & 0xf;
	uint8_t low_nibble_nc = reg & 0xf;
	set_flag(cpu, H_FLAG, (((low_nibble_a + low_nibble_nc + get_flag_on(cpu, C_FLAG)) & 0x10)) == 0x10);
	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	cpu->a = val & 0xff;
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall adc_a_c(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->c;
	uint16_t val = cpu->a + reg + get_flag_on(cpu, C_FLAG);
	uint8_t low_nibble_a = cpu->a & 0xf;
	uint8_t low_nibble_nc = reg & 0xf;
	set_flag(cpu, H_FLAG, (((low_nibble_a + low_nibble_nc + get_flag_on(cpu, C_FLAG)) & 0x10)) == 0x10);
	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	cpu->a = val & 0xff;
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall adc_a_d(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->d;
	uint16_t val = cpu->a + reg + get_flag_on(cpu, C_FLAG);
	uint8_t low_nibble_a = cpu->a & 0xf;
	uint8_t low_nibble_nc = reg & 0xf;
	set_flag(cpu, H_FLAG, (((low_nibble_a + low_nibble_nc + get_flag_on(cpu, C_FLAG)) & 0x10)) == 0x10);
	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	cpu->a = val & 0xff;
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall adc_a_e(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->e;
	uint16_t val = cpu->a + reg + get_flag_on(cpu, C_FLAG);
	uint8_t low_nibble_a = cpu->a & 0xf;
	uint8_t low_nibble_nc = reg & 0xf;
	set_flag(cpu, H_FLAG, (((low_nibble_a + low_nibble_nc + get_flag_on(cpu, C_FLAG)) & 0x10)) == 0x10);
	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	cpu->a = val & 0xff;
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall adc_a_h(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->h;
	uint16_t val = cpu->a + reg + get_flag_on(cpu, C_FLAG);
	uint8_t low_nibble_a = cpu->a & 0xf;
	uint8_t low_nibble_nc = reg & 0xf;
	set_flag(cpu, H_FLAG, (((low_nibble_a + low_nibble_nc + get_flag_on(cpu, C_FLAG)) & 0x10)) == 0x10);
	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	cpu->a = val & 0xff;
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall adc_a_l(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->l;
	uint16_t val = cpu->a + reg + get_flag_on(cpu, C_FLAG);
	uint8_t low_nibble_a = cpu->a & 0xf;
	uint8_t low_nibble_nc = reg & 0xf;
	set_flag(cpu, H_FLAG, (((low_nibble_a + low_nibble_nc + get_flag_on(cpu, C_FLAG)) & 0x10)) == 0x10);
	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	cpu->a = val & 0xff;
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall adc_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = gb_read(cpu, *cpu->hl);
	uint16_t val = cpu->a + reg + get_flag_on(cpu, C_FLAG);
	uint8_t low_nibble_a = cpu->a & 0xf;
	uint8_t low_nibble_nc = reg & 0xf;
	set_flag(cpu, H_FLAG, (((low_nibble_a + low_nibble_nc + get_flag_on(cpu, C_FLAG)) & 0x10)) == 0x10);
	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	cpu->a = val & 0xff;
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall adc_a_a(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->a;
	uint16_t val = cpu->a + reg + get_flag_on(cpu, C_FLAG);
	uint8_t low_nibble_a = cpu->a & 0xf;
	uint8_t low_nibble_nc = reg & 0xf;
	set_flag(cpu, H_FLAG, (((low_nibble_a + low_nibble_nc + get_flag_on(cpu, C_FLAG)) & 0x10)) == 0x10);
	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	cpu->a = val & 0xff;
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall sub_b(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->b;
	uint16_t val = cpu->a - reg;
	set_flag(cpu, C_FLAG, cpu->a < reg);
	set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (reg & 0xf)) < 0);
	cpu->a = val & 0xff;

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall sub_c(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->c;
	uint16_t val = cpu->a - reg;
	set_flag(cpu, C_FLAG, cpu->a < reg);
	set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (reg & 0xf)) < 0);
	cpu->a = val & 0xff;

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall sub_d(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->d;
	uint16_t val = cpu->a - reg;
	set_flag(cpu, C_FLAG, cpu->a < reg);
	set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (reg & 0xf)) < 0);
	cpu->a = val & 0xff;

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall sub_e(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->e;
	uint16_t val = cpu->a - reg;
	set_flag(cpu, C_FLAG, cpu->a < reg);
	set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (reg & 0xf)) < 0);
	cpu->a = val & 0xff;

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall sub_h(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->h;
	uint16_t val = cpu->a - reg;
	set_flag(cpu, C_FLAG, cpu->a < reg);
	set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (reg & 0xf)) < 0);
	cpu->a = val & 0xff;

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall sub_l(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->l;
	uint16_t val = cpu->a - reg;
	set_flag(cpu, C_FLAG, cpu->a < reg);
	set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (reg & 0xf)) < 0);
	cpu->a = val & 0xff;

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall sub_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = gb_read(cpu, *cpu->hl);
	uint16_t val = cpu->a - reg;
	set_flag(cpu, C_FLAG, cpu->a < reg);
	set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (reg & 0xf)) < 0);
	cpu->a = val & 0xff;

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall sub_a(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->a;
	uint16_t val = cpu->a - reg;
	set_flag(cpu, C_FLAG, cpu->a < reg);
	set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (reg & 0xf)) < 0);
	cpu->a = val & 0xff;

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall sbc_b(gb_cpu_t *cpu, uint32_t instruction) {
	uint16_t reg = cpu->b + get_flag_on(cpu, C_FLAG);

	uint16_t val = cpu->a - reg;
	set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (val & 0xf) - get_flag_on(cpu, C_FLAG)) < 0);
	set_flag(cpu, C_FLAG, cpu->a < reg);
	cpu->a = val & 0xff;

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall sbc_c(gb_cpu_t *cpu, uint32_t instruction) {
	uint16_t reg = cpu->c + get_flag_on(cpu, C_FLAG);

	uint16_t val = cpu->a - reg;
	set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (val & 0xf) - get_flag_on(cpu, C_FLAG)) < 0);
	set_flag(cpu, C_FLAG, cpu->a < reg);
	cpu->a = val & 0xff;

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall sbc_d(gb_cpu_t *cpu, uint32_t instruction) {
	uint16_t reg = cpu->d + get_flag_on(cpu, C_FLAG);

	uint16_t val = cpu->a - reg;
	set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (val & 0xf) - get_flag_on(cpu, C_FLAG)) < 0);
	set_flag(cpu, C_FLAG, cpu->a < reg);
	cpu->a = val & 0xff;

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall sbc_e(gb_cpu_t *cpu, uint32_t instruction) {
	uint16_t reg = cpu->e + get_flag_on(cpu, C_FLAG);

	uint16_t val = cpu->a - reg;
	set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (val & 0xf) - get_flag_on(cpu, C_FLAG)) < 0);
	set_flag(cpu, C_FLAG, cpu->a < reg);
	cpu->a = val & 0xff;

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall sbc_h(gb_cpu_t *cpu, uint32_t instruction) {
	uint16_t reg = cpu->h + get_flag_on(cpu, C_FLAG);

	uint16_t val = cpu->a - reg;
	set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (val & 0xf) - get_flag_on(cpu, C_FLAG)) < 0);
	set_flag(cpu, C_FLAG, cpu->a < reg);
	cpu->a = val & 0xff;

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall sbc_l(gb_cpu_t *cpu, uint32_t instruction) {
	uint16_t reg = cpu->l + get_flag_on(cpu, C_FLAG);

	uint16_t val = cpu->a - reg;
	set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (val & 0xf) - get_flag_on(cpu, C_FLAG)) < 0);
	set_flag(cpu, C_FLAG, cpu->a < reg);
	cpu->a = val & 0xff;

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall sbc_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	uint16_t reg = gb_read(cpu, *cpu->hl) + get_flag_on(cpu, C_FLAG);

	uint16_t val = cpu->a - reg;
	set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (val & 0xf) - get_flag_on(cpu, C_FLAG)) < 0);
	set_flag(cpu, C_FLAG, cpu->a < reg);
	cpu->a = val & 0xff;

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall sbc_a(gb_cpu_t *cpu, uint32_t instruction) {
	uint16_t reg = cpu->a + get_flag_on(cpu, C_FLAG);

	uint16_t val = cpu->a - reg;
	set_flag(cpu, H_FLAG, ((cpu->a & 0xf) - (val & 0xf) - get_flag_on(cpu, C_FLAG)) < 0);
	set_flag(cpu, C_FLAG, cpu->a < reg);
	cpu->a = val & 0xff;

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall and_b(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 1);
	cpu->a &= cpu->b;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall and_c(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 1);
	cpu->a &= cpu->c;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall and_d(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 1);
	cpu->a &= cpu->d;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall and_e(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 1);
	cpu->a &= cpu->e;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall and_h(gb_cpu_t *cpu, uint32_t instruction) 
{
	set_flag(cpu, H_FLAG, 1);
	cpu->a &= cpu->h;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall and_l(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 1);
	cpu->a &= cpu->l;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall and_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 1);
	cpu->a &= gb_read(cpu, *cpu->hl);

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall and_a(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 1);
	cpu->a &= cpu->a;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall xor_b(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 0);
	cpu->a ^= cpu->b;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall xor_c(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 0);
	cpu->a ^= cpu->c;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall xor_d(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 0);
	cpu->a ^= cpu->d;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall xor_e(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 0);
	cpu->a ^= cpu->e;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall xor_h(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 0);
	cpu->a ^= cpu->h;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall xor_l(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 0);
	cpu->a ^= cpu->l;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall xor_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 0);
	cpu->a ^= gb_read(cpu, *cpu->hl);

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall xor_a(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 0);
	cpu->a ^= cpu->a;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall or_b(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 0);
	cpu->a |= cpu->b;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall or_c(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 0);
	cpu->a |= cpu->c;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall or_d(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 0);
	cpu->a |= cpu->d;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall or_e(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 0);
	cpu->a |= cpu->e;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall or_h(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 0);
	cpu->a |= cpu->h;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall or_l(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 0);
	cpu->a |= cpu->l;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall or_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 0);
	cpu->a |= gb_read(cpu, *cpu->hl);

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall or_a(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, H_FLAG, 0);
	cpu->a |= cpu->a;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
}

static void ___fastcall cp_b(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->b;
	set_flag(cpu, H_FLAG, (cpu->a & 0xf) < (reg & 0xf));
	set_flag(cpu, C_FLAG, cpu->a < reg);
	set_flag(cpu, Z_FLAG, cpu->a == reg);
	set_flag(cpu, N_FLAG, 1);
}

static void ___fastcall cp_c(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->c;
	set_flag(cpu, H_FLAG, (cpu->a & 0xf) < (reg & 0xf));
	set_flag(cpu, C_FLAG, cpu->a < reg);
	set_flag(cpu, Z_FLAG, cpu->a == reg);
	set_flag(cpu, N_FLAG, 1);
}

static void ___fastcall cp_d(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->d;
	set_flag(cpu, H_FLAG, (cpu->a & 0xf) < (reg & 0xf));
	set_flag(cpu, C_FLAG, cpu->a < reg);
	set_flag(cpu, Z_FLAG, cpu->a == reg);
	set_flag(cpu, N_FLAG, 1);
}

static void ___fastcall cp_e(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->e;
	set_flag(cpu, H_FLAG, (cpu->a & 0xf) < (reg & 0xf));
	set_flag(cpu, C_FLAG, cpu->a < reg);
	set_flag(cpu, Z_FLAG, cpu->a == reg);
	set_flag(cpu, N_FLAG, 1);
}

static void ___fastcall cp_h(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->h;
	set_flag(cpu, H_FLAG, (cpu->a & 0xf) < (reg & 0xf));
	set_flag(cpu, C_FLAG, cpu->a < reg);
	set_flag(cpu, Z_FLAG, cpu->a == reg);
	set_flag(cpu, N_FLAG, 1);
}

static void ___fastcall cp_l(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->l;
	set_flag(cpu, H_FLAG, (cpu->a & 0xf) < (reg & 0xf));
	set_flag(cpu, C_FLAG, cpu->a < reg);
	set_flag(cpu, Z_FLAG, cpu->a == reg);
	set_flag(cpu, N_FLAG, 1);
}

static void ___fastcall cp_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = gb_read(cpu, *cpu->hl);
	set_flag(cpu, H_FLAG, (cpu->a & 0xf) < (reg & 0xf));
	set_flag(cpu, C_FLAG, cpu->a < reg);
	set_flag(cpu, Z_FLAG, cpu->a == reg);
	set_flag(cpu, N_FLAG, 1);
}

static void ___fastcall cp_a(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t reg = cpu->a;
	set_flag(cpu, H_FLAG, (cpu->a & 0xf) < (reg & 0xf));
	set_flag(cpu, C_FLAG, cpu->a < reg);
	set_flag(cpu, Z_FLAG, cpu->a == reg);
	set_flag(cpu, N_FLAG, 1);
}

static void ___fastcall ret_nz(gb_cpu_t *cpu, uint32_t instruction) {
	if (!get_flag_on(cpu, Z_FLAG)) {
		_ret(cpu);
		cpu->instruction_wait_cycles = 20;
	} else {
		cpu->instruction_wait_cycles = 8;
	}
}

static void ___fastcall pop_bc(gb_cpu_t *cpu, uint32_t instruction) {
	_pop(cpu, cpu->bc);
}

static void ___fastcall jp_nz_a16(gb_cpu_t *cpu, uint32_t instruction) {
	if (!get_flag_on(cpu, Z_FLAG)) {
		uint16_t immw = ((instruction & 0x00ffff00) >> 8);
		cpu->pc = immw;
		cpu->instruction_wait_cycles = 16;
	} else {
		cpu->instruction_wait_cycles = 12;
	}
}

static void ___fastcall jp_a16(gb_cpu_t *cpu, uint32_t instruction) {
	uint16_t immw = ((instruction & 0x00ffff00) >> 8);
	cpu->pc = immw;
}

static void ___fastcall call_nz_a16(gb_cpu_t *cpu, uint32_t instruction) {
	if (!get_flag_on(cpu, Z_FLAG)) {
		_push(cpu, &cpu->pc);
		uint16_t immw = ((instruction & 0x00ffff00) >> 8);
		cpu->pc = immw;
		cpu->instruction_wait_cycles = 24;
	} else {
		cpu->instruction_wait_cycles = 12;
	}
}

static void ___fastcall push_bc(gb_cpu_t *cpu, uint32_t instruction) {
	_push(cpu, cpu->bc);
}

static void ___fastcall add_a_d8(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t immb = ((instruction & 0x00ff0000) >> 16);
	uint16_t val = cpu->a + immb;
	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	set_flag(cpu, H_FLAG, (((cpu->a & 0xf) + (immb & 0xf)) & 0x10) == 0x10);
	cpu->a = val & 0xff;
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall rst_0x0(gb_cpu_t *cpu, uint32_t instruction) {
	_push(cpu, &cpu->pc);
	cpu->pc = 0;
}

static void ___fastcall ret_z(gb_cpu_t *cpu, uint32_t instruction) {
	if (get_flag_on(cpu, Z_FLAG)) {
		_ret(cpu);
		cpu->instruction_wait_cycles = 20;
	} else {
		cpu->instruction_wait_cycles = 8;
	}
}

static void ___fastcall ret(gb_cpu_t *cpu, uint32_t instruction) {
	_ret(cpu);
}

static void ___fastcall jp_z_a16(gb_cpu_t *cpu, uint32_t instruction) {
	if (get_flag_on(cpu, Z_FLAG)) {
		uint16_t immw = ((instruction & 0x00ffff00) >> 8);
		cpu->pc = immw;
		cpu->instruction_wait_cycles = 16;
	} else {
		cpu->instruction_wait_cycles = 12;
	}
}

static void ___fastcall addr_cb_prefix(gb_cpu_t *cpu, uint32_t instruction) {
	printf("we shouldn't have gotten here (CB)\n");
}

static void ___fastcall call_z_a16(gb_cpu_t *cpu, uint32_t instruction) {
	if (get_flag_on(cpu, Z_FLAG)) {
		_push(cpu, &cpu->pc);
		uint16_t immw = ((instruction & 0x00ffff00) >> 8);
		cpu->pc = immw;
		cpu->instruction_wait_cycles = 24;
	} else {
		cpu->instruction_wait_cycles = 12;
	}
}

static void ___fastcall call_a16(gb_cpu_t *cpu, uint32_t instruction) {
	_push(cpu, &cpu->pc);
	uint16_t immw = ((instruction & 0x00ffff00) >> 8);
	cpu->pc = immw;
}

static void ___fastcall adc_a_d8(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t immb = ((instruction & 0x00ff0000) >> 16);

	uint16_t val = cpu->a + immb + get_flag_on(cpu, C_FLAG);
	uint8_t low_nibble_a = cpu->a & 0xf;
	uint8_t low_nibble_nc = immb & 0xf;
	set_flag(cpu, H_FLAG, (((low_nibble_a + low_nibble_nc + get_flag_on(cpu, C_FLAG)) & 0x10)) == 0x10);
	set_flag(cpu, C_FLAG, (val & 0x100) == 0x100);
	cpu->a = val & 0xff;
	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 0);
}

static void ___fastcall rst_0x8(gb_cpu_t *cpu, uint32_t instruction) {
	_push(cpu, &cpu->pc);
	cpu->pc = 0x8;
}

static void ___fastcall ret_nc(gb_cpu_t *cpu, uint32_t instruction) {
	if (!get_flag_on(cpu, C_FLAG)) {
		_ret(cpu);
		cpu->instruction_wait_cycles = 20;
	} else {
		cpu->instruction_wait_cycles = 8;
	}
}

static void ___fastcall pop_de(gb_cpu_t *cpu, uint32_t instruction) {
	_pop(cpu, cpu->de);
}

static void ___fastcall jp_nc_a16(gb_cpu_t *cpu, uint32_t instruction) {
	if (!get_flag_on(cpu, C_FLAG)) {
		uint16_t immw = ((instruction & 0x00ffff00) >> 8);
		cpu->pc = immw;
		cpu->instruction_wait_cycles = 16;
	} else {
		cpu->instruction_wait_cycles = 12;
	}
}

static void ___fastcall call_nc_a16(gb_cpu_t *cpu, uint32_t instruction) {
	if (!get_flag_on(cpu, C_FLAG)) {
		_push(cpu, &cpu->pc);
		uint16_t immw = ((instruction & 0x00ffff00) >> 8);
		cpu->pc = immw;
		cpu->instruction_wait_cycles = 24;
	} else {
		cpu->instruction_wait_cycles = 12;
	}
}

static void ___fastcall push_de(gb_cpu_t *cpu, uint32_t instruction) {
	_push(cpu, cpu->de);
}

static void ___fastcall _sub_d8(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t immb = ((instruction & 0x00ff0000) >> 16);
	uint16_t val = cpu->a - immb;

	set_flag(cpu, C_FLAG, cpu->a < immb);
	set_flag(cpu, H_FLAG, (((cpu->a & 0xf) - (immb & 0xf)) & 0x10) == 0x10);

	cpu->a = val;

	set_flag(cpu, N_FLAG, 1);
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall _rst_0x10(gb_cpu_t *cpu, uint32_t instruction) {
	_push(cpu, &cpu->pc);
	cpu->pc = 0x10;
}

static void ___fastcall ret_c(gb_cpu_t *cpu, uint32_t instruction) {
	if (get_flag_on(cpu, C_FLAG)) {
		_ret(cpu);
		cpu->instruction_wait_cycles = 20;
	} else {
		cpu->instruction_wait_cycles = 8;
	}
}

static void ___fastcall reti(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->ime = true;
	_ret(cpu);
}

static void ___fastcall jp_c_a16(gb_cpu_t *cpu, uint32_t instruction) {
	if (get_flag_on(cpu, C_FLAG)) {
		uint16_t immw = ((instruction & 0x00ffff00) >> 8);
		cpu->pc = immw;
		cpu->instruction_wait_cycles = 16;
	} else {
		cpu->instruction_wait_cycles = 12;
	}
}

static void ___fastcall call_c_a16(gb_cpu_t *cpu, uint32_t instruction) {
	if (get_flag_on(cpu, C_FLAG)) {
		_push(cpu, &cpu->pc);
		uint16_t immw = ((instruction & 0x00ffff00) >> 8);
		cpu->pc = immw;
		cpu->instruction_wait_cycles = 24;
	} else {
		cpu->instruction_wait_cycles = 12;
	}
}

static void ___fastcall sbc_a_d8(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t immb = ((instruction & 0x00ff0000) >> 16);
	uint16_t nc = immb + get_flag_on(cpu, C_FLAG);
	uint16_t val = cpu->a - nc;
	set_flag(cpu, H_FLAG, (((cpu->a & 0xf) - (immb & 0xf) - get_flag_on(cpu, C_FLAG)) & 0x10) == 0x10);
	set_flag(cpu, C_FLAG, cpu->a < nc);
	cpu->a = val & 0xff;

	set_flag(cpu, Z_FLAG, cpu->a == 0);
	set_flag(cpu, N_FLAG, 1);
}

static void ___fastcall rst_0x18(gb_cpu_t *cpu, uint32_t instruction) {
	_push(cpu, &cpu->pc);
	cpu->pc = 0x18;
}

static void ___fastcall ldh_addr_a8_a(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t immb = ((instruction & 0x00ff0000) >> 16);
	gb_write(cpu, 0xFF00 + immb, cpu->a, 1);
}

static void ___fastcall pop_hl(gb_cpu_t *cpu, uint32_t instruction) {
	_pop(cpu, cpu->hl);
}

static void ___fastcall ld_addr_c_a(gb_cpu_t *cpu, uint32_t instruction) {
	gb_write(cpu, 0xFF00 + cpu->c, cpu->a, 1);
}

static void ___fastcall push_hl(gb_cpu_t *cpu, uint32_t instruction) {
	_push(cpu, cpu->hl);
}

static void ___fastcall and_d8(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t immb = ((instruction & 0x00ff0000) >> 16);

	set_flag(cpu, H_FLAG, 1);
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
	cpu->a &= immb;
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall rst_0x20(gb_cpu_t *cpu, uint32_t instruction) {
	_push(cpu, &cpu->pc);
	cpu->pc = 0x20;
}

static void ___fastcall add_sp_r8(gb_cpu_t *cpu, uint32_t instruction) {
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
}

static void ___fastcall jp_addr_hl(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->pc = *cpu->hl;
}

static void ___fastcall ld_addr_a16_a(gb_cpu_t *cpu, uint32_t instruction) {
	uint16_t immw = ((instruction & 0x00ffff00) >> 8);
	gb_write(cpu, immw, cpu->a, 1);
}

static void ___fastcall xor_d8(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, H_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);

	uint8_t immb = ((instruction & 0x00ff0000) >> 16);
	cpu->a ^= immb;
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall rst_0x28(gb_cpu_t *cpu, uint32_t instruction) {
	_push(cpu, &cpu->pc);
	cpu->pc = 0x28;
}

static void ___fastcall ldh_a_addr_a8(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t immb = ((instruction & 0x00ff0000) >> 16);
	cpu->a = gb_read(cpu, 0xFF00 + immb);
}

static void ___fastcall pop_af(gb_cpu_t *cpu, uint32_t instruction) {
	_pop(cpu, cpu->af);
}

static void ___fastcall ld_a_addr_c(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->a = gb_read(cpu, 0xFF00 + cpu->c);
}

static void ___fastcall di(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->ime = false;
}

static void ___fastcall push_af(gb_cpu_t *cpu, uint32_t instruction) {
	_push(cpu, cpu->af);
}

static void ___fastcall or_d8(gb_cpu_t *cpu, uint32_t instruction) {
	set_flag(cpu, N_FLAG, 0);
	set_flag(cpu, H_FLAG, 0);
	set_flag(cpu, C_FLAG, 0);
	uint8_t immb = ((instruction & 0x00ff0000) >> 16);
	cpu->a |= immb;
	set_flag(cpu, Z_FLAG, cpu->a == 0);
}

static void ___fastcall rst_0x30(gb_cpu_t *cpu, uint32_t instruction) {
	_push(cpu, &cpu->pc);
	cpu->pc = 0x30;
}

static void ___fastcall ld_hl_spir8(gb_cpu_t *cpu, uint32_t instruction) {
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
}

static void ___fastcall ld_sp_hl(gb_cpu_t *cpu, uint32_t instruction) {
	cpu->sp = *cpu->hl;
}

static void ___fastcall ld_a_addr_a16(gb_cpu_t *cpu, uint32_t instruction) {
	uint16_t immw = ((instruction & 0x00ffff00) >> 8);
	cpu->a = gb_read(cpu, immw);
}

static void ___fastcall ei(gb_cpu_t *cpu, uint32_t instruction) {
	queue_task(cpu, QUEUE_INTERRUPT_ENABLE, 0, 1);
}

static void ___fastcall cp_d8(gb_cpu_t *cpu, uint32_t instruction) {
	uint8_t immb = ((instruction & 0x00ff0000) >> 16);
	set_flag(cpu, H_FLAG, (cpu->a & 0xf) < (immb & 0xf));
	set_flag(cpu, C_FLAG, cpu->a < immb);
	set_flag(cpu, Z_FLAG, cpu->a == immb);
	set_flag(cpu, N_FLAG, 1);
}

static void ___fastcall rst_0x38(gb_cpu_t *cpu, uint32_t instruction) {
	_push(cpu, &cpu->pc);
	cpu->pc = 0x38;
}

static void execute_prefix_instruction(gb_cpu_t *cpu, uint8_t opcode);

void execute_instruction(gb_cpu_t *cpu, uint32_t instruction, FILE *assembly_dump) {
	#ifdef DEBUG
	fprintf(assembly_dump, "pc: 0x%x, sp:%04x, instruction(0x%06x): %s\naf: 0x%04x, bc: 0x%04x, de: 0x%04x, hl: 0x%04x, ime: 0x%02x, if: 0x%02x, ie: 0x%02x\n\n", 
		cpu->pc, cpu->sp, instruction >> 8, assembly_translation[(instruction & 0xff000000) >> 24], *cpu->af, *cpu->bc, *cpu->de, *cpu->hl, cpu->ime, *cpu->ifl, *cpu->ie);
	#endif
	if (instruction >> 24 == 0xcb)
		execute_prefix_instruction(cpu, (instruction & 0x00ff0000) >> 16);
	else {
		execute_main_instruction(cpu, instruction);
	}
}


void (*opcode_table[256])(gb_cpu_t *, uint32_t) = {
	nothing, ld_bc_d16, ld_addr_bc_a, inc_bc, inc_b, dec_b, ld_b_d8, rlca, ld_addr_a16_sp, add_hl_bc, ld_a_addr_bc, dec_bc, inc_c, dec_c, ld_c_d8, rrca, 
	stop, ld_de_d16, ld_addr_de_a, inc_de, inc_d, dec_d, ld_d_d8, rla, jr_r8, add_hl_de, ld_a_addr_de, dec_de, inc_e, dec_e, ld_e_d8, rra, 
	jr_nz_r8, ld_hl_d16, ld_addr_hli_a, inc_hl, inc_h, dec_h, ld_h_d8, daa, jr_z_r8, add_hl_hl, ld_a_addr_hli, dec_hl, inc_l, dec_l, ld_l_d8, cpl, 
	jr_nc_r8, ld_sp_d16, ld_addr_hld_a, inc_sp, inc_addr_hl, dec_addr_hl, ld_addr_hl_d8, scf, jr_c_r8, add_hl_sp, ld_a_addr_hld, dec_sp, inc_a, dec_a, ld_a_d8, ccf, 
	ld_b_b, ld_b_c, ld_b_d, ld_b_e, ld_b_h, ld_b_l, ld_b_addr_hl, ld_b_a, ld_c_b, ld_c_c, ld_c_d, ld_c_e, ld_c_h, ld_c_l, ld_c_addr_hl, ld_c_a, 
	ld_d_b, ld_d_c, ld_d_d, ld_d_e, ld_d_h, ld_d_l, ld_d_addr_hl, ld_d_a, ld_e_b, ld_e_c, ld_e_d, ld_e_e, ld_e_h, ld_e_l, ld_e_addr_hl, ld_e_a, 
	ld_h_b, ld_h_c, ld_h_d, ld_h_e, ld_h_h, ld_h_l, ld_h_addr_hl, ld_h_a, ld_l_b, ld_l_c, ld_l_d, ld_l_e, ld_l_h, ld_l_l, ld_l_addr_hl, ld_l_a, 
	ld_addr_hl_b, ld_addr_hl_c, ld_addr_hl_d, ld_addr_hl_e, ld_addr_hl_h, ld_addr_hl_l, _halt, ld_addr_hl_a, ld_a_b, ld_a_c, ld_a_d, ld_a_e, ld_a_h, ld_a_l, ld_a_addr_hl, ld_a_a, 
	add_a_b, add_a_c, add_a_d, add_a_e, add_a_h, add_a_l, add_a_addr_hl, add_a_a, adc_a_b, adc_a_c, adc_a_d, adc_a_e, adc_a_h, adc_a_l, adc_addr_hl, adc_a_a, 
	sub_b, sub_c, sub_d, sub_e, sub_h, sub_l, sub_addr_hl, sub_a, sbc_b, sbc_c, sbc_d, sbc_e, sbc_h, sbc_l, sbc_addr_hl, sbc_a, 
	and_b, and_c, and_d, and_e, and_h, and_l, and_addr_hl, and_a, xor_b, xor_c, xor_d, xor_e, xor_h, xor_l, xor_addr_hl, xor_a, 
	or_b, or_c, or_d, or_e, or_h, or_l, or_addr_hl, or_a, cp_b, cp_c, cp_d, cp_e, cp_h, cp_l, cp_addr_hl, cp_a, 
	ret_nz, pop_bc, jp_nz_a16, jp_a16, call_nz_a16, push_bc, add_a_d8, rst_0x0, ret_z, ret, jp_z_a16, addr_cb_prefix, call_z_a16, call_a16, adc_a_d8, rst_0x8, 
	ret_nc, pop_de, jp_nc_a16, nothing, call_nc_a16, push_de, _sub_d8, _rst_0x10, ret_c, reti, jp_c_a16, nothing, call_c_a16, nothing, sbc_a_d8, rst_0x18, 
	ldh_addr_a8_a, pop_hl, ld_addr_c_a, nothing, nothing, push_hl, and_d8, rst_0x20, add_sp_r8, jp_addr_hl, ld_addr_a16_a, nothing, nothing, nothing, xor_d8, rst_0x28, 
	ldh_a_addr_a8, pop_af, ld_a_addr_c, di, nothing, push_af, or_d8, rst_0x30, ld_hl_spir8, ld_sp_hl, ld_a_addr_a16, ei, nothing, nothing, cp_d8, rst_0x38
};

static inline void execute_main_instruction(gb_cpu_t *cpu, uint32_t instruction) {
	opcode_table[instruction >> 24](cpu, instruction);
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
					gb_write(cpu, *cpu->hl, byte, 1);
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
					gb_write(cpu, *cpu->hl, byte, 1);
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
					gb_write(cpu, *cpu->hl, byte, 1);
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

					gb_write(cpu, *cpu->hl, byte, 1);
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
					gb_write(cpu, *cpu->hl, byte, 1);
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
					gb_write(cpu, *cpu->hl, byte, 1);
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

					gb_write(cpu, *cpu->hl, byte, 1);
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
					gb_write(cpu, *cpu->hl, byte, 1);
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

				gb_write(cpu, *cpu->hl, byte, 1);
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

				gb_write(cpu, *cpu->hl, byte, 1);
			} else {
				set_bit_on(reg, bit, 1);
			}
			break;
		}
	}
}