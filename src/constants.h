#ifndef CONSTANTS_INCLUDE
#define CONSTANTS_INCLUDE

#include <stdint.h>
#include <stdbool.h>
const bool load_bootstrap = true;
const bool max_speed = false;
const double target_frame_rate = 59.7275;
const double target_delay_time = 1000 / target_frame_rate;


char *assembly_translation[256] = {
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
#endif