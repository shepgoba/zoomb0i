#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include "cpu.h"
#include "utils.h"

void set_bit_on(uint8_t *value, uint8_t bit, uint8_t status) 
{
	*value ^= (-status ^ *value) & (1UL << bit);
}

uint8_t get_bit_on(uint8_t value, uint8_t bit) 
{
	return (value >> bit) & 1;
}

void *read_bytes(FILE *file, uint32_t offset, size_t size) {
	void *buffer = calloc(sizeof(char), size);
	fseek(file, offset, SEEK_SET);
	fread(buffer, size, sizeof(char), file);
	return buffer;
}

void ram_dump(void *cpu) {
	FILE *dump = fopen("ramdump.bin", "wb+");
	for (int i = 0; i < 0x10000; i++) {
		fwrite(((gb_cpu_t *)cpu)->address_space + i, sizeof(uint8_t), 1, dump);
	}
	fclose(dump);
}


int cap(int val, int max) 
{
	if (val > max)
		return max;
	return val;
}

void last_path_component(const char *string, char *dest)
{
	if (string == NULL || dest == NULL)
		return;

	#ifdef __WIN32__
    char c = '\\';
    #else
    char c = '/';
    #endif

	for (int i = strlen(string); i > 0; i--) {
		if (string[i] == c) {
			strcpy(dest, string + i + 1);
			return;
		}
	}
	strcpy(dest, string);
}