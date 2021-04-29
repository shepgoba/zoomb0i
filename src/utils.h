#ifndef utils_h
#define utils_h

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>


#define err(x) fprintf(stderr, x)

void *read_bytes(FILE *file, uint32_t offset, size_t size);
void ram_dump(void *cpu);
void last_path_component(const char *string, char *dest);
int cap(int val, int max);

void set_bit_on(uint8_t *value, uint8_t bit, uint8_t status);
uint8_t get_bit_on(uint8_t value, uint8_t bit);

#endif