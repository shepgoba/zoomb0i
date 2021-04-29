#ifndef rom_h
#define rom_h

#include <stdint.h>

struct rom_header {
	uint8_t entry_point[4];
	uint8_t nintendo_logo[48];
	char title[16];
	uint16_t license_code;
	uint8_t sgb_flag;
	uint8_t cartridge_type;
	uint8_t rom_size;
	uint8_t ram_size;
	uint8_t destination_code;
	uint8_t old_license_code;
	uint8_t header_checksum;
	uint16_t rom_checksum;
};

#endif