#pragma once

#include <cstddef>
#include <string>

using namespace std::literals;
using std::size_t;

typedef std::uint8_t byte_t, bank_t;
typedef std::uint16_t word_t;
typedef std::uint32_t long_t;
typedef size_t index_t, n_t;


inline long_t hex2snes(long_t address)
{
    return address << 1 & 0xFF0000 | address & 0xFFFF | 0x808000;
}

inline long_t snes2hex(long_t address)
{
    return address >> 1 & 0x3F8000 | address & 0x7FFF;
}
