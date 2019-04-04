#ifndef ADQC_UTIL_H
#define ADQC_UTIL_H

#include <stdint.h>
// XXX use write not printf
#include <stdio.h>

static int8_t cstr_first_char(const char* str)
{
  return *str;
}

static int32_t print_S8(int8_t n) 
{
  return printf("%hhd", n);
}

static int32_t print_U8(uint8_t n)
{
  return printf("%hhu", n);
}

static int32_t print_S16(int16_t n)
{
  return printf("%hd", n);
}

static int32_t print_U16(uint16_t n)
{
  return printf("%hu", n);
}

static int32_t print_S32(int32_t n)
{
  return printf("%d", n);
}

static int32_t print_U32(uint32_t n)
{
  return printf("%u", n);
}

static int32_t print_S64(int64_t n)
{
  return printf("%ld", n);
}

static int32_t print_U64(uint64_t n)
{
  return printf("%lu", n);
}

static int32_t print_F32(float n)
{
  return printf("%f", (double)n);
}

static int32_t printf_F64(double n)
{
  return printf("%f", n);
}

#endif

