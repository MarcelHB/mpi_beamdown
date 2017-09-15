/*
 * Copyright (c) 2017, Marcel Heing-Becker, University of Hamburg
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
*/

#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

extern void (*replay_filter)(void*, const size_t);
extern uint64_t receive_counter;
extern uint8_t isolation_mode, wrapper_enabled;
extern int unit_rank, current_rank, comm_size;

extern size_t buffer_size, buffer_position;
extern int8_t *inline_buffer;

void rwi_reset_counter_(void) __attribute__((weak, alias("rwi_reset_counter")));
void rwi_reset_counter (void) {
  receive_counter = 0;
}

void rwi_set_comm_size(int size) {
  comm_size = size;
}

void rwi_set_comm_size_(int *size) {
  rwi_set_comm_size(*size);
}

void rwi_set_enabled (int status) {
  wrapper_enabled = status;
}

void rwi_set_enabled_ (int *status) {
  rwi_set_enabled(*status);
}

void rwi_set_replay_data(int8_t *ptr, const size_t size) {
  if (NULL == inline_buffer && size > 0) {
    inline_buffer = (int8_t*) malloc(size);
    buffer_size = size;
    buffer_position = 0;
    memcpy(inline_buffer, ptr, size);
  } else if (NULL != inline_buffer && 0 == size) {
    free(inline_buffer);
    inline_buffer = NULL;
    buffer_size = 0;
  } else if(size > 0) {
    inline_buffer = (int8_t*) realloc(inline_buffer, size);
    buffer_size = size;
    buffer_position = 0;
    memcpy(inline_buffer, ptr, size);
  }
}

void rwi_set_replay_data_(int8_t **ptr, size_t *size) {
  rwi_set_replay_data(*ptr, *size);
}

void rwi_set_replay_enabled (int status, int rank) {
  isolation_mode = status >= 1 ? 1 : 0;
  current_rank = unit_rank = rank;
}

void rwi_set_replay_enabled_ (int *status, int *rank) {
  rwi_set_replay_enabled(*status, *rank);
}

void rwi_set_replay_filter (void (*f)(void*, const size_t)) {
  replay_filter = f;
}

void rwi_set_replay_filter_ (void (**f)(void*, size_t)) {
  rwi_set_replay_filter(*f);
}
