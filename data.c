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

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <fcntl.h>
#include <sys/stat.h>

#include "raw_wrapper.h"
#include "sha256.h"

int8_t *inline_buffer = NULL;
size_t buffer_position = 0;
size_t buffer_size = 0;

void (*replay_filter)(void*, size_t) = NULL;

int32_t rw_create_capture_rank_dir(int rank, int8_t *path_buffer) {
  struct stat stat_buf;
  int8_t out_dir[] = "out";
  int32_t offset = 3;

  if (0 != stat(out_dir, &stat_buf)) {
    if (0 != mkdir(out_dir, 0755)) {
      return -1;
    }
  }

  memcpy(path_buffer, out_dir, 3);
  memcpy(&(path_buffer[offset]), "/", 1);
  offset += 1;

  sprintf(&(path_buffer[offset]), "%d", rank);

  if (0 != stat(path_buffer, &stat_buf)) {
    if (0 != mkdir(path_buffer, 0755)) {
      return -1;
    }
  }

  offset = strlen(path_buffer);

  return offset;
}

void rw_mpi_request_to_unique_string(MPI_Request *request, int8_t *out_buffer) {
  int8_t internal_buffer[32] = {0};

  sha256((void*) request, sizeof(MPI_Request), internal_buffer);

  for (size_t i = 0; i < SHA256_BYTES / sizeof(uint32_t); ++i) {
    size_t offset = sizeof(uint32_t) * i;
    sprintf(&(out_buffer[offset * 2]), "%08x", *(uint32_t*)&(internal_buffer[offset]));
  }
}

void rw_replay_filter_apply (void *buf, size_t size) {
  if (NULL != replay_filter) {
    replay_filter(buf, size);
  }
}

void rw_tape_free () {
  if (NULL != inline_buffer) {
    free(inline_buffer);
    inline_buffer = NULL;
  }
}

size_t rw_tape_read_bytes(void *buf, size_t total_size) {
  if (NULL != inline_buffer && total_size <= buffer_size - buffer_position) {
    memcpy(buf, inline_buffer + buffer_position, total_size);

    rw_replay_filter_apply(buf, total_size);

    #pragma omp atomic write
    buffer_position = buffer_position + total_size;

    return total_size;
  } else {
    return 0;
  }
}

