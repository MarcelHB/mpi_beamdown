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

static int rw_create_capture_sink(int, int);

void rw_capture_data (
    int rank
  , int call
  , const void *buf
  , int count
  , MPI_Datatype datatype
  , int source
  , int tag
  ) {
  assert(count >= 0);

  int fd = rw_create_capture_sink(rank, call);

  assert(-1 != fd);

  MPI_Status mpi_status;
  assert(MPI_SUCCESS == MPI_Status_set_elements(&mpi_status, datatype, count));
  assert(MPI_SUCCESS == MPI_Status_set_cancelled(&mpi_status, 0));

  mpi_status.MPI_TAG = tag;
  mpi_status.MPI_ERROR = MPI_SUCCESS;
  mpi_status.MPI_SOURCE = source;

  assert(sizeof(MPI_Status) == pwrite(fd, &mpi_status, sizeof(MPI_Status), 0));

  int type_size = 0;
  size_t total_size = 0;

  assert(MPI_SUCCESS == MPI_Type_size(datatype, &type_size));
  total_size = type_size * count;

  void *pack_buffer = malloc(total_size);
  int position = 0;
  assert(MPI_SUCCESS == MPI_Pack(buf, count, datatype, pack_buffer, total_size, &position, MPI_COMM_WORLD));

  assert(total_size == pwrite(fd, pack_buffer, total_size, 0));

  free(pack_buffer);
  close(fd);
}

static int rw_create_capture_sink(int rank, int call) {
  int8_t path_buf[516] = { 0 };
  int32_t offset = rw_create_capture_rank_dir(rank, path_buf);
  assert(-1 != offset);

  memcpy(&(path_buf[offset]), "/", 1);
  offset += 1;

  sprintf(&(path_buf[offset]), "%x", call);

  return creat(path_buf, S_IRUSR | S_IWUSR);
}

int rw_create_pending_request (
    int rank
  , RW_AsyncMarker *marker
  , int8_t *request_name) {

  struct stat stat_buf;
  int8_t path_buffer[516] = { 0 };
  int32_t offset = rw_create_capture_rank_dir(rank, path_buffer);
  int8_t subdir_buffer[] = "pending";
  assert(-1 != offset);

  memcpy(&(path_buffer[offset]), "/", 1);
  offset += 1;

  memcpy(&(path_buffer[offset]), subdir_buffer, 8);

  if (0 != stat(path_buffer, &stat_buf)) {
    if (0 != mkdir(path_buffer, 0755)) {
      return -1;
    }
  }
  offset += 7;

  memcpy(&(path_buffer[offset]), "/", 1);
  offset += 1;

  memcpy(&(path_buffer[offset]), request_name, SHA256_HEX_CHARS);

  int fd = creat(path_buffer, S_IRUSR | S_IWUSR);
  assert(-1 != fd);

  assert(sizeof(RW_AsyncMarker) == pwrite(fd, (void*) marker, sizeof(RW_AsyncMarker), 0));

  close(fd);

  return 0;
}

static int rw_get_replay_source (int rank, int call) {
  int8_t path_buf[516] = {0};
  struct stat stat_buf;

  sprintf(path_buf, "out/%d/%x", rank, call);

  if (0 != stat(path_buf, &stat_buf)) {
    return -1;
  }

  return open(path_buf, O_RDONLY);
}


int rw_obtain_pending_request (
    int rank
  , int8_t *request_name
  , RW_AsyncMarker *marker) {
  int8_t path_buf[516] = {0};
  struct stat stat_buf;

  sprintf(path_buf, "out/%d/pending/%s", rank, request_name);

  if (0 != stat(path_buf, &stat_buf)) {
    return -1;
  }

  int fd = open(path_buf, O_RDONLY);

  assert(sizeof(RW_AsyncMarker) == pread(fd, (void*) marker, sizeof(RW_AsyncMarker), 0));

  close(fd);
  unlink(path_buf);

  return 0;
}

void rw_replay_data (
    int rank
  , int call
  , void *buf
  , MPI_Datatype datatype
  , int count
  , MPI_Status *status) {
  int fd = rw_get_replay_source(rank, call);

  int type_size = 0;
  size_t total_size = 0;
  int position = 0;

  assert(MPI_SUCCESS == MPI_Type_size(datatype, &type_size));
  total_size = type_size * count;

  if (-1 != fd) {
    assert(sizeof(MPI_Status) == pread(fd, status, sizeof(MPI_Status), 0));

    if (0 == rw_tape_read_bytes(buf, total_size)) {
      void *pack_buffer = malloc(total_size);

      assert(-1 != pread(fd, pack_buffer, total_size, 0));

      rw_replay_filter_apply(pack_buffer, total_size);

      assert(MPI_SUCCESS == MPI_Unpack(pack_buffer, total_size, &position, buf, count, datatype, MPI_COMM_WORLD));

      free(pack_buffer);
    }

    close(fd);
  } else if (0 != rw_tape_read_bytes(buf, total_size)) {
      /* Simple guessing. */
      MPI_Status_set_elements(status, datatype, count);
      MPI_Status_set_cancelled(status, 0);
      status->MPI_SOURCE = rank;
      status->MPI_TAG = MPI_ANY_TAG;
      status->MPI_ERROR = MPI_SUCCESS;
  } else {
    assert(0);
  }
}


