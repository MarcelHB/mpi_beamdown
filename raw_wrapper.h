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

#ifndef HEADER_RAW_WRAPPER
#define HEADER_RAW_WRAPPER

#include "mpi.h"

#define SHA256_BYTES 32
#define SHA256_HEX_CHARS 65

#define RW_MPI_CALL_ID_IRECV 1

typedef struct RW_AsyncMarker {
  uint16_t call_id;
  uint64_t call_number;
  int32_t type_count;
  MPI_Datatype type;
  int32_t source;
  int32_t tag;
  void *data_address;
} RW_AsyncMarker;

void rwi_reset_counter(void);
void rwi_set_comm_size(int);
void rwi_set_comm_size_(int*);
void rwi_set_enabled(int);
void rwi_set_enabled_(int*);
void rwi_set_replay_data(int8_t*, const size_t);
void rwi_set_replay_data_(int8_t**, size_t*);
void rwi_set_replay_filter(void (*)(void*,const size_t));
void rwi_set_replay_filter_(void (**)(void*, size_t));
void rwi_set_replay_enabled(int, int);
void rwi_set_replay_enabled_(int*, int*);

int32_t rw_create_capture_rank_dir(int rank, int8_t *path_buffer);
void    rw_mpi_request_to_unique_string(MPI_Request*, int8_t*);
void    rw_replay_filter_apply(void*, size_t);
void    rw_tape_free();
size_t  rw_tape_read_bytes(void*, size_t);

/* Calls to implement for some customized storage backend. */
void rw_capture_data(
    int rank
  , int call_number
  , const void* src_buf
  , int count_source_elements
  , MPI_Datatype type
  , int source
  , int tag
);

int rw_create_pending_request(
    int rank
  , RW_AsyncMarker *marker
  , int8_t *request_token_sha256
);

void rw_finalize();

void rw_init(int *argc, char ***argv, int rank);

int rw_obtain_pending_request(
    int rank
  , int8_t *request_token_sha256
  , RW_AsyncMarker *marker
);

void rw_replay_data(
    int rank
  , int call_number
  , void *dest_buf
  , MPI_Datatype type
  , int count_dest_elements
  , MPI_Status *result_status
);

#endif
