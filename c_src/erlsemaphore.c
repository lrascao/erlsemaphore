/* -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Luis Rascão.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% ------------------------------------------------------------------- */
#include "erl_nif.h"

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <errno.h>

#include "uthash.h"

#define MAX_SEMAPHORE_NAME 64
#define SEMAPHORE_PREFIX "/tmp/erlsemaphore.sem."

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_NOT_FOUND;
static ERL_NIF_TERM ATOM_ALREADY_EXISTS;
static ERL_NIF_TERM ATOM_SHMEM_CREATION_FAILED;
static ERL_NIF_TERM ATOM_WOULD_BLOCK;
static ERL_NIF_TERM ATOM_INVALID;

typedef struct {
  char name[MAX_SEMAPHORE_NAME];
  int semid;
  UT_hash_handle hh; /* makes this structure hashable */
} semaphore_hashed_t;

semaphore_hashed_t *semaphores = NULL;

/*********************************************************************/

static ERL_NIF_TERM
nif_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2 || !enif_is_atom(env, argv[0]) ||
        !enif_is_number(env, argv[1])) {
      return enif_make_badarg(env);
    }
    char name[MAX_SEMAPHORE_NAME];
    enif_get_atom(env, argv[0], name, MAX_SEMAPHORE_NAME, ERL_NIF_LATIN1);
    int n;
    enif_get_int(env, argv[1], &n);

    /* first make sure there isn't already one semaphore with the same name */
    semaphore_hashed_t *semaphore = NULL;
    HASH_FIND_STR(semaphores, name, semaphore);
    if (semaphore != NULL)
      return enif_make_tuple2(env, ATOM_ERROR, ATOM_ALREADY_EXISTS);

    char filename[256];
    strcpy(filename, SEMAPHORE_PREFIX);
    strcat(filename, name);
    FILE *fp = fopen(filename, "ab+");
    fclose(fp);

    int semid = semget(ftok(filename, 1), n, IPC_CREAT | 0666);
    if (semid == -1)
      return enif_make_tuple2(env, ATOM_ERROR, ATOM_SHMEM_CREATION_FAILED);

    semaphore = (semaphore_hashed_t *) malloc(sizeof(semaphore_hashed_t));
    semaphore->semid = semid;
    strcpy(semaphore->name, name);
    HASH_ADD_STR(semaphores, name, semaphore);

    return enif_make_tuple2(env, ATOM_OK, argv[0]);
}

static ERL_NIF_TERM
nif_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1 || !enif_is_atom(env, argv[0])) {
      return enif_make_badarg(env);
    }
    char name[MAX_SEMAPHORE_NAME];
    enif_get_atom(env, argv[0], name, MAX_SEMAPHORE_NAME, ERL_NIF_LATIN1);

    /* first make sure there isn't already one semaphore with the same name */
    semaphore_hashed_t *semaphore = NULL;
    HASH_FIND_STR(semaphores, name, semaphore);
    if (semaphore != NULL)
      return enif_make_tuple2(env, ATOM_OK, argv[0]);

    char filename[256];
    strcpy(filename, SEMAPHORE_PREFIX);
    strcat(filename, name);
    FILE *fp = fopen(filename, "ab+");
    fclose(fp);

    int semid = semget(ftok(filename, 1), 0, 0);
    if (semid == -1)
      return enif_make_tuple2(env, ATOM_ERROR, ATOM_NOT_FOUND);

    semaphore = (semaphore_hashed_t *) malloc(sizeof(semaphore_hashed_t));
    semaphore->semid = semid;
    strcpy(semaphore->name, name);
    HASH_ADD_STR(semaphores, name, semaphore);

    return enif_make_tuple2(env, ATOM_OK, argv[0]);
}

static ERL_NIF_TERM
nif_delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1 || !enif_is_atom(env, argv[0])) {
      return enif_make_badarg(env);
    }
    char name[MAX_SEMAPHORE_NAME];
    enif_get_atom(env, argv[0], name, MAX_SEMAPHORE_NAME, ERL_NIF_LATIN1);

    /* first make sure there is a semaphore with the same name */
    semaphore_hashed_t *semaphore = NULL;
    HASH_FIND_STR(semaphores, name, semaphore);
    if (semaphore == NULL)
      return enif_make_tuple2(env, ATOM_ERROR, ATOM_NOT_FOUND);

    HASH_DEL(semaphores, semaphore);
    if (semctl(semaphore->semid, 0, IPC_RMID, NULL) == -1)
      return enif_make_tuple2(env, ATOM_ERROR, ATOM_INVALID);
    free(semaphore);

    return ATOM_OK;
}

static ERL_NIF_TERM
scheduled_nif_wait(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char name[MAX_SEMAPHORE_NAME];
    enif_get_atom(env, argv[0], name, MAX_SEMAPHORE_NAME, ERL_NIF_LATIN1);
    int n;
    enif_get_int(env, argv[1], &n);
    int no_wait;
    enif_get_int(env, argv[2], &no_wait);

    /* first make sure there is a semaphore with the same name */
    semaphore_hashed_t *semaphore = NULL;
    HASH_FIND_STR(semaphores, name, semaphore);
    if (semaphore == NULL)
      return enif_make_tuple2(env, ATOM_ERROR, ATOM_NOT_FOUND);

    // http://www.tldp.org/LDP/lpg/node52.html#SECTION00743400000000000000
    struct sembuf op;
    op.sem_num = 0;
    // If sem_op is negative, then its value is subtracted from the semaphore.
    // This correlates with obtaining resources that the semaphore controls or monitors access of.
    // If IPC_NOWAIT is not specified, then the calling process sleeps until the requested
    // amount of resources are available in the semaphore (another process has released some).
    op.sem_op = -n;
    op.sem_flg = IPC_NOWAIT;

    if (semop(semaphore->semid, &op, 1) == -1) {
      if (errno == EAGAIN) {
        if (no_wait == 1)
          return enif_make_tuple2(env, ATOM_ERROR, ATOM_WOULD_BLOCK);
        else
          return enif_schedule_nif(env, "scheduled_nif_wait", 0, scheduled_nif_wait, 3, argv);
      }
    }

    return ATOM_OK;
}

static ERL_NIF_TERM
nif_wait(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
   if (argc != 3 || !enif_is_atom(env, argv[0])
                 || !enif_is_number(env, argv[1])
                 || !enif_is_number(env, argv[2])) {
      return enif_make_badarg(env);
    }

    return enif_schedule_nif(env, "scheduled_nif_wait", 0, scheduled_nif_wait, 3, argv);
}

static ERL_NIF_TERM
nif_signal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
   if (argc != 2 || !enif_is_atom(env, argv[0])
                 || !enif_is_number(env, argv[1])) {
      return enif_make_badarg(env);
    }
    char name[MAX_SEMAPHORE_NAME];
    enif_get_atom(env, argv[0], name, MAX_SEMAPHORE_NAME, ERL_NIF_LATIN1);
    int n;
    enif_get_int(env, argv[1], &n);

    /* first make sure there is a semaphore with the same name */
    semaphore_hashed_t *semaphore = NULL;
    HASH_FIND_STR(semaphores, name, semaphore);
    if (semaphore == NULL)
      return enif_make_tuple2(env, ATOM_ERROR, ATOM_NOT_FOUND);

    // http://www.tldp.org/LDP/lpg/node52.html#SECTION00743400000000000000
    struct sembuf op;
    op.sem_num = 0;
    // If sem_op is negative, then its value is subtracted from the semaphore.
    // This correlates with obtaining resources that the semaphore controls or monitors access of.
    // If IPC_NOWAIT is not specified, then the calling process sleeps until the requested
    // amount of resources are available in the semaphore (another process has released some).
    op.sem_op = n;
    op.sem_flg = IPC_NOWAIT;
    if (semop(semaphore->semid, &op, 1) == -1)
      return enif_make_tuple2(env, ATOM_ERROR, ATOM_INVALID);

    return ATOM_OK;
}

/*********************************************************************/

static void init(ErlNifEnv *env)
{
  ATOM_OK = enif_make_atom(env, "ok");
  ATOM_ERROR = enif_make_atom(env, "error");
  ATOM_NOT_FOUND = enif_make_atom(env, "not_found");
  ATOM_ALREADY_EXISTS = enif_make_atom(env, "already_exists");
  ATOM_SHMEM_CREATION_FAILED = enif_make_atom(env, "shmem_creation_failed");
  ATOM_WOULD_BLOCK = enif_make_atom(env, "would_block");
  ATOM_INVALID = enif_make_atom(env, "invalid");
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  init(env);
  return 0;
}

static int on_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
                      ERL_NIF_TERM load_info)
{
  init(env);
  return 0;
}

static void on_unload(ErlNifEnv *env, void *priv_data)
{
}

/*********************************************************************/

static ErlNifFunc nif_funcs[] = {
  {"nif_new", 2, nif_new},
  {"nif_get", 1, nif_get},
  {"nif_delete", 1, nif_delete},
  {"nif_wait", 3, nif_wait},
  {"nif_signal", 2, nif_signal}
};

ERL_NIF_INIT(erlsemaphore, nif_funcs, &on_load, NULL, &on_upgrade, &on_unload)
