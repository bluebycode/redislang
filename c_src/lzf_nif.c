#include "lzf.h"
#include "erl_nif.h"
#include "erl_nif_compat.h"
#include <stdio.h>

static ERL_NIF_TERM decompress_nif(ErlNifEnv* env, int argc,
    const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"decompress", 2, decompress_nif}
};

int decompress_aux(ErlNifEnv *env, unsigned int bufsize, ErlNifBinary *src, ErlNifBinary *dest) {
  enif_alloc_binary_compat(env, bufsize, dest);
  unsigned int result = lzf_decompress(src->data, src->size, dest->data, dest->size);
  if (result) {
    enif_realloc_binary_compat(env, dest, result);
    return 1;
  }
  enif_release_binary_compat(env, dest);
  return 0;
}

static ERL_NIF_TERM decompress_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary src_bin, res_bin;
  unsigned int res_size;

  if (!enif_get_uint(env, argv[0], &res_size)) {
    return 0;
  }

  if (!enif_inspect_binary(env, argv[1], &src_bin)) {
    return 0;
  }

  if (!decompress_aux(env, res_size, &src_bin, &res_bin)){
    return enif_make_badarg(env);
  }
  return enif_make_binary(env,&res_bin);
}

ERL_NIF_INIT(lzf, nif_funcs, NULL, NULL, NULL, NULL);