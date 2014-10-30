#include "erl_nif.h"
#include "tag_c.h"

static ErlNifResourceType* taglib_nif_RESOURCE = NULL;

typedef struct
{
    TagLib_File * taglib_file;
} taglib_nif_handle;

// Prototypes
static ERL_NIF_TERM taglib_nif_new(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM taglib_nif_new_type(ErlNifEnv* env, int argc,
                                        const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"new", 1, taglib_nif_new_type},
    {"new_type", 2, taglib_nif_new_type}
};

static ERL_NIF_TERM taglib_nif_new_type(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    ErlNifBinary filename_bin;
    unsigned char filename[1024];
    TagLib_File * taglib_file;
    int type_enum;
    if (!enif_inspect_binary(env, argv[0], &filename_bin)) {
        return enif_make_badarg(env);
    }
    if (argc == 2 && !enif_get_int(env, argv[1], &type_enum)) {
        return enif_make_badarg(env);
    }
    if (filename_bin.size > 1023) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                enif_make_string(env, "Filename is longer than 1023 bytes.", ERL_NIF_LATIN1));
    }

    memcpy(filename, filename_bin.data, filename_bin.size);
    filename[filename_bin.size] = 0; /* null terminator */
    if (argc == 1) {
        taglib_file = taglib_file_new(filename);
    } else if (argc == 2) {
        taglib_file = taglib_file_new_type(filename, type_enum);
    }
    if (taglib_file == NULL) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                enif_make_string(env, "File could not be opened by taglib.", ERL_NIF_LATIN1));
    }

    taglib_nif_handle* handle = enif_alloc_resource(taglib_nif_RESOURCE,
                                                    sizeof(taglib_nif_handle));
    handle->taglib_file = taglib_file;
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

static void taglib_nif_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in taglib_nif_handle */
    taglib_nif_handle* handle = (taglib_nif_handle*)arg;
    taglib_file_free(handle->taglib_file);
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                                                     "taglib_nif_resource",
                                                     &taglib_nif_resource_cleanup,
                                                     flags, NULL);
    if (rt == NULL)
        return -1;

    taglib_set_string_management_enabled(0);

    taglib_nif_RESOURCE = rt;

    return 0;
}

ERL_NIF_INIT(taglib_nif, nif_funcs, &on_load, NULL, NULL, NULL);
