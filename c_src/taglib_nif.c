#include <stdlib.h>
#include <string.h>
#include "erl_nif.h"
#include "tag_c.h"

#define FFI_PROTO(X) static ERL_NIF_TERM X(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])

static ErlNifResourceType* taglib_nif_RESOURCE = NULL;

ERL_NIF_TERM ATOM_TRUE;
ERL_NIF_TERM ATOM_FALSE;
ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ERROR;

typedef struct
{
    TagLib_File * taglib_file;
} taglib_nif_handle;

// Prototypes
FFI_PROTO(taglib_nif_new_type);
FFI_PROTO(taglib_nif_tag_title);
FFI_PROTO(taglib_nif_tag_artist);
FFI_PROTO(taglib_nif_tag_album);
FFI_PROTO(taglib_nif_tag_comment);
FFI_PROTO(taglib_nif_tag_genre);
FFI_PROTO(taglib_nif_tag_year);
FFI_PROTO(taglib_nif_tag_track);

static ErlNifFunc nif_funcs[] =
{
    {"new", 1, taglib_nif_new_type},
    {"new_type", 2, taglib_nif_new_type},
    {"tag_title", 1, taglib_nif_tag_title},
    {"tag_artist", 1, taglib_nif_tag_artist},
    {"tag_album", 1, taglib_nif_tag_album},
    {"tag_comment", 1, taglib_nif_tag_comment},
    {"tag_genre", 1, taglib_nif_tag_genre},
    {"tag_year", 1, taglib_nif_tag_year},
    {"tag_track", 1, taglib_nif_tag_track}
};

/* please free() result */
char * get_null_term_string_from_binary(ErlNifBinary bin) {
    char * string = (char *)malloc(bin.size + 1);
    memcpy(string, bin.data, bin.size);
    string[bin.size] = 0;
    return string;
}

FFI_PROTO(taglib_nif_new_type)
{
    ErlNifBinary filename_bin;
    char * filename;
    TagLib_File * taglib_file = NULL;
    int type_enum;
    if (!enif_inspect_binary(env, argv[0], &filename_bin)) {
        return enif_make_badarg(env);
    }
    if (argc == 2 && !enif_get_int(env, argv[1], &type_enum)) {
        return enif_make_badarg(env);
    }
    filename = get_null_term_string_from_binary(filename_bin);
    if (argc == 1) {
        taglib_file = taglib_file_new(filename);
    } else if (argc == 2) {
        taglib_file = taglib_file_new_type(filename, type_enum);
    }
    free(filename);

    if (taglib_file == NULL) {
        return enif_make_tuple2(env, ATOM_ERROR,
                                enif_make_string(env, "File could not be opened by taglib.", ERL_NIF_LATIN1));
    }

    if (!taglib_file_is_valid(taglib_file)) {
        return enif_make_tuple2(env, ATOM_ERROR,
                                enif_make_string(env, "File is not valid.", ERL_NIF_LATIN1));
    }

    taglib_nif_handle* handle = enif_alloc_resource(taglib_nif_RESOURCE,
                                                    sizeof(taglib_nif_handle));
    handle->taglib_file = taglib_file;
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, ATOM_OK, result);
}

static ERL_NIF_TERM tag_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[],
    char *(*tag_function)(const TagLib_Tag *))
{
    taglib_nif_handle* handle;
    if (!enif_get_resource(env, argv[0], taglib_nif_RESOURCE, (void **) &handle)) {
        return enif_make_badarg(env);
    }

    TagLib_Tag * tag = taglib_file_tag(handle->taglib_file);
    char * strval = tag_function(tag);
    size_t size = strlen(strval);
    ErlNifBinary bin;
    enif_alloc_binary(size, &bin);
    memcpy(bin.data, strval, size);
    taglib_free(strval);
    return enif_make_binary(env, &bin);
}

FFI_PROTO(taglib_nif_tag_title) { return tag_string(env, argc, argv, &taglib_tag_title); }
FFI_PROTO(taglib_nif_tag_artist) { return tag_string(env, argc, argv, &taglib_tag_artist); }
FFI_PROTO(taglib_nif_tag_album) { return tag_string(env, argc, argv, &taglib_tag_album); }
FFI_PROTO(taglib_nif_tag_comment) { return tag_string(env, argc, argv, &taglib_tag_comment); }
FFI_PROTO(taglib_nif_tag_genre) { return tag_string(env, argc, argv, &taglib_tag_genre); }

static ERL_NIF_TERM tag_int(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[],
    unsigned int(*tag_function)(const TagLib_Tag *))
{
    taglib_nif_handle* handle;
    if (!enif_get_resource(env, argv[0], taglib_nif_RESOURCE, (void **) &handle)) {
        return enif_make_badarg(env);
    }

    TagLib_Tag * tag = taglib_file_tag(handle->taglib_file);
    unsigned int intval = tag_function(tag);
    return enif_make_int(env, (int) intval);
}

FFI_PROTO(taglib_nif_tag_year) { return tag_int(env, argc, argv, &taglib_tag_year); }
FFI_PROTO(taglib_nif_tag_track) { return tag_int(env, argc, argv, &taglib_tag_track); }

static void taglib_nif_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in taglib_nif_handle */
    taglib_nif_handle* handle = (taglib_nif_handle*)arg;
    taglib_file_free(handle->taglib_file);
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ATOM_TRUE = enif_make_atom(env, "true");
    ATOM_FALSE = enif_make_atom(env, "false");
    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_ERROR = enif_make_atom(env, "error");
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
