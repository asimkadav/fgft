#ifndef WRAPPERS_NOOKS_H
#define WRAPPERS_NOOKS_H

// nooks ioctl request handlers
void handle_nooks_xlate_u2k (struct req_args *inarg);
void handle_nooks_xlate_k2u (struct req_args *inarg);
void handle_nooks_add_to_hash (struct req_args *inarg);
void handle_nooks_del_from_hash (struct req_args *inarg);
void handle_nooks_del_from_hash_reverse (struct req_args *inarg);
void handle_nooks_register_userfn (struct req_args *inarg);
void handle_nooks_memory_assoc (struct req_args *inarg);
void handle_nooks_get_array_numelts_reverse (struct req_args *inarg);
void handle_nooks_store_array_numelts_reverse (struct req_args *inarg);
void handle_nooks_range_update (struct req_args *inarg);
void handle_nooks_range_free (struct req_args *inarg);
void handle_nooks_range_add (struct req_args *inarg);
void handle_nooks_debug_translate_u2k (struct req_args *inarg);

#endif
