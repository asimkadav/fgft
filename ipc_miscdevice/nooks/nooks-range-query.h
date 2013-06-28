#ifndef NOOKS_RANGE_QUERY_H
#define NOOKS_RANGE_QUERY_H

struct nooks_range_query_node;

struct nooks_range_query_list {
    // The first element in the list
    struct nooks_range_query_node *first;

    // The last element in the list
    struct nooks_range_query_node *last;
    
    // Purely for debugging.  If this gets too high we have a problem.
    int count;
};

// > 1000 ranges suggests a problem. exists
#define NOOKS_MAX_SANE_LIST_SIZE 1000

#define NOOKS_RANGE_REMOVE_IGNORE 0xFFFFFFFF

struct nooks_range_query_list *nooks_create_range_query_list (void);
void nooks_free_range_query_list (struct nooks_range_query_list *);

void nooks_range_add (struct nooks_range_query_list *list,
                      unsigned long kern_base,
                      unsigned long user_base,
                      unsigned long length_in_bytes);
void nooks_range_remove (struct nooks_range_query_list *list,
                         unsigned long kern_base,
                         unsigned long user_base);
void nooks_range_lookup_user (struct nooks_range_query_list *list,
                              unsigned long kernptr,
                              unsigned long *userptr);
void nooks_range_lookup_kern (struct nooks_range_query_list *list,
                              unsigned long userptr,
                              unsigned long *kernptr);
void nooks_range_dump (struct nooks_range_query_list *list);

#endif
