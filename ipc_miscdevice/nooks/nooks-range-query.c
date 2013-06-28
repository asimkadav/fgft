#include "../../common/uprintk.h"
#include "nooks-range-query.h"

#include <linux/types.h>
#include <linux/slab.h>
#include <linux/module.h> 

#define RANGE_MAGIC 0x12345678

struct nooks_range_query_node {
    unsigned int magic;
    unsigned long kern_base;
    unsigned long user_base;
    unsigned long length_in_bytes;
    struct nooks_range_query_node *next;
};

static void nooks_range_verify(struct nooks_range_query_list *list); // Debugging

struct nooks_range_query_list *nooks_create_range_query_list (void) {
    struct nooks_range_query_list *new_list;
    new_list = kmalloc (sizeof (struct nooks_range_query_list), GFP_KERNEL);
    new_list->first = NULL;
    new_list->last = NULL;
    new_list->count = 0;
    return new_list;
}

void nooks_free_range_query_list (struct nooks_range_query_list *list) {
    int i = 0;
    int first_deleted = 0, last_deleted = 0;
    struct nooks_range_query_node *cur_node;
    struct nooks_range_query_node *next_node;

    if (list->first == NULL || list->last == NULL) {
        first_deleted = 1;
        last_deleted = 1;
        if (list->last != NULL || list->first != NULL) {
            panic ("nooks_free_range_query_list error!\n");
        }
    }

    cur_node = list->first;
    while (cur_node != NULL) {
        if (cur_node->magic != RANGE_MAGIC) {
            panic ("%s corruption RANGE_MAGIC\n", __func__);
        }
                
        next_node = cur_node->next;

        if (cur_node == list->first) {
            first_deleted = 1;
        }
        if (cur_node == list->last) {
            last_deleted = 1;
        }
        
        kfree (cur_node);
        cur_node = next_node;
        i++;
    }

    kfree (list);

    if (first_deleted == 0 || last_deleted == 0) {
        panic ("Failed to delete entire range query list?");
    }
}

void nooks_range_add (struct nooks_range_query_list *list,
                      unsigned long kern_base,
                      unsigned long user_base,
                      unsigned long length_in_bytes) {
    struct nooks_range_query_node *new_node;

    uprintk ("Adding range kern %p --> user %p.  Length: %ld\n",
             (void *) kern_base, (void *) user_base, length_in_bytes);
    
    //struct nooks_range_query_node *node;
    new_node = kmalloc (sizeof (struct nooks_range_query_node), GFP_ATOMIC);
    new_node->magic = RANGE_MAGIC;

    // list must be provided
    if (list == NULL) {
        panic ("Error in %s 0", __func__);
    }

    // If we do have a "last"
    if (list->last != NULL) {
        // Verify it's really the last
        if (list->last->next != NULL) {
            panic ("Error in %s 1", __func__);
        }
    }

    //
    // TODO: If this code is changed, consider the impact on allocation wrappers
    // which may include - 1 to compensate
    //
    // As a practical matter length_in_bytes == 0 should be a bug.
    //
    if ((long) length_in_bytes <= 0) {
        panic ("Error in %s:  length_in_bytes < 0 (%lu)!", __func__, length_in_bytes);
    }

    // ens1371 definitely can use up 64KB
    if (length_in_bytes >= 80000) {
        printk ("kern_base: %p, user_base: %p, length: %ld\n",
                (void *) kern_base, (void *) user_base, length_in_bytes);
        panic ("%s this is probably an error", __func__);
    }

    // This node will be added to the end of the list.
    new_node->kern_base = kern_base;
    new_node->user_base = user_base;
    new_node->length_in_bytes = length_in_bytes;
    new_node->next = NULL;

    if (list->first == NULL) {
        list->first = new_node;
        if (list->last != NULL) {
            panic ("Error in %s 3", __func__);
        }
    }

    if (list->last == NULL) {
        // Empty list, this is the first elt
        list->last = new_node;
    } else {
        // Non-empty list, add this elt to end
        list->last->next = new_node;
        list->last = new_node;
    }

    // After adding an element, we should always have a "last"
    if (list->last == NULL) {
        panic ("Error in %s 4", __func__);
    }
    
    if (list->last->next != NULL) {
        panic ("Error in %s 5", __func__);
    }

    // Increment number of elements in the list
    list->count++;
    if (list->count > NOOKS_MAX_SANE_LIST_SIZE) {
        panic ("Error in %s 6", __func__);
    }

    nooks_range_verify(list);
}

// Should be able to specify either parameter, no need
// to supply both.  Specify 0xFFFFFFFF for the parameter to ignore.
// Silently ignores the request if the list is empty.
void nooks_range_remove (struct nooks_range_query_list *list,
                         unsigned long kern_base,
                         unsigned long user_base) {
    struct nooks_range_query_node *node; // current node
    struct nooks_range_query_node *prev; // previous node

    if (list == NULL) {
        panic ("Error in %s\n", __func__);
    }

    uprintk ("Removing range kern %p --> user %p.\n",
             (void *) kern_base, (void *) user_base);
    
    prev = NULL;
    node = list->first;
    while (node != NULL) {
        if (node->magic != RANGE_MAGIC) {
            panic ("%s corruption RANGE_MAGIC\n", __func__);
        }

        // If the address specified matches either base
        if (node->kern_base == kern_base ||
            node->user_base == user_base) {

            // Delete the node.  Specifics depend on
            // where the node is in the linked list.
            if (node == list->first &&
                node == list->last) {
                // Deleting the only node in the list
                list->first = NULL;
                list->last = NULL;
                list->count--;
                kfree (node);
                break;
            } else if (node == list->first &&
                       node != list->last) {
                // Deleting the first node in the list.
                list->first = list->first->next;
                list->count--;
                kfree (node);
                break;
            } else if (node != list->first &&
                       node == list->last) {
                // Deleting the last node in the list
                prev->next = NULL;
                list->last = prev;
                list->count--;
                kfree (node);
                break;
            } else if (node != list->first &&
                       node != list->last) {
                // Deleting some other node in the list
                prev->next = node->next;
                list->count--;
                kfree (node);
                break;
            } else {
                // HUH?
                panic ("Error in nooks_remove_range_query_node, bug\n");
            }
        }
        
        prev = node;
        node = node->next;
    }

    nooks_range_verify(list);
}

// Given a kernel pointer, lookup the corresponding user pointer,
// if possible.  Sets *userptr = NULL if no match is found.
void nooks_range_lookup_user (struct nooks_range_query_list *list,
                              unsigned long kernptr,
                              unsigned long *userptr) {
    struct nooks_range_query_node *node;
    int offset_from_base;

    if (list == NULL) {
        panic ("Error in %s 1\n", __func__);
    }

    if (userptr == NULL) {
        panic ("Error in %s 2\n", __func__);
    }
    
    *userptr = 0;

    node = list->first;
    while (node != NULL) {
        if (node->magic != RANGE_MAGIC) {
            panic ("%s corruption RANGE_MAGIC\n", __func__);
        }

        if ((node->kern_base <= kernptr) &&
            (kernptr < node->kern_base + node->length_in_bytes)) {
            // TODO this second term should be <, but is <= to allow a pointer to
            // point one past the end of an array and still get translated.
            //
            // TODO: If this code is changed, consider the impact on allocation wrappers
            // which may include - 1 to compensate
            
            offset_from_base = kernptr - node->kern_base;
            *userptr = node->user_base + offset_from_base;
            break;
        }

        if (kernptr == node->kern_base + node->length_in_bytes) {
            uprintk ("%s: ======================================\n", __func__);
            uprintk ("%s: ======================================\n", __func__);
            uprintk ("%s: ======================================\n", __func__);
            uprintk ("%s: NOTE:  TRANSLATION - 1 BUG IS INCOMING\n", __func__);
            uprintk ("%s: ======================================\n", __func__);
            uprintk ("%s: ======================================\n", __func__);
            uprintk ("%s: ======================================\n", __func__);
        }
        node = node->next;
    }

    if (*userptr != 0) {
        uprintk ("%s:  Found userptr: 0x%lx, kernptr: 0x%lx\n", __func__, *userptr, kernptr);
    }
}

// Given a user pointer, lookup the corresponding kernel pointer,
// if possible.  Sets *kernptr = NULL if no match is found.
void nooks_range_lookup_kern (struct nooks_range_query_list *list,
                              unsigned long userptr,
                              unsigned long *kernptr) {
    struct nooks_range_query_node *node;
    int offset_from_base;

    if (list == NULL) {
        panic ("Error in %s 1\n", __func__);
    }

    if (kernptr == NULL) {
        panic ("Error in %s 2\n", __func__);
    }

    *kernptr = 0;

    node = list->first;
    while (node != NULL) {
        if (node->magic != RANGE_MAGIC) {
            panic ("%s corruption RANGE_MAGIC\n", __func__);
        }

        if ((node->user_base <= userptr) &&
            (userptr <= node->user_base + node->length_in_bytes)) {
            // TODO this second term should be <, but is <= to allow a pointer to
            // point one past the end of an array and still get translated.
            //
            // TODO: If this code is changed, consider the impact on allocation wrappers
            // which may include - 1 to compensate
            
            offset_from_base = userptr - node->user_base;
            *kernptr = node->kern_base + offset_from_base;
            break;
        }
        node = node->next;
    }

    if (*kernptr != 0) {
        uprintk ("%s:  Found kernptr: 0x%lx, userptr: 0x%lx\n", __func__, *kernptr, userptr);
    }
}

static void print_node (struct nooks_range_query_node *node) {
    uprintk ("Node addr %p, kern_base 0x%lx, user_base 0x%lx, length_in_bytes %lu, next %p\n",
             node, node->kern_base, node->user_base, node->length_in_bytes, node->next);
}

static void nooks_range_verify(struct nooks_range_query_list *list) {
    struct nooks_range_query_node *node;
    struct nooks_range_query_node *verification;

    // Simple n^2 algorithm to compare each node
    // pair to ensure all ranges are non-overlapping
    node = list->first;
    while (node != NULL) {
        verification = list->first;
        while (verification != NULL) {
            int condition1;
            int condition2;
            // Don't compare against ourselves
            if (verification == node) {
                verification = verification->next;
                continue;
            }

            // Strictly contained within -- does not include
            // the one-off issue described in the lookup functions, above.
            condition1 = (node->kern_base <= verification->kern_base) &&
                (verification->kern_base < node->kern_base + node->length_in_bytes);
            condition2 = (node->user_base <= verification->user_base) &&
                (verification->user_base < node->user_base + node->length_in_bytes);
            if (condition1 || condition2) {
                // overlap
                uprintk ("Failure:\n");
                print_node (node);
                print_node (verification);
                panic ("Failure: Overlapping ranges.");
            }

            verification = verification->next;
        }

        node = node->next;
    }
}

// Print out the entire structure.
void nooks_range_dump (struct nooks_range_query_list *list) {
    int i = 0;
    struct nooks_range_query_node *node;

    node = list->first;
    uprintk ("Count: %d First: %p, Last: %p\n", list->count, list->first, list->last);
    while (node != NULL) {
        if (node->magic != RANGE_MAGIC) {
            panic ("%s corruption RANGE_MAGIC\n", __func__);
        }

        print_node (node);
        
        node = node->next;
        i++;
    }

    nooks_range_verify(list);
}
