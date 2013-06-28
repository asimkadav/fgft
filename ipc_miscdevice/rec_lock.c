#if 0
#include <linux/spinlock.h>
#include <linux/semaphore.h>
#include <linux/sched.h>
#include <linux/module.h>
#endif

#include <linux/module.h>
#include <asm/current.h>
#include <linux/thread_info.h>

void *MJR_get_current (void) {
    return current;
}

void this_is_a_test(void) {
    printk ("Fuck off");
}

EXPORT_SYMBOL(MJR_get_current);
EXPORT_SYMBOL(this_is_a_test);

#if 0

struct recursive_lock {
    // The number of times the current lock holder
    // has acquired the lock, or -1 if it's free.
    int depth;

    // The mutex
    struct semaphore sem;

    // The kernel thread that currently holds the lock.
    // The idea is to prevent any other kernel thread
    // from acquiring the lock, while still allowing this
    // kernel thread to acquire it again (recursively).
    struct task_struct *cur;

    // Used to protect this data structure.
    spinlock_t local_lock;
};

// Only can access via these three functions.
static struct recursive_lock global_lock;

void init_recursive_lock (void) {
    global_lock.depth = -1;
    sema_init (&global_lock.sem, 1);
    global_lock.cur = NULL;
    spin_lock_init (&global_lock.local_lock);
}

void acquire_recursive_lock (void) {
    for (;;) {
        spin_lock (&global_lock.local_lock);
        if (global_lock.depth == -1) {
            // In this branch, the recursive lock is free so we grab it.
            down (&global_lock.sem);

            // global_lock.cur should be NULL.
            if (global_lock.cur != NULL) {
                panic ("acquire_recursive: global_lock.cur != NULL");
            }

            // Establish ourselves as the owner of this lock
            global_lock.cur = current;
            global_lock.depth++;
            spin_unlock (&global_lock.local_lock);
            break;
        } else if (global_lock.depth >= 0 && global_lock.cur != current) {
            // In this branch, the lock is held by some thread other than
            // ourselves, so we must wait.
            spin_unlock (&global_lock.local_lock);
            down (&global_lock.sem); // Join the wait list.
            // We got the lock, and then we free it again.
            up (&global_lock.sem);

            // At this point, we've been awakened.  The thread
            // holding the recursive lock released it and awoke
            // one waiter.  We have to compete with any other
            // incoming threads, so simply try the whole process again.
            continue;
        } else if (global_lock.depth >= 0 && global_lock.cur == current) {
            // In this branch, the lock is held by our own thread!
            // Quick and easy.
            global_lock.depth++;
            spin_unlock (&global_lock.local_lock);
            break;
        } else if (global_lock.depth == -1 && global_lock.cur == current) {
            // In this branch, the lock is supposedly held by us,
            // but it's not?
            panic ("Fix the acquire_recursive function <Error 1>\n");
        } else {
            // Eh?
            panic ("Fix the acquire_recursive function <Error 2>\n");
        }
    }
}

void release_recursive_lock (void) {
    spin_lock (&global_lock.local_lock);
    if (global_lock.depth <= -1) {
        // The lock is not currently held, but has been released?
        // Bug in some other code.
        panic ("Fix the recursive lock!");
    }

    if (global_lock.depth <= -1 && global_lock.cur == current) {
        panic ("Error: We don't really own the recursive lock Error 1");
    } else if (global_lock.depth == 0 && global_lock.cur == current) {
        // Here, we need to release the lock and wake up a waiter.
        global_lock.depth--;
        global_lock.cur = NULL;
        up (&global_lock.sem);
        spin_unlock (&global_lock.local_lock);
    } else if (global_lock.depth >= 1 && global_lock.cur == current) {
        // Here, we don't need to release the lock, but we do need to
        // keep track of the current level of recursion
        global_lock.depth--;
        spin_unlock (&global_lock.local_lock);
    } else if (global_lock.depth <= -1 && global_lock.cur != current) {
        panic ("Error: We don't own the recursive lock Error 2");
    } else if (global_lock.depth == 0 && global_lock.cur != current) {
        panic ("Error: We don't own the recursive lock Error 3");
    } else if (global_lock.depth >= 1 && global_lock.cur != current) {
        panic ("Error: We don't own the recursive lock Error 4");
    } else {
        panic ("Fix the release_recursive function");
    }
}

// EXPORT_SYMBOL(init_recursive_lock)  Called from misc.c
EXPORT_SYMBOL(acquire_recursive_lock);
EXPORT_SYMBOL(release_recursive_lock);
#endif
