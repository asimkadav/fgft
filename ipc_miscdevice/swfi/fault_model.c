/*
 * fault-model.c -- fault injection code for drivers
 *
 * Copyright (C) 2003 Mike Swift
 * Copyright (c) 1999 Wee Teck Ng
 *
 * The source code in this file can be freely used, adapted,
 * and redistributed in source or binary form, so long as an
 * acknowledgment appears in derived source files.  No warranty 
 * is attached; * we cannot take responsibility for errors or 
 * fitness for use.
 *
 */


/*
 * Fault injector for testing the usefulness of NOOKS
 * 
 * Adapted from the SWIFI tools used by Wee Teck Ng to evaluate the RIO
 * file cache at the University of Michigan
 * 
 */

/* 
 * This tool can inject faults into modules, whether they are loaded into a 
 * nook or loaded into the kernel (for comparison testing).
 * 
 * There are several classes of faults emulated:
 * - Corruption of text
 *    - corruption
 *    - simulated programming faults
 *         - skip initialization (immediate write to EBP-x)
 *         - remove instruction (replace with NOP)
 *	   - incorrect source/destination (corrupted)
 *         - remove jmp or rep instruction
 *         - change address computation for memory access (not stack)
 *	   - change termination condition for loop (change repeat to repeat 
 *           -while equal, change condition to !condition
	   - remove instructions loading registers from arguments (ebp+x)
 *        
 * - Corruption of stack
 * - Corruption of heap
 * - copy overruns
 * - use after free
 */

#include <linux/kernel.h>
#include <linux/kallsyms.h>
#include <asm/delay.h>
#include <asm/page.h>
#include "ddb.h"
#include "db_sym.h"
#include "swifi.h"


#define CRASH_INTERVAL	8192
#define FI_MASK			0xfff
#define P50     0x3fffffff      /* 50% of max rand */
#define P94     0x7851eb84      /* 94% of max rand */
#ifdef NOP
#undef NOP
#endif
#define NOP		0x90

unsigned long randomSeed=0;		/* random number */
unsigned long injectFault=1;		/* inject fault ? */
unsigned long diskTest=0;	        /* run disk test instead of rio */
unsigned long faultInjected=0;	        /* has fault been injected? */
unsigned long crashInterval=0;	        /* interval between injecting fault */
unsigned long crashCount=0;	        /* number of times fault is injected */
unsigned long faultType;			 
unsigned long numFaults;
char *crashAddr=0;		        /* track current malloc */
int crashToggle=1;
int text_fault(pnook_t nook, struct module * module, pswifi_result_t res);
int stack_fault(pnook_t nook, pswifi_result_t res);
int heap_fault(pnook_t nook, pswifi_result_t res);
int direct_fault(int fault_address, int fault_content, pswifi_result_t res);
int direct_fault1(int fault_address, int fault_content, pswifi_result_t res);
int while1(void);

int *testVA;

#if 0
#define printk(fmt, args...) \
do { \
      printk( KERN_ALERT "NOOKS: " fmt, ## args); \
} while (0)
#else
#define printk(fmt, args...)
#endif


long 
sys_inject_fault(char * nook_name,
		 unsigned long argFaultType,
		 unsigned long argRandomSeed,
		 unsigned long argNumFaults,
		 pswifi_result_t result_record,
		 unsigned long argInjectFault)
{   
  int result = 0;
  unsigned long fault_address = 0; 
  unsigned long fault_data = 0 ; 
  int namelen = 0;
  char * kern_name = NULL;
  pnook_t nook = NULL;
  struct module * mod = NULL;
  pswifi_result_t res = NULL;

  if (argNumFaults > SWIFI_MAX_FAULTS) {
    result = -E2BIG;
    goto Cleanup;
  }
  res = (pswifi_result_t) kmalloc((1+argNumFaults) * sizeof(swifi_result_t), 
				  GFP_KERNEL);
  if (res == NULL) {
    result = -ENOMEM;
    goto Cleanup;
  }
  memset(res, 0, (1 + argNumFaults) * sizeof(swifi_result_t));
  
  //
  // Capture the name of the nook from usermode
  //

  result = nooks_get_user_string(nook_name, &kern_name, &namelen);
  if (result < 0) {
    goto Cleanup;
  }



  if ((argFaultType & MODULE_TEST) != 0) {
    int found = 0;
    argFaultType &= ~MODULE_TEST;

  switch(faultType)
    {
    case DIRECT_FAULT:
    case DIRECT_FAULT1: 
    case STACK_FAULT: 
    case HEAP_FAULT: 
    case WHILE1_FAULT: 
    case COW_FAULT:
    case DEBUGGER_FAULT:
      result = -EINVAL;
      goto Cleanup;
      break;
    case TEXT_FAULT: 
    case INIT_FAULT:
    case NOP_FAULT: 
    case DST_FAULT: 
    case SRC_FAULT: 
    case BRANCH_FAULT: 
    case PTR_FAULT: 
    case LOOP_FAULT: 
    case INTERFACE_FAULT: 
    case IRQ_FAULT:
      break;
    case FREE_FAULT: 
    case BCOPY_FAULT: 
    case SYNC_FAULT:
    case ALLOC_FAULT:
    case MEM_LEAK_FAULT: 
    case PANIC_FAULT: 
      break;
    default: 
      result = -EINVAL;
      goto Cleanup;
      break;
    }

    lock_kernel();

    for (mod = module_list; mod ; mod = mod->next) {
      if (strcmp(kern_name, mod->name) == 0) {
	found = 1;
	break;
      }
    }
    unlock_kernel(); 
    if (!found) {
      result = -ENOENT;
      goto Cleanup;
    }
		
  } else {

    nook = nooks_find_nook(kern_name);
    if (nook == NULL) {
      result = -ENOENT;
      goto Cleanup;
    }

    //
    // Allow the nook to write to the state necessary to inject faults
    //
    nooks_mm_update_pagetable(nook,
			      NOOKS_WRITEABLE, 
			      (unsigned long) &randomSeed,
			      (unsigned long) ((char *) &crashToggle - 
					       (char *) &randomSeed));
    
  }
  numFaults = argNumFaults;
  faultType = argFaultType;
  randomSeed = argRandomSeed;
  injectFault = argInjectFault;


  if(faultType>=DISK_TEST) {
    faultType=faultType-DISK_TEST;
    diskTest=1;
  }
  if(faultType==STATS) {
#if 0    
    extern long time_vmp, n_vmp;
    extern long time_pmp, n_pmp;

    printk("# vm_map_protect=%ld, total cycle=%ld\n", n_vmp, time_vmp);
    printk("# pmap_protect=%ld, total cycle=%ld\n", n_pmp, time_pmp);
    n_vmp=0; time_vmp=0;
    n_pmp=0; time_pmp=0;
#endif
  } else if (faultType == DIRECT_FAULT) {
    fault_address = numFaults;
    fault_data = randomSeed;
    printk("sys inject fault, type %ld, addr=%lx, flip bit%lx\n", 
	   faultType, fault_address, fault_data);
  } else if (faultType == DIRECT_FAULT1) {
    fault_address = numFaults;
    fault_data = randomSeed;
    printk("sys inject fault, type %ld, addr=%lx, zero bytes %lx\n", 
	   faultType, fault_address, fault_data);
  } else {
    printk("sys inject fault, type %ld, seed=%ld, fault=%ld, config=%ld\n", 
	   faultType, randomSeed, numFaults, config);
  }
  faultInjected=1;
  
  srandom(randomSeed);
  /* set warm reboot, leave RAM unchanged  
   * 0 : don't inject fault
   * 1 : run POST, wipe out memory
   * 2 : don't test memory
   * 3 : don't change memory (doesn't work)
   * 4 : don't sync registry
   */
  
  /* default number of faults is 5 */
  if(numFaults<=0 || numFaults>100) numFaults=5;
  
  switch(faultType)
    {
    case TEXT_FAULT: 
      result = text_fault(nook, mod, res); 
      break;
    case STACK_FAULT: 
      result = stack_fault(nook, res); 
      break;
    case HEAP_FAULT: 
      result = heap_fault(nook, res); 
      break;
    case INIT_FAULT:
    case NOP_FAULT: 
    case DST_FAULT: 
    case SRC_FAULT: 
    case BRANCH_FAULT: 
    case PTR_FAULT: 
    case LOOP_FAULT: 
    case INTERFACE_FAULT: 
    case IRQ_FAULT:
      result = text_fault(nook, mod, res); 
      break;
    case FREE_FAULT: 
    case BCOPY_FAULT: 
    case SYNC_FAULT:
    case ALLOC_FAULT:
      crashInterval=CRASH_INTERVAL; 	/* interval between crash */
      break;
    case MEM_LEAK_FAULT: 
      crashToggle=0;
      crashInterval=CRASH_INTERVAL; 	/* interval between crash */
      break;
    case PANIC_FAULT: 
      panic("testing panic"); 
      result = 0;
      break;
      /*        case WP_FAULT: page_reg_fault(random()); break; */
    case DIRECT_FAULT:
      {
	pnook_t old_nook = GET_CURRENT_NOOK();
	SET_CURRENT_NOOK(nook);
	result = nooks_driver_call(direct_fault, 3, fault_address, fault_data, res);
	SET_CURRENT_NOOK(old_nook);
	break;
      }
    case DIRECT_FAULT1: 
      {
	pnook_t old_nook = GET_CURRENT_NOOK();
	SET_CURRENT_NOOK(nook);
	result = nooks_driver_call(direct_fault1, 3, fault_address, fault_data, res);
	SET_CURRENT_NOOK(old_nook);
	
	break;
      }
      /*    	case PAGE_REG_DUMP: rio_dump(); break; */
    case WHILE1_FAULT: 
      {
	pnook_t old_nook = GET_CURRENT_NOOK();
	
	SET_CURRENT_NOOK(nook);
	result = nooks_driver_call(while1, 0);
	SET_CURRENT_NOOK(old_nook);
	
	break;
      }
      /* case CPU_RESET_FAULT: cpu_reset(); break; */;
    case COW_FAULT: 
      {
				/* test writing to kernel text. freebsd currently do a COW on a
				 * write to kernel text.
				 */
	unsigned long *addr1, *addr2;
	
	addr1 = (unsigned long *) 0xf0212000;
	addr2 = (unsigned long *) 0xf0212010;
	printk("%p=%lx, %p=%lx\n", addr1, *addr1, addr2, *addr2);
	__asm__ ("movl $0xf0212000, %eax\n\t" \
		 "movl $6, 0(%eax)\n\t" \
		 "movl $6, 4(%eax)\n\t");
	addr1 = (unsigned long *) 0xf0212000;
	addr2 = (unsigned long *) 0xf0212010;
	printk("after injecting fault\n");
	printk("%p=%lx, %p=%lx\n", addr1, *addr1, addr2, *addr2);
	result = 0;
	break;
      }
    
    case DEBUGGER_FAULT: 
      printk("Debugger fault"); 
      __asm__ ("movl %cr4, %ecx\n\t" \
	       "movl $42, %ecx; .byte 0x0f, 0x32\n\t" \
	       "movl $377, %ecx; .byte 0x0f, 0x32\n\t");
      result = 0;
      break;
    default: printk("unknown fault type %ld\n", faultType); break;
    }
  if (copy_to_user(result_record, res, argNumFaults * sizeof(swifi_result_t))) {
    result = -EFAULT;
  }
 Cleanup:
  if (kern_name != NULL) {
    nooks_free_user_string(kern_name);
  }
  if (nook != NULL) {
    nooks_dereference_nook(nook);
  }
  if (res != NULL) {
    kfree(res);
  }

  return (result);
}

int while1(void)
{
  int i=0;

  printk("entering into while 1 loop\n");
  while(1) { 
    udelay(20000); 
    printk("delay %4d secs, cpl=0x%x, ipend=0x%x\n", i+=5, 20, 30); 
    if(i>(100 * 2500)) 
      break;
  }
  return(0);
}


int direct_fault(int fault_address, int fault_content, pswifi_result_t res)
{   
  unsigned long *addr;
  int flip_bit=0;


  addr = (unsigned long *) (PAGE_OFFSET + fault_address);

  printk("%p:0x%lx => ", addr, *addr);
  
  flip_bit = 1 << fault_content;

  res[0].address = (unsigned long) addr;
  res[0].old = *addr;
  res[0].new = (*addr) ^ flip_bit;

  if (injectFault) {
    *addr = (*addr) ^ flip_bit; 
  }
  printk("%lx\n", *addr);
  return(0);
}

int direct_fault1(int fault_address, int fault_content, pswifi_result_t res)
{   
  unsigned long *addr, data;


  addr = (unsigned long *) (PAGE_OFFSET + fault_address);
  
  printk("%p:%lx => ", addr, *addr);
  
  
  data = *addr;
  if(fault_content==1) {
    data = data & 0xffffff00;
    data = data | 0x00000090;
  } else if(fault_content==2) {
    data = data & 0xffff0000;
    data = data | 0x00009090;
  } else if(fault_content==3) {
    data = data & 0xff000000;
    data = data | 0x00909090;
  } else if(fault_content==4) {
    data = 0x90909090;
  } 
  res[0].address = (unsigned long) addr;
  res[0].old = *addr;
  res[0].new = data;
  if (injectFault) {
    *addr = data;
  }

  printk("%lx\n", *addr);
  
    
  return(0);
}



//
// Corrupt the stack by randomly flipping a bit on the stack page. We need to 
// find an active stack executing inside a nook - if we look at other tasks 
// in the system, and fine one that is executing in a nook, then we can find 
// the stack page in use and corrupt it.
//

pnook_thread_state_t
swifi_check_task_history(struct task_struct * task,
			 pnook_t nook)
{
  pnook_thread_state_t thread_state = &(task->nook_state.thread_state);

  for (; thread_state->kernel_frame != NULL; 
       thread_state = &thread_state->kernel_frame->thread_state) {
    if (thread_state->nook == nook) {
      return(thread_state);
    }
  }   
  return(NULL);
}

#include <linux/sched.h>

#define MAX_NUM_TASKS 20

pnook_thread_state_t
find_nooks_stack(pnook_t nook)
{
  struct task_struct * task = NULL;
  pnook_thread_state_t result = NULL;
  int i,j;
  i = 1 + (random() % MAX_NUM_TASKS);
  j = i;

  
  do {
    read_lock(&tasklist_lock);
    for_each_task(task) { 
      if (swifi_check_task_history(task, nook)) {
	if (--i == 0) {
	  result = &task->nook_state.thread_state;
	  break;
	}
      }
    }
    read_unlock(&tasklist_lock);
  } while ((i > 0) && (i != j));

  return(result);
}

int
stack_fault(pnook_t nook,
	    pswifi_result_t res)
{   
  pnook_thread_state_t thread_state = NULL;
  unsigned long *addr, size, taddr;
  int flip_bit=0;
  int count=0;

  while(count < numFaults) {
    thread_state = find_nooks_stack(nook);
    if (thread_state == NULL) {
      return(-1);
    }

    size = thread_state->stack_base + PAGE_SIZE - 
      thread_state->trap_state.nook_stack;

    printk("stack range=%lx-%lx\n", 
	   (unsigned long) thread_state->trap_state.nook_stack, 
	   (unsigned long) thread_state->stack_base + PAGE_SIZE);

    addr = (unsigned long *) ((long) thread_state->trap_state.nook_stack + 
			      (random()&~0x3)%size);  
    taddr=(unsigned long) addr;
    flip_bit = random() & 0x1f;
    printk("%lx:%lx flip bit %d => ", taddr, *addr, flip_bit);
    flip_bit = 1 << flip_bit;
    res[count].address = taddr;
    res[count].old = *addr;
    res[count].new = (*addr) ^ flip_bit;
    if (injectFault) {
      *addr = ((*addr)^flip_bit); 
    }
    printk("%lx\n", *addr);
    count++;
  }
  return(0);
}


//
// Instead of dealing with heaps directly, we look at the area cache of pages 
// and vm pages and find an address there.
//


int heap_fault(pnook_t nook,
	       pswifi_result_t res)
{   
  unsigned long *addr, taddr;
  int flip_bit=0;
  int count=0;
  unsigned long flags;
  struct list_head *next;

  //
  // Bail if there are no memory areas
  //

  NOOKS_LOCK_RESOURCES(nook, flags);
  if (nooks_hash_empty(&nook->resources.area_map) && 
      list_empty(&nook->resources.page_map)) {
    NOOKS_UNLOCK_RESOURCES(nook, flags);
    return(-1);
  }


  //
  // Pick a random number, and count through that many maps before picking
  // an address in that map.
  //
#define MAX_NUM_AREAS 200

 do {
   int i = 1 + (random() % MAX_NUM_AREAS);
   nooks_hash_enum_t context = {0};
   pnooks_hash_entry_t entry = NULL;
   nooks_mm_map_t dummy_map;
   pnooks_mm_map_t map = NULL;

   do {
     while (nooks_enumerate_in_hash(&nook->resources.area_map,
				    &context,
				    &entry) == 0) {
       if (--i == 0) {
	 dummy_map.address = (unsigned long) entry->key;
	 dummy_map.size = (unsigned long) entry->data;
	 map = &dummy_map;
	 break;
       }
     }
     if (i == 0) {
       break;
     }
     list_for_each(next, &nook->resources.page_map) {
       i--;
       if (i == 0) {
	 map = list_entry(next,nooks_mm_map_t, next);
	 break;
       }
     }
   } while (i != 0);


   addr = (unsigned long *) (map->address + (random()&~0xf)%map->size); 
   
   taddr=(unsigned long) addr;
   flip_bit = random() & 0x1f;
   printk("heap range=%lx-%lx ", map->address, map->address + map->size);
   printk("%lx:%lx flip bit %d => ", taddr, *addr, flip_bit);
   flip_bit = 1 << flip_bit;
   res[count].address = taddr;
   res[count].old = *addr;
   res[count].new = (*addr) ^ flip_bit;

   if (injectFault) {
     *addr = ((*addr)^flip_bit); 
   }
   printk("%lx\n", *addr);
   count++;   
 } while (count < numFaults);

  NOOKS_UNLOCK_RESOURCES(nook, flags);
  return(0);
  
}


unsigned long	
do_fault_copy_from_user (void *kaddr, const void *udaddr, unsigned long len,
		      unsigned long (* copy_fn) (void *, const void *, unsigned long))
{   
  unsigned int prob, i=0;

  if ( faultInjected && (faultType==BCOPY_FAULT) ) {

    if (++crashCount == crashInterval) {   
      
      crashCount=0;
      prob = random();
      crashInterval = CRASH_INTERVAL + (random() & FI_MASK);
      
      if (prob < P50) {                    /* corrupt 1 QW         */
	i=1; 
      } else if (prob < P94) {               /* corrupt 2 - 1024 QW  */
	i = prob & 0x3fe;
	while(!i) {
	  i = random() & 0x3fe; 
	}
      } else {                            /* corrupt 2-4 pages    */
	i= prob & 0xc00;
	while(!i) {
	  i = random() & 0xc00; 
	}
      }
      printk("va=%p, pte=%lx, ova=%p, pte=%lx\n", 
	     kaddr, nooks_mm_get_pte(nooks_global_nook, kaddr), 
	     kaddr+len, nooks_mm_get_pte(nooks_global_nook, kaddr));
      printk("copyin: %p to %p, len=%ld overrun=%d, Intvl=%ld, inj=%ld\n", 
	     udaddr, kaddr, len, i, crashInterval, faultInjected);
      if (faultInjected++ <numFaults) {
	len += i;
      } else {
	faultInjected = 0;
      }
      i = 1;
    }
    return(copy_fn(kaddr, udaddr, len));
  } else {
    return(copy_fn(kaddr, udaddr, len));
  }
}

unsigned long
do_fault_copy_to_user(void *udaddr, const void *kaddr, unsigned long len,
		   unsigned long (* copy_fn) (void *, 
					      const void *, 
					      unsigned long))
{   
  unsigned int prob, i=0;

  if( faultInjected && (faultType==BCOPY_FAULT) ){
    crashCount++;
    if (crashCount == crashInterval) {
      crashCount=0;
      prob = random();
      crashInterval = CRASH_INTERVAL + (random() & FI_MASK);

      if ( prob < P50) {                    /* corrupt 1 QW         */
	i=1; 
      } else if(prob < P94) {               /* corrupt 2 - 1024 QW  */
	i = prob & 0x3fe;
	while (!i) {
	  i = random() & 0x3fe; 
	}
      } else {
	i = prob & 0xc00;
	while(!i) {
	  i = random() & 0xc00; 
	}
      }
      printk("copyout: %p to %p, len=%ld overrun=%d, Intvl=%ld, inj=%ld\n",
	     kaddr, udaddr, len, i, crashInterval, faultInjected);
      if (faultInjected++ <numFaults) {
	len+=i;
      } else  {
	faultInjected = 0;
      }
      i=1;
    }
    return(copy_fn(udaddr, kaddr, len));
  } else 
    return(copy_fn(udaddr, kaddr, len));
}


unsigned long
swifi___generic_copy_from_user (void *kaddr, void *udaddr, unsigned long len)
{
  return(do_fault_copy_from_user(kaddr, 
				 udaddr, 
				 len, 
				 __generic_copy_from_user));
}

unsigned long	
swifi___generic_copy_to_user(void *udaddr, void *kaddr, unsigned long len)
{
  return(do_fault_copy_to_user(udaddr, 
			       kaddr, 
			       len, 
			       __generic_copy_to_user));
}


unsigned long	
nooks_w_swifi___generic_copy_from_user(void *kaddr, 
				       const void *udaddr, 
				       unsigned long len)
{
  return(do_fault_copy_from_user(kaddr, 
				 udaddr, 
				 len, 
				 nooks_w___generic_copy_from_user));
}

unsigned long	
nooks_w_swifi___generic_copy_to_user(void *udaddr, 
				     const void *kaddr, 
				     unsigned long len)
{
  return(do_fault_copy_from_user(udaddr, 
				 kaddr, 
				 len, 
				 nooks_w___generic_copy_to_user));
}

void *
swifi_memcpy_fn (void *to, void *from, size_t len)
{   
  unsigned int prob, i=0;

  if( faultInjected && (faultType==BCOPY_FAULT) ) {
    crashCount++;
    if (crashCount == crashInterval) {
      crashCount=0;
      prob = random();
      crashInterval = CRASH_INTERVAL + (random() & FI_MASK);

      if (prob < P50) {                    /* corrupt 1 QW         */
               i=1; 
      } else if (prob < P94) {               /* corrupt 2 - 1024 QW  */
	i= prob & 0x3fe;
	while(!i) {
	  i = random() & 0x3fe; 
	}
      } else {                            /* corrupt 2-4 pages    */
	i=prob&0xc00;
	while(!i) {
	  i = random() & 0xc00; 
	}
      }
    
      printk("memcpy: %p to %p, len=%d overrun=%d, Intvl=%ld, inj=%ld\n", 
	     from, to, len, i, crashInterval, faultInjected);
      if(faultInjected++ <numFaults) len+=i;
      else faultInjected=0;
      i=1;
    }
    return(memcpy(to, from, len));
  } else 
    return(memcpy(to, from, len));
}


void *
swifi_memmove_fn (void *to, void *from, size_t len)
{   
  unsigned int prob, i=0;

  if( faultInjected && (faultType==BCOPY_FAULT) ) {
    crashCount++;
    if (crashCount == crashInterval) {
      crashCount=0;
      prob = random();
      crashInterval = CRASH_INTERVAL + (random() & FI_MASK);

      if (prob < P50) {                    /* corrupt 1 QW         */
               i=1; 
      } else if (prob < P94) {               /* corrupt 2 - 1024 QW  */
	i= prob & 0x3fe;
	while(!i) {
	  i = random() & 0x3fe; 
	}
      } else {                            /* corrupt 2-4 pages    */
	i=prob&0xc00;
	while(!i) {
	  i = random() & 0xc00; 
	}
      }
    
      printk("memmove: %p to %p, len=%d overrun=%d, Intvl=%ld, inj=%ld\n", 
	     from, to, len, i, crashInterval, faultInjected);
      if(faultInjected++ <numFaults) len+=i;
      else faultInjected=0;
      i=1;
    }
    return(memmove(to, from, len));
  } else 
    return(memmove(to, from, len));
}


void *
memmove_fn(void *to, void *from, size_t len)
{
  return(memmove(to, from, len));
}



void *
memcpy_fn(void *to, void *from, size_t len)
{
  return(memcpy(to, from, len));
}




void
do_fault_kfree(void *addr, void (* kfree_fn)(const void *))
{   
  if(addr == crashAddr) {
    crashAddr=0;
  }
  if (faultInjected && (faultType==FREE_FAULT || 
			faultType==MEM_LEAK_FAULT)) {
    crashCount++;
    if(crashCount>=crashInterval) {   
      
      /* alternate between premature freeing and non-free */
      if(crashToggle) {
	if(crashAddr) { 
	  printk("malloc : freeing %p prematurely\n", 
		 crashAddr);
	  kfree_fn(crashAddr);
	  kfree_fn(addr);
	  crashAddr=0;
	  crashToggle=0;
	  crashCount=0;
	  crashInterval = CRASH_INTERVAL + (random()&FI_MASK);
	  if (faultInjected++ > numFaults) {
	    faultInjected=0;
	  }
	} 
      } else {
	printk("free: don't free %p\n", addr); 
	if(faultInjected++ > numFaults) {
	  faultInjected=0;
	}
	if(faultType==FREE_FAULT) {
	  crashToggle=1;
	}
	crashCount=0;
	crashInterval = CRASH_INTERVAL + (random()&FI_MASK);
      }
    }
  } else {
    kfree_fn(addr);
  }
}

void
swifi_kfree(const void *addr)
{
  do_fault_kfree((void *) addr, kfree);
}

void
nooks_w_swifi_kfree(const void *addr)
{
  do_fault_kfree((void *) addr, nooks_w_kfree);
}

void do_vfree(const void * addr)
{
  vfree((void *) addr);
}

void do_nooks_w_vfree(const void * addr)
{
  nooks_w_vfree((void *) addr);
}

void
swifi_vfree(void *addr)
{
  do_fault_kfree(addr, do_vfree);
}

void
nooks_w_swifi_vfree(void *addr)
{
  do_fault_kfree(addr, do_nooks_w_vfree);
}



void *
do_fault_kmalloc(size_t size, 
		 int flags,
		 void * (* kmalloc_fn)(size_t size, int flags))
{
  if (faultInjected && (faultType==ALLOC_FAULT)) {
    crashCount++;
    if(crashCount>=crashInterval) {   
      printk("kmalloc : returning null\n");
      crashCount=0;
      crashInterval = CRASH_INTERVAL + (random()&FI_MASK);
      if (faultInjected++ > numFaults) {
	faultInjected=0;
	return(NULL);
      }

    }
  }

  return(kmalloc_fn(size, flags));
}


void *
swifi_kmalloc(size_t size, int flags)
{
  return(do_fault_kmalloc(size, flags, kmalloc));
}

void *
nooks_w_swifi_kmalloc(size_t size, int flags)
{
  return(do_fault_kmalloc(size, flags, nooks_w_kmalloc));
}


void * do_fault_vmalloc(unsigned long size, 
			int gfp_mask, 
			pgprot_t prot,
			void * (*vmalloc_fn)(unsigned long size, 
					     int gfp_mask, 
					     pgprot_t prot))
{
  if (faultInjected && (faultType==ALLOC_FAULT)) {
    crashCount++;
    if(crashCount>=crashInterval) {   
      printk("vmalloc : returning null\n");
      crashCount=0;
      crashInterval = CRASH_INTERVAL + (random()&FI_MASK);
      if (faultInjected++ > numFaults) {
	faultInjected=0;
	return(NULL);
      }

    }
  }
  return(vmalloc_fn(size, gfp_mask, prot));
}

void *
swifi___vmalloc(unsigned long size, int gfp_mask, pgprot_t prot)
{
  return(do_fault_vmalloc(size, gfp_mask, prot, __vmalloc));
}
 
void *
nooks_w_swifi___vmalloc(unsigned long size , int gfp_mask, pgprot_t prot)
{
  return(do_fault_vmalloc(size, gfp_mask, prot, nooks_w___vmalloc));
}


typedef struct section_callback {
  const char * module_name;
  const char * section_name;
  unsigned long sec_start;
  unsigned long sec_end;
} section_callback_t;

static int
text_section_callback(void *token, 
		      const char *modname, 
		      const char *secname,
		      ElfW(Addr) secstart, 
		      ElfW(Addr) secend, 
		      ElfW(Word) secflags)
{
  section_callback_t * info = (section_callback_t *) token;
  
  if ((strcmp(modname, info->module_name) == 0) &&
      (strcmp(secname, info->section_name) == 0)) {
    info->sec_start = secstart;
    info->sec_end = secend;
    return(1);
  }
  return(0);
}





int text_fault(pnook_t nook, struct module * mod, pswifi_result_t res)
{   
  unsigned long *addr, text_size, offset, page, taddr;
  unsigned long btext, etext;

  int count, flip_bit=0, len, rc;
  unsigned char *c;
  struct module * module;
  section_callback_t info;

  //
  // Find the text for this nook. It is found in the loaded
  // modules. First, randomly choose a module.
  //

#define MAX_NUM_MODULES 10

  /* inject faults into text space */

  for(count=0; count<numFaults; count++) {
    int i = 1 + (random() % MAX_NUM_MODULES);
    int j = i;
    if (mod == NULL) {

      struct list_head * next;
      pnook_driver_t driver = NULL;
      do {
	list_for_each(next, &nook->drivers) {
	  driver = list_entry(next,nook_driver_t, next_in_nook);
	  if (driver->driver_type == mops_e) {
	    if (--i == 0) {
	      break;
	    }
	  }
	}
      } while ((i != j) && (i > 0));
      if (i == j) {
	return(-1);
      }
    
      //
      // Determine where the code is for this module
      //
      
      module = (struct module *) driver->data;
    } else {
      module = mod;
    }
    info.module_name = module->name;
    info.section_name = ".text";

    kallsyms_sections(&info, text_section_callback);
    if (info.sec_start == 0 ) {
      return(-1);
    }

    btext = info.sec_start;
    etext = info.sec_end;
    text_size = etext - btext;
    
    printk("text=%lx-%lx, size=%lx\n", btext, etext, text_size);
    
    addr = (unsigned long *) 
      (btext + ((unsigned long) (random()&~0xf) % text_size)); 
    
    /* now the tricky part */

    taddr=(unsigned long) addr;
    if( faultType==INIT_FAULT || 
	faultType==NOP_FAULT || 
	faultType==DST_FAULT || 
	faultType==SRC_FAULT ||
	faultType==BRANCH_FAULT || 
	faultType==PTR_FAULT || 
	faultType==LOOP_FAULT || 
	faultType==INTERFACE_FAULT ||
	faultType==IRQ_FAULT ) {
      addr = (unsigned long *) find_faulty_instr(taddr, faultType, &len);
      /* do it over again if we can't find the right instruction */
      if(!addr || !len ) {
	i--;
	continue;
      }
    }
    printk("target addr=%lx, instr addr=%p, %lx=>", taddr, addr, *addr); 
      
    offset = (unsigned long) addr&PAGE_MASK;
    page = (unsigned long) addr&~PAGE_MASK;
    
    /* it doesn't matter what we used here to unprotect page,
     * as this routine will not be in production code.
     */
      
    res[count].address = taddr;
    res[count].old = *addr;
    res[count].new = *addr;

    if (faultType==TEXT_FAULT) {

      flip_bit = random() & 0x1f;
      printk("flip bit %d => ", flip_bit);
      flip_bit = 1 << flip_bit;

      res[count].new = (*addr) ^ flip_bit;
      
      if (injectFault) {
	*addr = ((*addr)^flip_bit); 
      }

    } else if (faultType==NOP_FAULT || 
	       faultType==INIT_FAULT ||
	       faultType==BRANCH_FAULT || 
	       faultType==INTERFACE_FAULT ||
	       faultType==IRQ_FAULT) {
      c = (unsigned char *) addr;

      for (j = 0; j < len; j++) {
	/* replace these bytes with NOP (*c=NOP) */
	if (j < sizeof(unsigned long)) {
	  ((unsigned char *) &res[count].new)[j] = NOP;
	}
	if (injectFault) {
	  *c=NOP;
	}	

	c++;
      }
    } else if (faultType==DST_FAULT || faultType==SRC_FAULT) {
      /* skip thru the prefix and opcode, and flip bits in following bytes */
      int prefix;
      c=(unsigned char *) addr;
      do {
	switch (*c) {
	case 0x66: case 0x67: case 0x26: case 0x36:
	case 0x2e: case 0x3e: case 0x64: case 0x65:
	case 0xf0: case 0xf2: case 0xf3:
	  prefix = 1;
	  break;
	default:
	  prefix = 0;
	  break;
	}
	if (prefix) {
	  c++;
	}
      } while (prefix);
      if(*c>=0xd8 && *c<=0xdf) {
	/* don't mess with fp instruction, yet.
	 * but there shouldn't be any fp instr in kernel.
	 */
	printk("floating point instruction, bailing out\n");
	i--;
	continue;
      } else if(*c==0x0f) {
	c++;
      }
      if(*c==0x0f) {
	c++;
      }
      c++;
      len = len-((long) c - (long) addr);
      flip_bit = random() % (len*8);
      printk("flip bit %d (len=%d) => ", flip_bit, len);
      for(j=0; j<len; j++) {
	/* go to the right byte */
	if(flip_bit<8) {
	  flip_bit = 1 << flip_bit;

	  if (j < sizeof(unsigned long)) {
	    ((unsigned char *) &res[count].new)[j] = (*c) ^ flip_bit;
	  }


	  if (injectFault) {
	    *c=(*c^flip_bit);
	  }

	  j=len;
	}
	c++;
	flip_bit = flip_bit-8;
      }
    } else if(faultType==PTR_FAULT) {
      /* 5f) ptr: if instruction has regmodrm byte (i_has_modrm),
       *     flip 1 bit in lower byte (0x0f) or any bit in following
       *     bytes (sib, imm or disp).
       */
      int prefix;
      c=(unsigned char *) addr;
      do {
	switch (*c) {
	case 0x66: case 0x67: case 0x26: case 0x36:
	case 0x2e: case 0x3e: case 0x64: case 0x65:
	case 0xf0: case 0xf2: case 0xf3:
	  prefix = 1;
	  break;
	default:
	  prefix = 0;
	  break;
	}
	if (prefix) {
	  c++;
	}
      } while (prefix);
      if(*c>=0xd8 && *c<=0xdf) {
	/* don't mess with fp instruction, yet */
	printk("floating point instruction, bailing out\n");
	i--;
	continue;
      } else if(*c==0x0f) {
	c++;
      }
      if(*c==0x0f) {
	c++;
      }
      c++;
      len = len-((long) c - (long) addr);
      flip_bit = random() % (len*8-4);
      printk("flip bit %d (len=%d) => ", flip_bit, len);

      /* mod/rm byte is special */

      if (flip_bit < 4) {
	flip_bit = 1 << flip_bit;

	rc = c - (unsigned char *) addr;
	if (rc < sizeof(unsigned long)) {
	  ((unsigned char *) &res[count].new)[rc] = (*c) ^ flip_bit;
	  
	}
	if (injectFault) {
	  *c=(*c^flip_bit);
	}

      }
      c++; 
      flip_bit=flip_bit-4;

      for(j=1; j<len; j++) {
	/* go to the right byte */
	if (flip_bit<8) {
	  flip_bit = 1 << flip_bit;

	  rc = (c - (unsigned char *) addr);
	  if (rc < sizeof(unsigned long)) {
	    ((unsigned char *) &res[count].new)[rc] = (*c) ^ flip_bit;
	    
	  }
	  if (injectFault) {
	    *c=(*c^flip_bit);
	  }

	  j=len;
	}
	c++;
	flip_bit = flip_bit-8;
      }
    } else if(faultType==LOOP_FAULT) {
      c=(unsigned char *) addr;
      /* replace rep with repe, and vice versa */
	if(*c==0xf3) {
	  if (j < sizeof(unsigned long)) {
	    ((unsigned char *) &res[count].new)[j] = NOP;
	  }

	  rc = (c - (unsigned char *) addr);
	  if (rc < sizeof(unsigned long)) {
	    ((unsigned char *) &res[count].new)[rc] = 0xf2;
	    
	  }
	  if (injectFault) {
	    *c=0xf2;
	  }
	} else if(*c==0xf2) {
	  rc = (c - (unsigned char *) addr);
	  if (rc < sizeof(unsigned long)) {
	    ((unsigned char *) &res[count].new)[rc] = 0xf3;
	    
	  }
	  if (injectFault) {
	    *c=0xf3;
	  }
	} else if( ((*c)&0xf0)==0x70 ) {
	  /* if we've jxx imm8 instruction, 
	   * incl even byte instruction, eg jo (70) to jno (71)
	   * decl odd byte instruction,  eg jnle (7f) to jle (7e)
	   */ 
	  if(*c%2 == 0) { 
	    rc = (c - (unsigned char *) addr);
	    if (rc < sizeof(unsigned long)) {
	      ((unsigned char *) &res[count].new)[rc] = (*c) + 1;
	    
	    }

	    if (injectFault) {
	      *c = *c+1;
	    }
	  }  else {

	    rc = (c - (unsigned char *) addr);
	    if (rc < sizeof(unsigned long)) {
	      ((unsigned char *) &res[count].new)[rc] = (*c) - 1;
	      
	    }

	    if (injectFault) {
	      *c = *c-1;
	    }
	  }
	} else if(*c==0x66 || *c==0x67)	{ 	/* override prefix */
	  c++;
	} else if(*(c++)==0xf && ((*c)&0xf0)==0x80 ) {
	  /* if we've jxx imm16/32 instruction, 
	   * incl even byte instruction, eg jo (80) to jno (81)
	   * decl odd byte instruction,  eg jnle (8f) to jle (8e)
	   */ 
	  if(*c%2 == 0) {
	    rc = (c - (unsigned char *) addr);
	    if (rc < sizeof(unsigned long)) {
	      ((unsigned char *) &res[count].new)[rc] = (*c) + 1;
	      
	    }
	    if (injectFault) {
	      *c = *c+1;
	    }
	  } else {
	    rc = (c - (unsigned char *) addr);
	    if (rc < sizeof(unsigned long)) {
	      ((unsigned char *) &res[count].new)[rc] = (*c) -1;
	      
	    }

	    if (injectFault) {
	      *c = *c-1;
	    }
	  }
	}
      
    }
    printk("%lx\n", *addr);
  }
  return(0);
}


