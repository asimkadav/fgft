#ifndef WRAPPERS_DISP_KERN_H
#define WRAPPERS_DISP_KERN_H

#ifdef WRAPPERS_SYM_DRIVER_STEP1
#ifdef WRAPPERS_SYM_DRIVER_STEP2
#error Define exactly one of WRAPPERS_SYM_DRIVER_STEP1 or WRAPPERS_SYM_DRIVER_STEP2
#endif
#endif

struct req_args;

//
// Execution modes
//
#define SYMEXEC_NA -2
#define SYMEXEC_ERROR -1
#define SYMEXEC_CONCRETE 0
#define SYMEXEC_REPLAY 1
#define SYMEXEC_IFNEEDED 2
#define SYMEXEC_FULL 3

//
// For host-ud only
//
void initialize_replay (void);
void initialize_exe_modes (const char *exe_mode_filename);

//
// Disp kern
//
int Sym_entry_point (const char *FUNCTION,
                     unsigned int LINE,
                     const char *driver_function,
                     const char *annotation);
int Sym_check_preconditions (const char *FUNCTION,
                             unsigned int LINE,
                             const char *fn_to_check);
int Sym_check_postconditions (const char *FUNCTION,
                              unsigned int LINE,
                              const char *fn_to_check);
int Sym_execution_mode (const char *FUNCTION,
                        unsigned int LINE,
                        const char *name);
int Sym_should_disp_kern (const char *FUNCTION,
                          unsigned int LINE);
void Sym_set_disp_kern (int yesno);
void Sym_register_function_buffer (const char *FUNCTION,
                                   unsigned int LINE,
                                   unsigned long function_id,
                                   struct req_args *rqargs);
void Sym_get_function_buffer (const char *FUNCTION,
                              unsigned int LINE,
                              unsigned long function_id,
                              struct req_args *rqargs);
void Sym_clear_buffers (void);

#ifdef WRAPPERS_SYM_DRIVER_STEP1
#endif

#ifdef WRAPPERS_SYM_DRIVER_STEP2
//
// Disp Kern handling
//
#define entry_point(driver_function, annotation)                        \
    Sym_entry_point(__FUNCTION__, __LINE__, driver_function, annotation)
#define check_preconditions(function_name)                              \
    Sym_check_preconditions(__FUNCTION__, __LINE__, function_name)
#define check_postconditions(function_name)                             \
    Sym_check_postconditions(__FUNCTION__, __LINE__, function_name)
#define execution_mode(name)                                    \
    Sym_execution_mode(__FUNCTION__, __LINE__, name)
#define should_disp_kern()                              \
    Sym_should_disp_kern(__FUNCTION__, __LINE__)
#define register_function_buffer(function_id, rqargs)                   \
    Sym_register_function_buffer(__FUNCTION__, __LINE__, function_id, rqargs)
#define get_function_buffer(function_id, rqargs)                        \
    Sym_get_function_buffer(__FUNCTION__, __LINE__, function_id, rqargs)
#endif

#endif
