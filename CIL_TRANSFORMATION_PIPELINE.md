# Complete CIL Code Transformation Pipeline

## Overview
This document maps the complete transformation pipeline from original driver source code to final kernel and user-mode split binaries, including all CIL processing steps.

---

## 1. CIL ENTRY POINTS AND MAIN DRIVER

### 1.1 Main CIL Entry Point
- **File**: `/home/user/fgft/cil/src/main.ml` (298 lines)
- **Function**: `theMain()` at line 160
- **Purpose**: Program entry point for the 'cilly' tool
- **Process**:
  1. Parse command-line arguments (line 221)
  2. Initialize CIL (line 222)
  3. Parse source files to CIL AST (line 230: `parseOneFile`)
  4. Merge multiple files if needed (line 240: `Mergecil.merge`)
  5. Process merged CIL file with features (line 261: `processOneFile`)

### 1.2 Feature Processing Pipeline
- **Location**: `main.ml` lines 90-111
- **Features List** (executed sequentially):
  1. `Epicenter.feature` - Callstack inspection
  2. `Simplify.feature` - Code simplification
  3. `Canonicalize.feature` - Canonical form
  4. `Callgraph.feature` - Call graph computation
  5. `Logwrites.feature` - Write logging
  6. `Heapify.feature1/2` - Heap transformation
  7. `Oneret.feature` - Single return point
  8. `makeCFGFeature` - Control flow graph
  9. `Partial.feature` - Partial evaluation
  10. `Simplemem.feature` - Memory simplification
  11. `Sfi.feature` - SFI instrumentation
  12. `Sfi2.feature` - SFI v2 instrumentation
  13. `Dataslicing.feature` - Data slicing
  14. `Logcalls.feature` - Call logging
  15. `Ptranal.feature` - Pointer analysis
  16. `Liveness.feature` - Liveness analysis
  17. `Carb.feature` - Carb analysis
  18. `Drivers.feature` - **DRIVER TRANSFORMATION** (line 109)

### 1.3 CIL Script Invocation
- **Script**: `/home/user/fgft/cil/bin/cilly` (Perl wrapper)
- **Type**: Perl script that wraps OCaml compiler
- **Base**: `CilCompiler` class inherits from `Cilly` module
- **Execution**: Uses either `cilly.asm.exe` or `cilly.byte.exe`

---

## 2. BUILD INTEGRATION: HOW DRIVERS ARE INVOKED

### 2.1 Driver Build System
- **Makefile Location**: `/home/user/fgft/ipc_drivers/e1000/Makefile` (341 lines)
- **Build Targets**: 
  - `$(DRIVER_NAME).merged.c` - Merged preprocessed driver
  - `$(DRIVER_NAME).entry_points.txt` - Root function entry points
  - `$(DRIVER_NAME).all_annots.txt` - All annotations (user/kern)
  - `$(DRIVER_NAME).kernel.c` - Kernel stub version
  - `$(DRIVER_NAME).sym.c` - User-mode symbolic version
  - `$(DRIVER_NAME).sym.o` - Compiled user-mode object
  - `$(DRIVER_NAME)-stub.ko` - Kernel stub module
  - `$(DRIVER_NAME)-sfi.ko` - SFI instrumented module

### 2.2 CIL Invocation in Makefile

#### Step 1: Create Merged File (Line 89-116)
```makefile
$(DRIVER_NAME).merged.c: export HOSTCC=cilly --merge --save-temps --commPrintLn $(EXTRA_CILLY_FLAGS)
$(DRIVER_NAME).merged.c: export CC=cilly --merge --save-temps --commPrintLn $(EXTRA_CILLY_FLAGS)
# Invokes kernel build system with cilly as compiler to merge all sources
```
- **Command Options**:
  - `--merge`: Enable CIL merging mode
  - `--save-temps`: Keep intermediate `.cil.c` files
  - `--commPrintLn`: Use comment-style line directives
- **Output**: `e1000.merged.c` - Single preprocessed file

#### Step 2: Find Entry Points (Line 159-167)
```makefile
$(DRIVER_NAME).entry_points.txt: cilly --dodrivers $(DOFUNCPTR) $(DOVOIDPTR) \
    --dowhat="find-roots $@" $(DRIVER_NAME).merged.c > output_entry_points.txt
```
- **Command Options**:
  - `--dodrivers`: Enable driver analysis
  - `--dofuncptr`: Enable function pointer analysis
  - `--dowhat="find-roots"`: Find root annotations (entry points)
- **Output**: Function list marked as entry points

#### Step 3: Generate Annotations (Line 196-217)
```makefile
$(DRIVER_NAME).all_annots.txt: cilly --dodrivers $(DOFUNCPTR) $(DOVOIDPTR) \
    --dowhat="annot-prop $(DRIVER_NAME).annot.txt ../common/COLORS.txt CG.dot $(DRIVER_NAME).all_annots.txt" \
    $(DRIVER_NAME).merged.c
```
- **Purpose**: Propagate annotations (user/kern) through call graph
- **Adds**: Special functions like `logRead`, `logWrite`, `logAlloc`, `logFree`, etc.

#### Step 4: Generate Kernel Version (Line 219-235)
```makefile
$(DRIVER_NAME).kernel.c: cilly --save-temps --commPrintLnSparse \
    --docarb --sfireads --sfiwrites --sfikernel=1 --heapifyAll \
    --dodrivers $(DOFUNCPTR) $(DOVOIDPTR) --dobecons=$(BECONS) \
    --dowhat="normal-kern $(DRIVER_NAME).all_annots.txt" \
    $(DRIVER_NAME).merged.kernel.c -o $(DRIVER_NAME).kernel.temp.c
```
- **Options**:
  - `--docarb`: Enable CARB transformation
  - `--sfireads/--sfiwrites`: Enable SFI read/write instrumentation
  - `--sfikernel=1`: SFI in kernel mode
  - `--heapifyAll`: Heapify all local arrays
  - `--dowhat="normal-kern"`: Generate kernel version
  - `--dobecons`: Conservative marshaling

#### Step 5: Generate User-Mode Version (Line 246-262)
```makefile
$(DRIVER_NAME).sym.c: cilly --save-temps --commPrintLnSparse \
    --docarb --sfireads --sfiwrites --heapifyAll \
    --dodrivers $(DOFUNCPTR) $(DOVOIDPTR) --dobecons=$(BECONS) \
    --dowhat="normal-user $(DRIVER_NAME).all_annots.txt $(DRIVER_NAME).entry_points.txt" \
    $(DRIVER_NAME).merged.sym.c
```
- **Purpose**: Generate user-mode symbolic driver
- **Difference**: No `--sfikernel` flag (user-mode by default)

#### Step 6: Compile User-Mode Object (Line 304-310)
```makefile
$(DRIVER_NAME).sym.o: $(DRIVER_NAME).sym.c
    $(UMCC) $(EXTRA_FLAGS) -Wno-unknown-pragmas \
        -I$(DRIVER_KERNEL_ANNOTATED_HEADERS) \
        -c $(DRIVER_NAME).sym.c -o $@
```

---

## 3. DRIVER TRANSFORMATION PIPELINE DETAILS

### 3.1 Driver Feature Entry Point
- **File**: `/home/user/fgft/cil/src/ext/drivers/drivers.ml` (203 lines)
- **Main Function**: `dodriveranalysis()` at line 44
- **Feature Descriptor**: Defined at line 177
- **Command-line Option**: `--dodrivers` (enables the feature)

### 3.2 Driver Analysis Options (Parsed at line 119-172)
The `--dowhat` parameter controls what transformation to perform:

1. **"find-roots <annotfile>"**
   - Module: `root_annot_dri.ml` (351 lines)
   - Function: `Root_annot_dri.do_extract_rootannot()`
   - Purpose: Extract root annotations from driver structures
   - Detects: Interrupt handlers, probe/remove functions, etc.

2. **"annot-prop <root-annot> <colormap> <dotty> <annot-out>"**
   - Module: `annotator_dri.ml` (709 lines)
   - Class: `propagation_based_annotator` (line 84)
   - Purpose: Propagate annotations through call graph
   - Output: Functions marked as "user" or "kern"

3. **"normal-user <annot-in> <entry-points>"**
   - Calls: `split_driver()` with `"produce-user"` (line 78)
   - Purpose: Generate user-mode driver

4. **"normal-kern <annot-in>"**
   - Calls: `split_driver()` with `"produce-kern"` (line 80)
   - Purpose: Generate kernel stub version

### 3.3 Core Transformation Modules

#### 3.3.1 Call Graph Computation
- **File**: `/home/user/fgft/cil/src/ext/drivers/cgcomp_dri.ml` (729 lines)
- **Class**: `cgComputer` (line 98 in callgraph.ml)
- **Purpose**: Build call graph, function pointer analysis
- **Key Functions**:
  - `fpta_tostring_typ()` - Type-to-string for matching (line 22)
  - `fpta_tostring_exp_as_lval_type()` - Expression type analysis (line 54)
  - Function pointer type-based analysis

#### 3.3.2 Marshaling Analysis
- **File**: `/home/user/fgft/cil/src/ext/drivers/marshal_dri.ml` (2107 lines)
- **Main Function**: `do_marshal_analysis()` (called from drivers.ml line 34)
- **Purpose**: Determine which arguments must be marshalled between user/kernel
- **Key Components**:
  - Points-to graph construction (line 69-89)
  - Argument copying analysis
  - Data structure reference tracking
- **Options**:
  - `do_be_conservative` (line 28) - Conservative marshaling
  - `do_void_ptr` (line 31) - Void pointer analysis
  - `do_simple_m_dm` (line 29) - Specialized marshaling code

#### 3.3.3 Splitting & Code Generation
- **File**: `/home/user/fgft/cil/src/ext/drivers/splitter_dri.ml` (1967 lines)
- **Main Function**: `do_splitting()` (called from drivers.ml line 35)
- **Purpose**: Split driver into user and kernel components
- **Steps**:
  1. Constant replacement (class `pass_modif_const` line 59)
  2. Function ID mapping (class `pass_function_id_map` line 31)
  3. Marshaling code generation
  4. Stub generation

#### 3.3.4 Marshaling Code Generation
- **File**: `/home/user/fgft/cil/src/ext/drivers/splitter_marshcode_dri.ml` (2201 lines)
- **Key Functions**:
  - `gen_registerfn()` (line 105) - Register functions with object tracker
  - `gen_registerglob()` (line 148) - Register global variables
  - `gen_basic_call()` (line 186) - Generate synchronization calls
  - `gen_extra_code()` (line 198) - Generate additional instrumentation
- **Purpose**: Generate marshaling/demarshaling code for function arguments

#### 3.3.5 Utilities and Globals
- **File**: `/home/user/fgft/cil/src/ext/drivers/utils_dri.ml` (488 lines)
- **File**: `/home/user/fgft/cil/src/ext/drivers/splitter_globals_dri.ml` (100 lines)
- **File**: `/home/user/fgft/cil/src/ext/drivers/splitter_utils_dri.ml` (3145 lines)
- **Key Globals** (defined in splitter_globals_dri.ml):
  - `struct_marshret_compinfo` - Return structure from marshaling
  - `struct_reqargs_compinfo` - Request argument structure
  - `marshwrap_prefix` (line 95) - Prefix for marshaling wrappers
  - `stub_prefix` (line 97) - Prefix for stub functions

---

## 4. SFI INSTRUMENTATION

### 4.1 SFI Feature
- **File**: `/home/user/fgft/cil/src/ext/sfi.ml` (601+ lines)
- **Feature**: Defined at line 601
- **Options**:
  - `--doSfi` - Enable SFI instrumentation
  - `--sfireads` - Instrument read operations
  - `--sfiwrites` - Instrument write operations (line 54)
  - `--sfikernel` - Kernel vs user-mode SFI (line 55)
- **Purpose**: Insert runtime checks for memory access
- **Key Components**:
  - `mustSfiFunction()` (line 86) - Determine if function needs SFI
  - `skipFunctions` hashtable (line 85)
  - Memory read/write logging instrumentation

### 4.2 SFI v2 (Enhanced)
- **File**: `/home/user/fgft/cil/src/ext/sfi2.ml`
- **Purpose**: Additional SFI enhancements and transformations

---

## 5. OTHER KEY TRANSFORMATIONS

### 5.1 CARB (Code Adaptation and Recovery Blocks)
- **File**: `/home/user/fgft/cil/src/ext/drivers/carb.ml` (3182 lines)
- **Purpose**: Prepare code for fault recovery mechanisms
- **Options**: `--docarb` in Makefile
- **Features**: Tickval expressions, bad function lists (line 17-97)

### 5.2 Annotation and Marshaling Annotations
- **File**: `/home/user/fgft/cil/src/ext/drivers/marshannot_dri.ml` (891 lines)
- **Function**: `do_perform_cleanup()` (called from drivers.ml line 36)
- **Purpose**: Finalize marshaling annotations

### 5.3 Modifications Analysis
- **File**: `/home/user/fgft/cil/src/ext/drivers/modifs_dri.ml` (263 lines)
- **Option**: `--dowhat="modifs"`
- **Purpose**: Generate modification annotations

### 5.4 Symbolic Wrappers
- **File**: `/home/user/fgft/cil/src/ext/drivers/symwrappers_dri.ml` (74 lines)
- **Option**: `--dowhat="generate-wrappers"`
- **Purpose**: Generate wrappers for kernel functions

---

## 6. TRANSFORMATION PIPELINE FLOW DIAGRAM

```
INPUT: Original Driver Source Code
        |
        v
[STEP 1: CIL Parse & Merge]
- Parse: Frontc.parse() -> CIL AST
- Merge: Mergecil.merge() (if multiple files)
        |
        v
[STEP 2: CIL Features (Sequential)]
- Epicenter, Simplify, Canonicalize, Callgraph, Logwrites, etc.
        |
        v
[STEP 3: DRIVER FEATURE ACTIVATION]
- Location: Drivers.feature (drivers.ml line 109)
- dodriveranalysis() is called
        |
        v
[STEP 4a: FIND-ROOTS ANALYSIS]
- Module: root_annot_dri.ml
- Identify: Interrupt handlers, probe/remove, entry points
- Output: entry_points.txt
        |
        v
[STEP 4b: ANNOTATION PROPAGATION]
- Module: annotator_dri.ml
- Propagate: user/kern labels through call graph
- Output: all_annots.txt (function -> user/kern mapping)
        |
        v
[STEP 4c: MARSHALING ANALYSIS]
- Module: marshal_dri.ml
- Analyze: Which arguments need marshalling
- Points-to: Graph construction, data flow analysis
        |
        v
[STEP 4d: SPLITTING & CODE GENERATION]
- Module: splitter_dri.ml
- Split function at user/kernel boundary
- Generate: Marshaling/demarshaling code
        |
        v
[STEP 5a: GENERATE KERNEL VERSION]
Option: --dowhat="normal-kern"
- Module: splitter_dri.ml
- Output: $(DRIVER_NAME).kernel.c
- Stubs: Calls to user-space via marshaling
- Includes: master_top.h (kernel headers)
        |
        v
[STEP 5b: GENERATE USER-MODE VERSION]
Option: --dowhat="normal-user"
- Module: splitter_dri.ml
- Output: $(DRIVER_NAME).sym.c
- Full driver: Complete implementation
- Includes: slave_top.h (user-space headers)
        |
        v
[STEP 6: SFI INSTRUMENTATION (optional)]
- Options: --sfireads, --sfiwrites
- Inserts: Runtime memory access checks
- Kernel: --sfikernel=1 for kernel version
        |
        v
[STEP 7: CARB TRANSFORMATION (optional)]
- Option: --docarb
- Adds: Fault recovery mechanisms
- Modifies: Error handling code
        |
        v
[STEP 8: HEAPIFICATION (optional)]
- Option: --heapifyAll
- Converts: Stack arrays to heap allocation
        |
        v
OUTPUTS:
- $(DRIVER_NAME).merged.c        <- Merged preprocessed driver
- $(DRIVER_NAME).kernel.c        <- Kernel stub version (master)
- $(DRIVER_NAME).sym.c           <- User-mode version (slave)
- $(DRIVER_NAME).entry_points.txt <- Entry functions
- $(DRIVER_NAME).all_annots.txt   <- Annotations
- $(DRIVER_NAME).sym.o            <- Compiled user-mode object
- $(DRIVER_NAME)-stub.ko          <- Kernel module with stubs
- $(DRIVER_NAME)-sfi.ko           <- SFI instrumented module
```

---

## 7. OUTPUT FILES GENERATED

### 7.1 Intermediate Files

| File | Purpose | Line Count | Module |
|------|---------|-----------|--------|
| `$(DRIVER_NAME).merged.c` | Preprocessed merged source | Input-dependent | `Mergecil` |
| `$(DRIVER_NAME).merged.cil.c` | CIL intermediate representation | ~1000s | CIL core |
| `$(DRIVER_NAME).entry_points.txt` | Root function list | ~100s | `root_annot_dri.ml` |
| `$(DRIVER_NAME).all_annots.txt` | Function annotations | ~100s | `annotator_dri.ml` |

### 7.2 Final Output Files

| File | Mode | Header | Purpose |
|------|------|--------|---------|
| `$(DRIVER_NAME).kernel.c` | Kernel | `master_top.h` | Kernel-side stubs and marshaling code |
| `$(DRIVER_NAME).sym.c` | User | `slave_top.h` | User-space driver implementation |
| `$(DRIVER_NAME).sym.o` | Object | N/A | Compiled user-mode object file |
| `$(DRIVER_NAME)-stub.ko` | Module | N/A | Kernel module with stubs |
| `$(DRIVER_NAME)-sfi.ko` | Module | N/A | SFI-instrumented kernel module |

### 7.3 Output Structure

**Kernel Version (master_top.h)**:
- Function stubs for driver functions
- Marshaling code to serialize arguments
- IPC mechanism calls
- Example: `e1000.kernel.c` includes `master_top.h`

**User-Mode Version (slave_top.h)**:
- Complete driver implementation
- Marshaling/demarshaling for IPC
- Function ID mapping
- Example: `e1000.sym.c` includes `slave_top.h`

---

## 8. KEY DATA STRUCTURES

### 8.1 Marshaling Structures (from splitter_globals_dri.ml)

```ocaml
struct marshret_struct {
    void *buf;      // Buffer with marshaled data
    int len;        // Length of buffer
};

struct req_args {
    int function_id;    // ID of function to call
    void *data;         // Marshaled arguments
    int length;         // Length of marshaled data
};
```

### 8.2 Points-to Graph (from marshal_dri.ml line 69-121)

```ocaml
type ptgnode_t = Ptgnode of typ
type ptgedge_t = Ptgedge of ptgnode_t * string * ptgnode_t * string
type ptgraph_t = {nodes: Ptgnodeset.t; edges: Ptgedgeset.t}
```

---

## 9. COMMAND-LINE OPTIONS MAPPING

| Option | Module | Purpose | Default |
|--------|--------|---------|---------|
| `--merge` | Frontc | Enable merge mode | Disabled |
| `--dodrivers` | drivers.ml | Enable driver analysis | Disabled |
| `--dowhat="..."` | drivers.ml | Specify analysis type | Varies |
| `--dofuncptr` | cgcomp_dri.ml | Function pointer analysis | `do_function_pointer_analysis` |
| `--dobecons` | marshal_dri.ml | Conservative marshaling | `do_be_conservative = true` |
| `--dovoidptr` | marshal_dri.ml | Void pointer analysis | `do_void_ptr = false` |
| `--docarb` | carb.ml | CARB transformation | Disabled |
| `--sfireads` | sfi.ml | SFI read instrumentation | `doSfiReads = false` |
| `--sfiwrites` | sfi.ml | SFI write instrumentation | `doSfiWrites = true` |
| `--sfikernel` | sfi.ml | Kernel-mode SFI | `doSfiKernel = 0` |
| `--heapifyAll` | heapify.ml | Heapify all arrays | Disabled |
| `--save-temps` | cilly | Keep intermediate files | Disabled |
| `--commPrintLn` | main.ml | Comment-style line directives | Enabled |

---

## 10. TRANSFORMATION STEP DETAILS

### 10.1 Step 1: Parse Driver Source
- **Input**: `e1000_main.c`, `e1000_hw.c`, `e1000_ethtool.c`, `e1000_param.c`
- **Function**: `Frontc.parse()` (frontc/frontc.ml)
- **Output**: CIL AST in memory
- **Size**: ~12,000+ lines of C code

### 10.2 Step 2: Analyze Function Calls and Data Flow
- **Input**: CIL AST
- **Function**: `Cgcomp_dri` module (callgraph computation)
- **Analyzes**:
  - Direct function calls
  - Function pointer targets (type-based)
  - Call chains
- **Output**: Call graph hashtable
- **File**: `/home/user/fgft/cil/src/ext/drivers/cgcomp_dri.ml`

### 10.3 Step 3: Generate Marshaling Code
- **Input**: Call graph, function signatures
- **Function**: `Marshal_dri.do_marshal_analysis()`
- **Analyzes**:
  - Pointer arguments
  - Nested structures
  - Array access patterns
- **Generates**: Marshaling specifications
- **File**: `/home/user/fgft/cil/src/ext/drivers/marshal_dri.ml` (2107 lines)

### 10.4 Step 4: Generate SFI Instrumented Version
- **Input**: CIL AST with marshaling info
- **Function**: `Sfi.feature` (sfi.ml line 601)
- **Adds**: Runtime checks for:
  - Memory reads
  - Memory writes
  - Pointer dereferences
- **Output**: Instrumented CIL AST
- **File**: `/home/user/fgft/cil/src/ext/sfi.ml`

### 10.5 Step 5: Generate Kernel Stub Version
- **Input**: CIL AST, annotations
- **Option**: `--dowhat="normal-kern"`
- **Function**: `Splitter_dri.do_splitting()` with `"produce-kern"`
- **Actions**:
  1. Identify kernel-side functions
  2. Replace driver functions with stubs
  3. Generate marshaling code
  4. Add IPC calls
- **Output**: `$(DRIVER_NAME).kernel.c`
- **File**: `/home/user/fgft/cil/src/ext/drivers/splitter_dri.ml`

### 10.6 Step 6: Generate User-Mode Symbolic Version
- **Input**: CIL AST, annotations
- **Option**: `--dowhat="normal-user"`
- **Function**: `Splitter_dri.do_splitting()` with `"produce-user"`
- **Actions**:
  1. Identify user-side functions
  2. Keep full driver implementation
  3. Replace kernel calls with marshaling wrappers
  4. Add object tracking code
- **Output**: `$(DRIVER_NAME).sym.c`
- **File**: `/home/user/fgft/cil/src/ext/drivers/splitter_dri.ml`

### 10.7 Step 7: Generate Merged Dispatch Version
- **Input**: Both kernel and user versions
- **Purpose**: Single binary with dispatcher
- **File**: `$(DRIVER_NAME).merged.c` (optional)
- **Not implemented** in current e1000 build

---

## 11. FILE PATHS REFERENCE

### Core CIL Entry Points
- Main entry: `/home/user/fgft/cil/src/main.ml`
- Features: `/home/user/fgft/cil/src/main.ml` lines 90-111
- Feature descriptor: `C.featureDescr` type

### Driver Extension Files (17,589 total lines)
- Base drivers module: `/home/user/fgft/cil/src/ext/drivers/drivers.ml` (203 lines)
- Call graph: `/home/user/fgft/cil/src/ext/drivers/cgcomp_dri.ml` (729 lines)
- Marshaling: `/home/user/fgft/cil/src/ext/drivers/marshal_dri.ml` (2,107 lines)
- Splitter: `/home/user/fgft/cil/src/ext/drivers/splitter_dri.ml` (1,967 lines)
- Marshaling code gen: `/home/user/fgft/cil/src/ext/drivers/splitter_marshcode_dri.ml` (2,201 lines)
- Utilities: `/home/user/fgft/cil/src/ext/drivers/splitter_utils_dri.ml` (3,145 lines)
- Root annotation: `/home/user/fgft/cil/src/ext/drivers/root_annot_dri.ml` (351 lines)
- Annotation: `/home/user/fgft/cil/src/ext/drivers/annotator_dri.ml` (709 lines)
- CARB: `/home/user/fgft/cil/src/ext/drivers/carb.ml` (3,182 lines)

### SFI Extension
- Main SFI: `/home/user/fgft/cil/src/ext/sfi.ml` (feature at line 601)
- SFI v2: `/home/user/fgft/cil/src/ext/sfi2.ml`

### Build System
- Driver Makefile: `/home/user/fgft/ipc_drivers/e1000/Makefile`
- Cilly script: `/home/user/fgft/cil/bin/cilly`

### Output Examples
- Kernel version: `/home/user/fgft/ipc_drivers/e1000/e1000.kernel.c`
- User version: `/home/user/fgft/ipc_drivers/e1000/e1000.sym.c`
- Merged: `/home/user/fgft/ipc_drivers/e1000/e1000.merged.c`

---

## 12. COMPLETE EXECUTION FLOW

```
User invokes: make -C ipc_drivers/e1000

  ↓

[Phase 1: Create Merged File]
  Makefile: $(DRIVER_NAME).merged.c target (line 89-116)
  Invokes: cilly --merge --save-temps ...
  Parser: Frontc.parse() in main.ml line 60
  Merger: Mergecil.merge() in main.ml line 240
  Output: e1000.merged.c (~15,000 lines of preprocessed C)

  ↓

[Phase 2: Find Root Annotations]
  Makefile: $(DRIVER_NAME).entry_points.txt target (line 159-167)
  Invokes: cilly --dodrivers --dowhat="find-roots $@"
  Feature: Drivers.feature enabled (main.ml line 109)
  Function: dodriveranalysis() → Root_annot_dri.do_extract_rootannot()
  Analysis: Identifies PCI probe/remove, interrupt handlers
  Output: e1000.entry_points.txt

  ↓

[Phase 3: Propagate Annotations]
  Makefile: $(DRIVER_NAME).all_annots.txt target (line 196-217)
  Invokes: cilly --dodrivers --dowhat="annot-prop ..."
  Feature: Drivers.feature enabled
  Function: dodriveranalysis() → Annotator_dri.do_annotation()
  Class: propagation_based_annotator (annotator_dri.ml line 84)
  Analysis:
    - Build call graph (cgcomp_dri.ml)
    - Propagate user/kern labels from roots
    - Compute line-count statistics
  Output: e1000.all_annots.txt (each function gets user/kern label)

  ↓

[Phase 4a: Generate Kernel Stub Version]
  Makefile: $(DRIVER_NAME).kernel.c target (line 220-235)
  Invokes: cilly --dodrivers --docarb --sfireads --sfiwrites --sfikernel=1 
           --dowhat="normal-kern e1000.all_annots.txt"
  Feature: Drivers.feature enabled
  Function: dodriveranalysis() → split_driver(f, "produce-kern", "normal")
  Sequence:
    1. Marshal_dri.do_marshal_analysis(f, "normal") - line 34
       - Analyzes which arguments need marshalling
       - Builds points-to graph
    2. Splitter_dri.do_splitting(f, "produce-kern", "normal") - line 35
       - Generates kernel stubs
       - Creates marshaling/demarshaling code
       - Adds IPC mechanism calls
    3. Marshannot_dri.do_perform_cleanup(f) - line 36
    4. Apply: CARB, SFI, Heapify features
  Output: e1000.kernel.c (includes master_top.h, ~5,000 lines)
  Structure:
    - Function stubs that call user-space
    - Marshaling code to package arguments
    - Global variable marshaling
    - Object tracker registration

  ↓

[Phase 4b: Generate User-Mode Symbolic Version]
  Makefile: $(DRIVER_NAME).sym.c target (line 246-262)
  Invokes: cilly --dodrivers --docarb --sfireads --sfiwrites 
           --dowhat="normal-user e1000.all_annots.txt e1000.entry_points.txt"
  Feature: Drivers.feature enabled
  Function: dodriveranalysis() → split_driver(f, "produce-user", "normal")
  Sequence:
    1. Marshal_dri.do_marshal_analysis(f, "normal") - line 34
    2. Splitter_dri.do_splitting(f, "produce-user", "normal") - line 35
       - Keeps full driver implementation
       - Wraps kernel calls
       - Generates marshaling wrappers
    3. Marshannot_dri.do_perform_cleanup(f) - line 36
    4. Apply: CARB, SFI features
  Output: e1000.sym.c (includes slave_top.h, ~8,000 lines)
  Structure:
    - Complete driver implementation
    - Marshaling wrappers for kernel calls
    - Function dispatch mechanism
    - Object tracking for pointers

  ↓

[Phase 5: Compile User-Mode Object]
  Makefile: $(DRIVER_NAME).sym.o target (line 304-310)
  Compiler: $(UMCC) (usually gcc)
  Input: e1000.sym.c
  Output: e1000.sym.o (compiled user-mode driver)

  ↓

[Phase 6: Build Kernel Module]
  Makefile: $(DRIVER_NAME)-stub.ko target (line 137-145)
  Compiler: Standard Linux kernel build system
  Input: e1000.kernel.c (compiled to e1000.kernel.o)
  Links with: ../../common/wrappers_*.o, ../../common/miscdevice.o
  Output: e1000-stub.ko (kernel module)

  ↓

Final Targets: $(DRIVER_NAME).sym.o, $(DRIVER_NAME)-stub.ko, $(DRIVER_NAME)-sfi.ko
```

---

## 13. SUMMARY TABLE

| Stage | Module | Input | Output | Key Operations |
|-------|--------|-------|--------|-----------------|
| 1 | Frontc | Driver C files | CIL AST | Parse, remove unused temps |
| 2 | Callgraph | CIL AST | Call graph | Function call analysis |
| 3 | RootAnnot | CIL AST | Entry points | Identify root functions |
| 4 | Annotator | Call graph + roots | Annotations | Propagate user/kern labels |
| 5 | Marshal | Annotations + CIL | Marshaling specs | Determine argument copying |
| 6a | Splitter (kern) | CIL + annotations | Kernel C code | Generate stubs + marshaling |
| 6b | Splitter (user) | CIL + annotations | User C code | Keep full driver + wrappers |
| 7 | SFI | Split C code | Instrumented C | Add memory access checks |
| 8 | CARB | Instrumented C | Fault-aware C | Add recovery mechanisms |
| 9 | Compiler | Split C files | .o/.ko files | Standard C compilation |

---
