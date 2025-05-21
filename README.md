# About

AzureMARS is a versatile Core War simulator written in Free Pascal. Tested on x86-64 under Windows and Linux, may work on even more platforms.

# Compiling and Usage

AzureMARS can be used in four different modes:
- as a standalone binary
- as a library
- as a FreePascal unit
- as a Julia module via provided bindings.

## Standalone Mode

Compile with `fpc -O4 -oAzureMARS Main.pas`, use with `AzureMARS [options] file1 file2`. Options mostly mirror PMARS. Only two-warrior matches are supported. Warrior compilation is not implemented, input files must be in load file format. Available options:

|Arg   | Type     | Description                           |
|------|----------|---------------------------------------|
|`-r`  | Integer  | Number of rounds                      |
|`-s`  | Integer  | Core size                             |
|`-c`  | Integer  | Cycles until tie                      |
|`-p`  | Integer  | Max. processes                        |
|`-l`  | Integer  | Max. warrior length                   |
|`-d`  | Integer  | Min. warriors distance                |
|`-h`  | String   | Hill preset, one of nano, tiny, 94nop |
|`-P`  |          | Permute starting positions            |
|`-Tp` |          | Test performance                      |
|`-Tc` |          | Test correctness                      |

## Library Mode

Compile with `fpc -O4 -dlib AzureMARS.pas`. See the list of exported functions at the end of `AzureMARS.pas`. Calling convention is cdecl. 

## FreePascal Unit

Place `AzureMARS.pas` in your project folder, import with `uses AzureMARS`. See the interface section for documentation.

## Julia Module

Place the compiled library and `AzureMARS.jl` in your project folder, then use as
```
include("AzureMARS.jl")
using .AzureMARS
```
Use the REPL help mode to see the documentation of exported types and functions: `help?> AzureMARS`.

# Performance

All tests performed with stock Ryzen 5950X. Built-in performance test: Nano 102 MIPS, 94nop 108 MIPS.

Comparison against other simulators, 4000 rounds, random warrior positions, time in seconds:
| Warrior 1 | Warrior 2 |  PMARS | Exhaust | Exhaust-MA | exmars | qmars | AzureMARS |
| ----------|-----------|--------|---------|------------|--------|-------|-----------|
| fixed     | fixed     |  4.844 | 3.244   | 2.497      | 2.690  | 3.084 | 2.744     |
| jaguar    | fixed     |  5.486 | 3.364   | 2.860      | 2.892  | 3.269 | 3.373     |
| jaguar    | jaguar    |  7.481 | 3.588   | 3.587      | 3.527  | 3.661 | 4.224     |
| stalker   | fixed     |  4.282 | 2.188   | 1.824      | 1.800  | 1.918 | 2.269     |
| stalker   | jaguar    |  4.132 | 1.961   | 1.572      | 1.610  | 1.629 | 2.157     |
| stalker   | stalker   |  3.247 | 1.480   | 1.121      | 1.197  | 1.245 | 1.696     |
| npaper2   | fixed     |  7.240 | 4.932   | 3.940      | 4.078  | 4.791 | 4.605     |
| npaper2   | jaguar    |  8.879 | 5.104   | 4.577      | 4.705  | 5.216 | 5.843     |
| npaper2   | stalker   |  5.156 | 2.528   | 2.215      | 2.236  | 2.309 | 2.917     |
| npaper2   | npaper2   | 10.318 | 6.249   | 5.754      | 5.261  | 6.587 | 6.659     |
