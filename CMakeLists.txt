cmake_minimum_required(VERSION 3.16)
project(LocalValueNumbering)

set(CMAKE_CXX_STANDARD 17)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(AddLLVM)
include(HandleLLVMOptions)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_library(LocalValueNumbering SHARED LocalValueNumbering.cpp)

llvm_map_components_to_libnames(llvm_libs support core irreader passes)
target_link_libraries(LocalValueNumbering PRIVATE ${llvm_libs})

set_target_properties(LocalValueNumbering PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY ../build
    PREFIX "lib"
    SUFFIX ".dylib"
)

target_compile_options(LocalValueNumbering PRIVATE
    -fPIC
    -DDEBUG
)

message(STATUS "LLVM_INCLUDE_DIRS: ${LLVM_INCLUDE_DIRS}")
message(STATUS "LLVM_LIBRARIES: ${LLVM_LIBRARIES}")
