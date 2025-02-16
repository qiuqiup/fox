cmake_minimum_required(VERSION 3.16)
project(LocalValueNumbering)

# 设置 C++ 标准
set(CMAKE_CXX_STANDARD 17)

# 查找 LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# 将 LLVM 的 CMake 设置导入到项目
list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(AddLLVM)
include(HandleLLVMOptions)

# 添加 include 路径
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# 构建动态库
add_library(LocalValueNumbering SHARED LocalValueNumbering.cpp)

# 链接到 LLVM Pass 插件所需的库
llvm_map_components_to_libnames(llvm_libs support core irreader passes)
target_link_libraries(LocalValueNumbering PRIVATE ${llvm_libs})

# 设置正确的符号导出
set_target_properties(LocalValueNumbering PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY ../build
    PREFIX "lib"
    SUFFIX ".dylib"
)

target_compile_options(LocalValueNumbering PRIVATE
    -fPIC
    -DDEBUG
)

# 显示 CMake 调试信息
message(STATUS "LLVM_INCLUDE_DIRS: ${LLVM_INCLUDE_DIRS}")
message(STATUS "LLVM_LIBRARIES: ${LLVM_LIBRARIES}")
