cmake_minimum_required(VERSION 3.1)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED on)

project(potswt_function)

if(MSVC)
  add_compile_options("/EHsc")
endif()

if(${CMAKE_CXX_COMPILER_ID} MATCHES "Clang" OR ${CMAKE_CXX_COMPILER_ID} MATCHES "GNU")
  add_compile_options("-Werror;-Wall")
endif()

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

include_directories(${CMAKE_SOURCE_DIR})

add_subdirectory(test)

enable_testing()

function(add_potswat_test test_name)
  add_test(
  NAME ${test_name}
  COMMAND ${test_name}
  WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
endfunction()

add_potswat_test(test_align)
add_potswat_test(test_badcopy)
add_potswat_test(test_callmove)
add_potswat_test(test_constructible)
add_potswat_test(test_container)
add_potswat_test(test_convnull)
add_potswat_test(test_ctorthrow)
add_potswat_test(test_heap)
add_potswat_test(test_nocopy)
add_potswat_test(test_overlambda)
add_potswat_test(test_ptm)
add_potswat_test(test_recover)
add_potswat_test(test_scoped_allocator)
add_potswat_test(test_swap)
add_potswat_test(test_target_access)
add_potswat_test(test_usernew)
add_potswat_test(test_volatile)

