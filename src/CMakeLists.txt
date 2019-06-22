project(src)

include_directories(src)
add_library(scm
  scm.cpp
)

add_executable(little-scheme
  main.cpp
)
target_link_libraries(little-scheme
  scm
)
install(
  TARGETS little-scheme
  RUNTIME DESTINATION bin
)

find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})
add_executable(tests
  test.cc
)
target_link_libraries(tests
  scm
  gtest
  gtest_main
)
