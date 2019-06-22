libscm = shared_library(
  'scm',
  ['scm.cpp'],
  include_directories: include_dir,
)

executable(
  'little-scheme',
  'main.cpp',
  include_directories: include_dir,
  link_with: libscm,
  install: true # little-scheme would be part of project installation
)

gtest = dependency('gtest', main : true, required : false)
if not gtest.found()
  error('MESON_SKIP_TEST: gtest not installed.')
endif
e = executable(
  'test-all',
  'test.cc',
  link_with: libscm,
  dependencies : gtest
)
test('gtest test', e)
