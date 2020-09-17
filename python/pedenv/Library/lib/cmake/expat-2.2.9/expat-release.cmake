#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "expat::libexpat" for configuration "Release"
set_property(TARGET expat::libexpat APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(expat::libexpat PROPERTIES
  IMPORTED_IMPLIB_RELEASE "${_IMPORT_PREFIX}/lib/libexpat.lib"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/libexpat.dll"
  )

list(APPEND _IMPORT_CHECK_TARGETS expat::libexpat )
list(APPEND _IMPORT_CHECK_FILES_FOR_expat::libexpat "${_IMPORT_PREFIX}/lib/libexpat.lib" "${_IMPORT_PREFIX}/bin/libexpat.dll" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
