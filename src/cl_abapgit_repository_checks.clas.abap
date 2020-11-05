CLASS cl_abapgit_repository_checks DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_abapgit_repository_checks.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS cl_abapgit_repository_checks IMPLEMENTATION.

  METHOD if_abapgit_repository_checks~check_connection.
    " Check connection with git server
    cl_abapgit_http=>create_by_url( iv_url = io_repository->get_url( )
                                    iv_service = iv_service ).

  ENDMETHOD.

ENDCLASS.
