INTERFACE if_abapgit_repository_checks
  PUBLIC .

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Performs connection checks on the given repository</p>
    "!
    "! @parameter io_repository | <p class="shorttext synchronized" lang="en">Repository to be checked</p>
    "! @parameter iv_service | <p class="shorttext synchronized" lang="en">Service key for connection</p>
    "! @raising cx_abapgit_exception | <p class="shorttext synchronized" lang="en">Exception will be raised
    "! if any connection issues</p>
    check_connection
      IMPORTING
        io_repository TYPE REF TO cl_abapgit_repo_online
        iv_service    TYPE string
      RAISING
        cx_abapgit_exception.

ENDINTERFACE.
