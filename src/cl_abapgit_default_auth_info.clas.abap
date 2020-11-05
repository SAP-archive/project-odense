CLASS cl_abapgit_default_auth_info DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    "! Method to store user and password as
    "! authentication information.
    "!
    "! @parameter iv_user      | user
    "! @parameter iv_password  | password
    "!
    CLASS-METHODS set_auth_info
      IMPORTING
        !iv_user     TYPE string
        !iv_password TYPE string .

    "! Method to retrieve the stored password.
    "!
    "! @parameter rv_password       | Returns the stored password
    "!
    CLASS-METHODS get_password
      RETURNING
        VALUE(rv_password) TYPE string .

    "! Method to retrieve the stored user.
    "!
    "! @parameter rv_user       | Returns the strored user
    "!
    CLASS-METHODS get_user
      RETURNING
        VALUE(rv_user) TYPE string .

    "! Method to refresh to stored information.
    "!
    CLASS-METHODS refresh .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA gv_password TYPE string .
    CLASS-DATA gv_user TYPE string .
ENDCLASS.



CLASS cl_abapgit_default_auth_info IMPLEMENTATION.


  METHOD get_password.
    rv_password = gv_password.
  ENDMETHOD.


  METHOD get_user.
    rv_user = gv_user.
  ENDMETHOD.

  METHOD refresh.
    CLEAR: gv_user, gv_password.
  ENDMETHOD.


  METHOD set_auth_info.
    gv_user = iv_user.
    gv_password = iv_password.
  ENDMETHOD.
ENDCLASS.
