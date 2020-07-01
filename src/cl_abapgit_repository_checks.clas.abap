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

    " Check whether two factor authentication is enabled
    DATA lo_authenticator TYPE REF TO if_abapgit_repository_auth.
    DATA(lv_username) = cl_abapgit_default_auth_info=>get_user( ).
    DATA(lv_password) = cl_abapgit_default_auth_info=>get_password( ).

    IF lv_username IS NOT INITIAL AND lv_password IS NOT INITIAL.
      lo_authenticator = NEW cl_abapgit_repository_auth( ).

      " Check if 2fa is enabled for this account without triggering sms
      IF lo_authenticator->is_2fa_required( iv_url         = io_repository->get_url( )
                                            iv_username    = lv_username
                                            iv_password    = lv_password
                                            iv_trigger_sms = abap_false ) = abap_true.
        cx_abapgit_exception=>raise( '2FA required' ).
      ENDIF.
    ENDIF.

    " Check connection with git server
    cl_abapgit_http=>create_by_url( iv_url = io_repository->get_url( ) iv_service = iv_service ).

  ENDMETHOD.

ENDCLASS.
