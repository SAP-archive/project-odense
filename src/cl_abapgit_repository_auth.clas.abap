CLASS cl_abapgit_repository_auth DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_abapgit_repository_auth.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_exception_from_http_status
      IMPORTING iv_http_status TYPE string
      RAISING   cx_adt_abapgit_auth.

ENDCLASS.



CLASS cl_abapgit_repository_auth IMPLEMENTATION.
  METHOD if_abapgit_repository_auth~determine_access_level.
    rv_repo_access = cl_abapgit_http=>determine_access_level( iv_url = iv_url ).
  ENDMETHOD.

  METHOD if_abapgit_repository_auth~is_authorization_issue.
    " Handle the "authorization" related exceptions from git connection checks manually,
    " since the http response code is not part of the exception type cx_abapgit_exception.

    " Authentication issues will result in error codes 401, 403 and 404.

    " As per the github documentation,
    " There are two ways to authenticate through GitHub API v3.
    " Requests that require authentication will return  404 Not Found, instead of  403 Forbidden,
    " in some places. This is to prevent the accidental leakage of private repositories to unauthorized users".
    DATA(lv_exc_text) = ix_abapgit_exception->get_text( ).
    IF lv_exc_text     = 'HTTP 401, unauthorized'.
      ev_http_status = cl_rest_status_code=>gc_client_error_unauthorized.
    ELSEIF lv_exc_text = 'HTTP 403, forbidden'.
      ev_http_status = cl_rest_status_code=>gc_client_error_forbidden.
    ELSEIF lv_exc_text = 'HTTP 404, not found'.
      ev_http_status = cl_rest_status_code=>gc_client_error_not_found.
    ELSEIF lv_exc_text = '2FA required'.
      ev_http_status = cl_rest_status_code=>gc_client_error_unauthorized.
    ENDIF.

    IF ev_http_status IS NOT INITIAL.
      rv_is_auth_issue = abap_true.
    ELSE.
      rv_is_auth_issue = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD if_abapgit_repository_auth~handle_auth_exception.

    " Convert lv_http_status to a string and then remove the trailing whitespaces
    " This step is added as directly sending the integer http status as part of exception properties will
    " will result in a trailing space at the end in the response
    DATA(lv_http_status_string) = CONV string( iv_http_status ).
    CONDENSE lv_http_status_string NO-GAPS.

    " Raise internal server error, as the connection to github failed from abap server
    " Return the error code from the abapgit exception as part of additional adt exception properties.
    DATA(lv_properties) = cx_adt_rest_abapgit=>create_properties( )->add_property( key = 'http_status'
                                                                                   value = lv_http_status_string ).
    DATA lv_auth_exception TYPE REF TO cx_adt_abapgit_auth.

    TRY.
        "cl_abapgit_repository_auth=>get_exception_from_http_status( ev_http_status = lv_http_status_string ).
        get_exception_from_http_status( iv_http_status = lv_http_status_string ).
      CATCH cx_adt_abapgit_auth INTO lv_auth_exception.
        DATA(lv_long_text) = cx_adt_rest=>get_longtext_from_exception( lv_auth_exception ).
        lv_properties->add_property( key = 'LONGTEXT' value = lv_long_text ).
        cx_adt_rest_abapgit=>raise_with_error( ix_error       = lv_auth_exception
                                               iv_http_status = cl_rest_status_code=>gc_server_error_internal
                                               iv_properties  = lv_properties ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_exception_from_http_status.
    CASE iv_http_status.
      WHEN '401'.
        RAISE EXCEPTION TYPE cx_adt_abapgit_auth
          EXPORTING
            textid = cx_adt_abapgit_auth=>http_401.

      WHEN '403'.
        RAISE EXCEPTION TYPE cx_adt_abapgit_auth
          EXPORTING
            textid = cx_adt_abapgit_auth=>http_403.

      WHEN '404'.
        RAISE EXCEPTION TYPE cx_adt_abapgit_auth
          EXPORTING
            textid = cx_adt_abapgit_auth=>http_404.

    ENDCASE.
  ENDMETHOD.

  METHOD if_abapgit_repository_auth~is_2fa_required.
    DATA: lo_authenticator TYPE REF TO if_abapgit_2fa_authenticator,
          lx_ex            TYPE REF TO cx_root.

    lo_authenticator = NEW cl_abapgit_2fa_github_auth( ).

    " Check if 2fa enabled for this account without sending otp via sms
    TRY.
        rv_required = lo_authenticator->is_2fa_required( iv_url         = iv_url
                                                         iv_username    = iv_username
                                                         iv_password    = iv_password
                                                         iv_trigger_sms = iv_trigger_sms ).
      CATCH cx_abapgit_2fa_comm_error INTO lx_ex.
        cx_abapgit_exception=>raise( |2FA error: { lx_ex->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
