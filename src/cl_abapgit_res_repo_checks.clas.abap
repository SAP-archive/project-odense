"! <p class="shorttext synchronized" lang="en">Resource Controller for Repository Checks</p>
CLASS cl_abapgit_res_repo_checks DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS post REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mo_repo_auth_srv      TYPE REF TO if_abapgit_repository_auth,
          mo_repo_service       TYPE REF TO if_abapgit_repo_srv,
          mo_repo_check_service TYPE REF TO if_abapgit_repository_checks.

    "! <p>Checks whether the abapgit exception which is raised is because of any
    "! authentication related issues on the git server</p>
    METHODS get_repository_check_service
      RETURNING VALUE(ro_repo_check_service) TYPE REF TO if_abapgit_repository_checks.

    METHODS get_repository_service
      RETURNING VALUE(ro_repo_service) TYPE REF TO if_abapgit_repo_srv.

    METHODS get_repo_auth_service
      RETURNING VALUE(ro_repo_auth_service) TYPE REF TO if_abapgit_repository_auth.

ENDCLASS.



CLASS cl_abapgit_res_repo_checks IMPLEMENTATION.


  METHOD post.

    DATA:
      lv_repo_key     TYPE if_abapgit_persistence=>ty_value,
      lv_username     TYPE string,
      lv_service      TYPE string,
      lo_repo_online  TYPE REF TO cl_abapgit_repo_online.

    " Get repository key
    request->get_uri_attribute( EXPORTING name = 'key' mandatory = abap_true
                                IMPORTING value = lv_repo_key ).

    " Get credentials from request header
    lv_username = request->get_inner_rest_request( )->get_header_field( iv_name = 'Username' ).

    " Client encodes password with base64 algorithm
    DATA(lv_password) = cl_abapgit_res_util=>encode_password(
          request->get_inner_rest_request( )->get_header_field( iv_name = 'Password' ) ).

    TRY.

        IF mo_repo_service IS INITIAL.
          mo_repo_service = get_repository_service( ).
        ENDIF.

        IF mo_repo_check_service IS INITIAL.
          mo_repo_check_service = get_repository_check_service( ).
        ENDIF.

        " Determine repository from repository key
        DATA(lo_repo) = mo_repo_service->get( lv_repo_key ).
        lo_repo_online ?= lo_repo.

        lv_service = 'upload'.

        " SET credentials in CASE they are supplied
        IF lv_username IS NOT INITIAL AND lv_password IS NOT INITIAL.
          cl_abapgit_default_auth_info=>set_auth_info( iv_user     = lv_username
                                                       iv_password = lv_password ).
          lv_service = 'receive'.
        ENDIF.

        " Check connection with git server
        mo_repo_check_service->check_connection( io_repository = lo_repo_online iv_service = lv_service ).

      CATCH cx_abapgit_exception INTO DATA(lx_abapgit_exception).
        DATA lv_http_status TYPE i.

        " Check whether the exception occurred because of any authentication issues
        mo_repo_auth_srv = get_repo_auth_service( ).
        IF mo_repo_auth_srv->is_authorization_issue( EXPORTING
                                                        ix_abapgit_exception = lx_abapgit_exception
                                                     IMPORTING
                                                        ev_http_status = lv_http_status ).
          mo_repo_auth_srv->handle_auth_exception( iv_http_status = lv_http_status
                                                   ix_abapgit_exception = lx_abapgit_exception ).
        ELSE.
          cx_adt_rest_abapgit=>raise_with_error( ix_error = lx_abapgit_exception
                                                 iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
        ENDIF.

        response->set_status( cl_rest_status_code=>gc_success_ok ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_repository_service.
    ro_repo_service = cl_abapgit_repo_srv=>get_instance( ).
  ENDMETHOD.


  METHOD get_repository_check_service.
    ro_repo_check_service = NEW cl_abapgit_repository_checks( ).
  ENDMETHOD.

  METHOD get_repo_auth_service.
    IF mo_repo_auth_srv IS INITIAL.
      ro_repo_auth_service = NEW cl_abapgit_repository_auth( ).
    ELSE.
      ro_repo_auth_service = mo_repo_auth_srv.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
